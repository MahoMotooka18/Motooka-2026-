# 処理内容:
# - 先発投手向けの予測モデルで共通利用する前処理・評価・出力関数を定義します。
# - NPB/MLBの加工済み投手データを読み込み、相互予測、評価指標、変数重要度CSVを作成します。

# 必要なRパッケージを一覧化し、未導入なら処理開始前に止めます。
required_packages <- c("dplyr", "readr", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install required packages first: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))",
    call. = FALSE
  )
}

# パッケージ読み込み時の起動メッセージを抑え、ログを読みやすくします。
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

# 乱数を固定し、データ分割やモデル学習の再現性を確保します。
set.seed(111)

# 実行場所からプロジェクトルートを検出し、以降の相対パス解決に使います。
detect_project_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  candidates <- c(
    if (length(file_arg) > 0) {
      file.path(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE)), "..", "..")
    },
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "..", "..")
  )

  for (candidate in candidates) {
    candidate <- normalizePath(candidate, mustWork = FALSE)
    if (dir.exists(file.path(candidate, "data"))) {
      return(candidate)
    }
  }

  normalizePath(getwd(), mustWork = FALSE)
}

# 共通ヘルパー内で使うプロジェクトルートを保持します。
PROJECT_ROOT <- detect_project_root()
# この予測セットの出力先ルートを定義します。
PREDICTIONS_ROOT <- file.path(PROJECT_ROOT, "predictions", "starting_pitchers")

# 予測対象となる目的変数を定義します。
DEPENDENT_VARIABLE <- "salary"

# モデルに投入する説明変数を、9イニング換算指標中心に定義します。
EXPLANATORY_VARIABLES_PER9 <- c(
  "age",
  "lefty",
  "games",
  "games_started",
  "games_finished",
  "complete_games",
  "shutouts",
  "saves",
  "holds",
  "innings_pitched",
  "batters_faced",
  "wins",
  "losses",
  "era",
  "hits_per_9",
  "home_runs_per_9",
  "strikeouts_per_9",
  "walks_per_9",
  "earned_runs",
  "intentional_walks",
  "hit_by_pitch",
  "wild_pitches",
  "balks"
)

# MLB/NPBそれぞれの加工済み投手データへの入力パスを定義します。
DATA_PATHS <- list(
  MLB = file.path(PROJECT_ROOT, "data", "processed", "mlb", "mlb_starting_pitchers.csv"),
  NPB = file.path(PROJECT_ROOT, "data", "processed", "npb", "npb_starting_pitchers.csv")
)

# 必要なファイルや設定を確認し、処理前提を整えます。
ensure_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      "Install the ",
      package,
      " package first: install.packages(\"",
      package,
      "\")",
      call. = FALSE
    )
  }
}

# カンマや空文字を含む値を、分析に使える数値へ変換します。
parse_numeric_clean <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA_character_
  suppressWarnings(readr::parse_number(x_chr, locale = readr::locale(grouping_mark = ",")))
}

# リーグ別の年俸表記を数値化し、NPBの万円表記は円単位へ変換します。
parse_salary <- function(x, league) {
  x_chr <- trimws(as.character(x))
  salary <- parse_numeric_clean(x_chr)

  if (identical(league, "NPB") && any(grepl("万円", x_chr), na.rm = TRUE)) {
    salary <- salary * 10000
  }

  salary
}

# 野球式の投球回表記を小数の投球回へ変換します。
convert_baseball_innings <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA_character_
  numeric_value <- suppressWarnings(as.numeric(x_chr))

  converted <- vapply(
    x_chr,
    function(value) {
      if (is.na(value)) {
        return(NA_real_)
      }

      parts <- regmatches(value, regexec("^(-?[0-9]+)(?:\\.([0-9]+))?$", value))[[1]]
      if (length(parts) == 0) {
        return(suppressWarnings(as.numeric(value)))
      }

      whole <- as.numeric(parts[[2]])
      fraction <- if (length(parts) >= 3) parts[[3]] else ""

      if (fraction %in% c("", "0")) {
        whole
      } else if (fraction == "1") {
        whole + 1 / 3
      } else if (fraction == "2") {
        whole + 2 / 3
      } else {
        suppressWarnings(as.numeric(value))
      }
    },
    numeric(1)
  )

  ifelse(is.na(converted), numeric_value, converted)
}

# 左右やTRUE/FALSE表記を0/1のダミー変数へ変換します。
parse_dummy <- function(x) {
  x_chr <- trimws(as.character(x))
  dplyr::case_when(
    is.na(x) | x_chr == "" ~ NA_real_,
    x_chr %in% c("1", "TRUE", "True", "true", "左", "両") ~ 1,
    x_chr %in% c("0", "FALSE", "False", "false", "右") ~ 0,
    TRUE ~ suppressWarnings(as.numeric(x_chr))
  )
}

# モデルに必要な列が入力データにそろっているか確認します。
validate_columns <- function(data, league) {
  required_columns <- c("player", "year", DEPENDENT_VARIABLE, EXPLANATORY_VARIABLES_PER9)
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      league,
      " data is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
}

# 指定リーグの加工済み投手データを読み込み、モデル用の列と型に整えます。
read_league_data <- function(league) {
  league <- toupper(league)

  if (!league %in% names(DATA_PATHS)) {
    stop("league must be one of: ", paste(names(DATA_PATHS), collapse = ", "), call. = FALSE)
  }

  validate_columns(readr::read_csv(DATA_PATHS[[league]], show_col_types = FALSE, n_max = 1), league)

  raw_data <- readr::read_csv(DATA_PATHS[[league]], show_col_types = FALSE, guess_max = 100000)

  data <- raw_data %>%
    transmute(
      league = league,
      player_name = as.character(.data[["player"]]),
      year = parse_numeric_clean(.data[["year"]]),
      salary = parse_salary(.data[[DEPENDENT_VARIABLE]], league),
      age = parse_numeric_clean(.data[["age"]]),
      lefty = parse_dummy(.data[["lefty"]]),
      games = parse_numeric_clean(.data[["games"]]),
      games_started = parse_numeric_clean(.data[["games_started"]]),
      games_finished = parse_numeric_clean(.data[["games_finished"]]),
      complete_games = parse_numeric_clean(.data[["complete_games"]]),
      shutouts = parse_numeric_clean(.data[["shutouts"]]),
      saves = parse_numeric_clean(.data[["saves"]]),
      holds = parse_numeric_clean(.data[["holds"]]),
      innings_pitched = convert_baseball_innings(.data[["innings_pitched"]]),
      batters_faced = parse_numeric_clean(.data[["batters_faced"]]),
      wins = parse_numeric_clean(.data[["wins"]]),
      losses = parse_numeric_clean(.data[["losses"]]),
      era = parse_numeric_clean(.data[["era"]]),
      hits_per_9 = parse_numeric_clean(.data[["hits_per_9"]]),
      home_runs_per_9 = parse_numeric_clean(.data[["home_runs_per_9"]]),
      strikeouts_per_9 = parse_numeric_clean(.data[["strikeouts_per_9"]]),
      walks_per_9 = parse_numeric_clean(.data[["walks_per_9"]]),
      earned_runs = parse_numeric_clean(.data[["earned_runs"]]),
      intentional_walks = parse_numeric_clean(.data[["intentional_walks"]]),
      hit_by_pitch = parse_numeric_clean(.data[["hit_by_pitch"]]),
      wild_pitches = parse_numeric_clean(.data[["wild_pitches"]]),
      balks = parse_numeric_clean(.data[["balks"]])
    )

  model_columns <- c(DEPENDENT_VARIABLE, EXPLANATORY_VARIABLES_PER9)
  complete_rows <- stats::complete.cases(data[, model_columns, drop = FALSE])
  dropped_rows <- sum(!complete_rows)

  if (dropped_rows > 0) {
    message(league, ": dropped ", dropped_rows, " rows with missing model variables.")
  }

  data[complete_rows, , drop = FALSE]
}

# 目的変数と説明変数からモデル式を作成します。
model_formula <- function() {
  stats::as.formula(
    paste(DEPENDENT_VARIABLE, "~", paste(EXPLANATORY_VARIABLES_PER9, collapse = " + "))
  )
}

# 目的変数と説明変数だけを含む学習用データを取り出します。
model_frame <- function(data) {
  data[, c(DEPENDENT_VARIABLE, EXPLANATORY_VARIABLES_PER9), drop = FALSE]
}

# 説明変数だけを含む予測用データを取り出します。
predictor_frame <- function(data) {
  data[, EXPLANATORY_VARIABLES_PER9, drop = FALSE]
}

# 説明変数データを行列形式に変換し、機械学習パッケージへ渡します。
model_matrix <- function(data) {
  as.matrix(predictor_frame(data))
}

# 学習データと予測データの説明変数列が一致しているか確認します。
assert_matching_predictors <- function(train_data, predict_data, trained_on, predicted_on) {
  train_cols <- names(predictor_frame(train_data))
  predict_cols <- names(predictor_frame(predict_data))

  if (!identical(train_cols, predict_cols)) {
    stop(
      "Predictor columns differ between ",
      trained_on,
      " and ",
      predicted_on,
      ".",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# 実測値と予測値からMSE、RMSE、MAE、R二乗などを計算します。
calc_metrics <- function(actual, predicted) {
  error <- actual - predicted
  sse <- sum(error^2)
  sst <- sum((actual - mean(actual))^2)

  tibble(
    mse = mean(error^2),
    rmse = sqrt(mean(error^2)),
    mae = mean(abs(error)),
    r_squared = if (sst == 0) NA_real_ else 1 - sse / sst,
    n_obs = length(actual)
  )
}

# 選手年度ごとの実測年俸・予測年俸・誤差を出力用行に整えます。
prediction_rows <- function(data, predicted, trained_on, predicted_on, model_name) {
  actual <- data[[DEPENDENT_VARIABLE]]

  tibble(
    league = predicted_on,
    player_name = data$player_name,
    year = data$year,
    actual_salary = actual,
    predicted_salary = as.numeric(predicted),
    squared_error = (actual - as.numeric(predicted))^2,
    trained_on = trained_on,
    predicted_on = predicted_on,
    model_name = model_name
  )
}

# 予測評価指標にモデル名と学習/予測リーグ情報を付けます。
metric_rows <- function(data, predicted, trained_on, predicted_on, model_name) {
  calc_metrics(data[[DEPENDENT_VARIABLE]], as.numeric(predicted)) %>%
    mutate(
      model_name = model_name,
      trained_on = trained_on,
      predicted_on = predicted_on,
      .before = 1
    )
}

# 変数重要度を最大絶対値で割り、モデル間で比較しやすい尺度にします。
standardize_importance <- function(x) {
  scale_value <- max(abs(x), na.rm = TRUE)

  if (!is.finite(scale_value) || scale_value == 0) {
    return(rep(0, length(x)))
  }

  x / scale_value
}

# 各説明変数をシャッフルし、MSE悪化量から置換重要度を計算します。
permutation_importance <- function(
  model,
  data,
  predict_function,
  baseline_mse,
  model_name,
  trained_on,
  predicted_on
) {
  rows <- lapply(
    EXPLANATORY_VARIABLES_PER9,
    function(variable) {
      permuted_data <- data
      permuted_data[[variable]] <- sample(permuted_data[[variable]])
      permuted_pred <- as.numeric(predict_function(model, permuted_data))
      permuted_mse <- mean((permuted_data[[DEPENDENT_VARIABLE]] - permuted_pred)^2)

      tibble(
        model_name = model_name,
        trained_on = trained_on,
        predicted_on = predicted_on,
        variable = variable,
        permutation_importance = permuted_mse - baseline_mse
      )
    }
  )

  bind_rows(rows) %>%
    mutate(
      permutation_importance_standardized = standardize_importance(.data[["permutation_importance"]])
    )
}

# モデル固有の重要度がない場合に、空の重要度表を返します。
empty_model_specific_importance <- function(model, train_data) {
  tibble(variable = EXPLANATORY_VARIABLES_PER9)
}

# 回帰係数を説明変数と目的変数の標準偏差で標準化します。
standardized_coefficients <- function(coefficients, train_data) {
  y_sd <- stats::sd(train_data[[DEPENDENT_VARIABLE]])

  vapply(
    EXPLANATORY_VARIABLES_PER9,
    function(variable) {
      coefficient <- coefficients[[variable]]
      x_sd <- stats::sd(train_data[[variable]])

      if (is.na(coefficient) || is.na(x_sd) || is.na(y_sd) || y_sd == 0) {
        return(NA_real_)
      }

      coefficient * x_sd / y_sd
    },
    numeric(1)
  )
}

# 名前付き重要度ベクトルを全説明変数そろいのtibbleへ変換します。
normalize_named_importance <- function(x, output_name = "model_specific_importance") {
  if (length(x) == 0) {
    return(tibble(variable = EXPLANATORY_VARIABLES_PER9, "{output_name}" := NA_real_))
  }

  tibble(
    variable = names(x),
    "{output_name}" := as.numeric(x)
  ) %>%
    right_join(tibble(variable = EXPLANATORY_VARIABLES_PER9), by = "variable") %>%
    mutate("{output_name}" := tidyr_free_na_to_zero(.data[[output_name]]))
}

# 欠損した重要度を0へ置き換え、tidyrに依存せず補完します。
tidyr_free_na_to_zero <- function(x) {
  x[is.na(x)] <- 0
  x
}

# 複数モデルの予測値を重み付き平均にまとめます。
weighted_mean_matrix <- function(prediction_matrix, weights) {
  as.numeric(prediction_matrix %*% weights / sum(weights))
}

# 回帰木を弱学習器にしたAdaBoost.R2モデルを学習します。
fit_adaboost_r2 <- function(train_data, n_iter = 50) {
  ensure_package("rpart")

  train_model_frame <- model_frame(train_data)
  n <- nrow(train_model_frame)
  observation_weights <- rep(1 / n, n)
  models <- list()
  alphas <- numeric(0)

  for (i in seq_len(n_iter)) {
    weighted_formula <- model_formula()
    environment(weighted_formula) <- environment()

    fitted_tree <- rpart::rpart(
      formula = weighted_formula,
      data = train_model_frame,
      weights = observation_weights
    )

    fitted_values <- as.numeric(stats::predict(fitted_tree, newdata = train_model_frame))
    absolute_error <- abs(train_model_frame[[DEPENDENT_VARIABLE]] - fitted_values)
    max_error <- max(absolute_error)

    if (!is.finite(max_error) || max_error == 0) {
      models[[length(models) + 1]] <- fitted_tree
      alphas <- c(alphas, 1)
      break
    }

    normalized_error <- absolute_error / max_error
    weighted_error <- sum(observation_weights * normalized_error)

    if (!is.finite(weighted_error) || weighted_error <= 0) {
      beta <- 1e-6
    } else if (weighted_error >= 0.5) {
      if (length(models) == 0) {
        models[[1]] <- fitted_tree
        alphas <- 1
      }
      break
    } else {
      beta <- weighted_error / (1 - weighted_error)
    }

    models[[length(models) + 1]] <- fitted_tree
    alphas <- c(alphas, log(1 / beta))

    observation_weights <- observation_weights * beta^(1 - normalized_error)
    observation_weights <- observation_weights / sum(observation_weights)
  }

  structure(
    list(models = models, alphas = alphas, variables = EXPLANATORY_VARIABLES_PER9),
    class = "adaboost_r2_model"
  )
}

# AdaBoost.R2の複数弱学習器予測を重み付き平均して返します。
predict_adaboost_r2 <- function(model, new_data) {
  prediction_matrix <- vapply(
    model$models,
    function(fitted_tree) {
      as.numeric(stats::predict(fitted_tree, newdata = predictor_frame(new_data)))
    },
    numeric(nrow(new_data))
  )

  if (is.null(dim(prediction_matrix))) {
    return(as.numeric(prediction_matrix))
  }

  weighted_mean_matrix(prediction_matrix, model$alphas)
}

# AdaBoost.R2内の回帰木重要度を重み付きで集計します。
adaboost_r2_importance <- function(model, train_data) {
  rows <- lapply(
    seq_along(model$models),
    function(i) {
      importance <- model$models[[i]]$variable.importance
      if (is.null(importance)) {
        return(tibble(variable = EXPLANATORY_VARIABLES_PER9, adaboost_rpart_importance = 0))
      }

      normalize_named_importance(importance, "adaboost_rpart_importance") %>%
        mutate(adaboost_rpart_importance = .data[["adaboost_rpart_importance"]] * model$alphas[[i]])
    }
  )

  bind_rows(rows) %>%
    group_by(.data[["variable"]]) %>%
    summarise(adaboost_rpart_importance = sum(.data[["adaboost_rpart_importance"]]), .groups = "drop") %>%
    mutate(adaboost_rpart_importance_standardized = standardize_importance(.data[["adaboost_rpart_importance"]]))
}

# NPB/MLBデータで学習・相互予測・評価指標・重要度CSV出力までを実行します。
run_model_pipeline <- function(
  model_name,
  fit_function,
  predict_function,
  model_specific_importance_function = empty_model_specific_importance
) {
  model_dir <- file.path(PREDICTIONS_ROOT, model_name)
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

  datasets <- list(
    NPB = read_league_data("NPB"),
    MLB = read_league_data("MLB")
  )

  assert_matching_predictors(datasets$NPB, datasets$MLB, "NPB", "MLB")
  assert_matching_predictors(datasets$MLB, datasets$NPB, "MLB", "NPB")

  fitted_models <- lapply(
    names(datasets),
    function(league) {
      message("Training ", model_name, " on ", league, " data.")
      fit_function(datasets[[league]])
    }
  )
  names(fitted_models) <- names(datasets)

  prediction_results <- list()
  model_summary <- list()
  variable_importance <- list()

  for (trained_on in names(fitted_models)) {
    trained_model <- fitted_models[[trained_on]]
    specific_importance <- model_specific_importance_function(trained_model, datasets[[trained_on]])

    for (predicted_on in names(datasets)) {
      assert_matching_predictors(datasets[[trained_on]], datasets[[predicted_on]], trained_on, predicted_on)

      predictions <- as.numeric(predict_function(trained_model, datasets[[predicted_on]]))
      metrics <- metric_rows(datasets[[predicted_on]], predictions, trained_on, predicted_on, model_name)

      prediction_results[[length(prediction_results) + 1]] <- prediction_rows(
        datasets[[predicted_on]],
        predictions,
        trained_on,
        predicted_on,
        model_name
      )

      model_summary[[length(model_summary) + 1]] <- metrics

      permutation_rows <- permutation_importance(
        trained_model,
        datasets[[predicted_on]],
        predict_function,
        metrics$mse[[1]],
        model_name,
        trained_on,
        predicted_on
      )

      variable_importance[[length(variable_importance) + 1]] <- permutation_rows %>%
        left_join(specific_importance, by = "variable")
    }
  }

  prediction_results <- bind_rows(prediction_results)
  model_summary <- bind_rows(model_summary)
  variable_importance <- bind_rows(variable_importance)

  readr::write_csv(
    prediction_results,
    file.path(model_dir, paste0(model_name, "_prediction_results.csv")),
    na = ""
  )
  readr::write_csv(
    model_summary,
    file.path(model_dir, paste0(model_name, "_model_summary.csv")),
    na = ""
  )
  readr::write_csv(
    variable_importance,
    file.path(model_dir, paste0(model_name, "_variable_importance.csv")),
    na = ""
  )

  write_global_comparison_tables()

  message("Wrote outputs for ", model_name, " to ", model_dir)

  invisible(list(
    prediction_results = prediction_results,
    model_summary = model_summary,
    variable_importance = variable_importance
  ))
}

# 各モデルの出力CSVを集約し、全体比較表と重要度上位表を更新します。
write_global_comparison_tables <- function() {
  summary_files <- list.files(
    PREDICTIONS_ROOT,
    pattern = "_model_summary\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  importance_files <- list.files(
    PREDICTIONS_ROOT,
    pattern = "_variable_importance\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(summary_files) > 0) {
    model_comparison <- bind_rows(lapply(summary_files, readr::read_csv, show_col_types = FALSE)) %>%
      select(
        "model_name",
        "trained_on",
        "predicted_on",
        "mse",
        "rmse",
        "mae",
        "r_squared",
        dplyr::everything()
      )

    readr::write_csv(model_comparison, file.path(PREDICTIONS_ROOT, "model_comparison_table.csv"), na = "")
  }

  if (length(importance_files) > 0) {
    variable_importance <- bind_rows(lapply(importance_files, readr::read_csv, show_col_types = FALSE))

    top_rows <- variable_importance %>%
      group_by(.data[["model_name"]], .data[["trained_on"]], .data[["predicted_on"]]) %>%
      group_split() %>%
      lapply(function(group_data) {
        group_data <- group_data %>%
          arrange(desc(.data[["permutation_importance_standardized"]])) %>%
          slice_head(n = 10)

        out <- tibble(
          model_name = group_data$model_name[[1]],
          trained_on = group_data$trained_on[[1]],
          predicted_on = group_data$predicted_on[[1]]
        )

        for (i in seq_len(10)) {
          variable_name <- paste0("top", i, "_variable")
          importance_name <- paste0("top", i, "_importance")

          out[[variable_name]] <- if (nrow(group_data) >= i) group_data$variable[[i]] else NA_character_
          out[[importance_name]] <- if (nrow(group_data) >= i) {
            group_data$permutation_importance_standardized[[i]]
          } else {
            NA_real_
          }
        }

        out
      }) %>%
      bind_rows()

    readr::write_csv(top_rows, file.path(PREDICTIONS_ROOT, "variable_importance_table.csv"), na = "")
  }

  invisible(TRUE)
}
