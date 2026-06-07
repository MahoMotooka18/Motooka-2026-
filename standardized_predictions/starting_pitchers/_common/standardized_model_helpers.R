# 処理内容:
# - 標準化済み salary データを使って、MLB/NPBそれぞれのモデルを作成します。
# - 各リーグを train/test に分割し、同一リーグは test、クロスリーグは相手リーグ full を評価対象にします。
# - 各モデル共通の前処理、評価指標、Permutation Importance、CSV出力、全体比較表作成を定義します。

# 共通処理で使う最小限のパッケージを確認します。
# モデル固有パッケージは各モデルの *_run.R 側で ensure_package() により個別確認します。
required_packages <- c("dplyr", "readr", "tibble", "rsample")
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

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(rsample)
})

# rsample::initial_split()、Permutation Importance の列シャッフル、
# 一部モデルの乱数利用を再現可能にするため、共通で乱数シードを固定します。
set.seed(111)

# 実行中のRスクリプト位置または作業ディレクトリからプロジェクトルートを推定します。
# all/_common だけでなく starting_pitchers/_common、relief/_common でも同じコードを使えるよう、
# 複数の親ディレクトリ候補を順に確認します。
detect_project_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  candidates <- c(
    if (length(file_arg) > 0) {
      file.path(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE)), "..", "..", "..")
    },
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

PROJECT_ROOT <- detect_project_root()

# 出力先となる予測セットのルートディレクトリを決めます。
# 通常は *_run.R の親ディレクトリ、つまり standardized_predictions/{all|starting_pitchers|relief} です。
# 環境変数 STANDARDIZED_PREDICTIONS_ROOT を使うと、外部から出力先を明示的に差し替えられます。
detect_predictions_root <- function() {
  env_root <- Sys.getenv("STANDARDIZED_PREDICTIONS_ROOT", unset = "")

  if (nzchar(env_root)) {
    return(normalizePath(env_root, mustWork = FALSE))
  }

  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE)
    return(normalizePath(file.path(dirname(script_path), ".."), mustWork = FALSE))
  }

  normalizePath(file.path(PROJECT_ROOT, "standardized_predictions", "all"), mustWork = FALSE)
}

PREDICTIONS_ROOT <- detect_predictions_root()
PREDICTION_SET <- basename(PREDICTIONS_ROOT)

# 被説明変数は、事前にリーグ内で標準化済みの salary です。
DEPENDENT_VARIABLE <- "salary"

# モデルに投入する説明変数です。
# predictions 配下の per9 系スクリプトと同じ説明変数セットを使い、
# 全投手・先発・救援の3系統で比較できるよう固定しています。
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
  "intentional_walks",
  "hit_by_pitch",
  "wild_pitches",
  "balks"
)

# 実行・表示・集計で使うモデル順です。
# ridge / elastic_net は今回の標準化版では除外しています。
MODEL_ORDER <- c(
  "linear_regression",
  "lasso",
  "regression_tree",
  "svm_regression",
  "bagging",
  "random_forest",
  "adaboost",
  "gbm",
  "xgboost",
  "lightgbm"
)

# 評価対象4パターンの列順です。
# 同一リーグは train ではなく test のみ、クロスリーグは相手リーグ full を使います。
PREDICTION_PATTERN_ORDER <- c(
  "MLB_train_MLB_test",
  "MLB_train_NPB_full",
  "NPB_train_NPB_test",
  "NPB_train_MLB_full"
)

# 全体比較表に出す指標です。
# MSE/RMSE/MAE は後段で linear_regression 基準の相対値に変換し、
# R_squared は元値のまま比較します。
COMPARISON_METRICS <- c(
  "mse_relative_to_linear",
  "rmse_relative_to_linear",
  "mae_relative_to_linear",
  "r_squared"
)

# model_summary.csv を一意に識別するキー列です。
# 全モデルのサマリーを読み直す時に、重複行が増殖しないようこのキーで distinct() します。
SUMMARY_KEY_COLUMNS <- c(
  "model_name",
  "trained_on",
  "predicted_on",
  "prediction_data_type"
)

# 相対指標を再計算する前に保持する、素の評価指標列です。
BASE_SUMMARY_COLUMNS <- c(
  SUMMARY_KEY_COLUMNS,
  "n_obs",
  "mse",
  "rmse",
  "mae",
  "r_squared"
)

# 予測セットごとに使う標準化済みCSVを対応づけます。
# PREDICTION_SET が all の場合は全投手、starting_pitchers は先発、relief は救援のみを読み込みます。
STANDARDIZED_DATA_FILES <- list(
  all = c(
    MLB = "standardized_mlb_pitchers.csv",
    NPB = "standardized_npb_pitchers.csv"
  ),
  starting_pitchers = c(
    MLB = "standardized_mlb_starting_pitchers.csv",
    NPB = "standardized_npb_starting_pitchers.csv"
  ),
  relief = c(
    MLB = "standardized_mlb_relief_pitchers.csv",
    NPB = "standardized_npb_relief_pitchers.csv"
  )
)

# 予期しないディレクトリ名で共通ヘルパーが実行された場合は、入力CSVを誤って選ばないよう停止します。
if (!PREDICTION_SET %in% names(STANDARDIZED_DATA_FILES)) {
  stop(
    "Unknown standardized prediction set: ",
    PREDICTION_SET,
    ". Expected one of: ",
    paste(names(STANDARDIZED_DATA_FILES), collapse = ", "),
    call. = FALSE
  )
}

# 実際に読み込む MLB/NPB の標準化済みCSVパスを作成します。
# salary は各リーグ内で平均0・標準偏差1に標準化済みなので、ここでは単位変換を行いません。
DATA_PATHS <- list(
  MLB = file.path(
    PROJECT_ROOT,
    "data",
    "processed",
    "standardized_mlb",
    STANDARDIZED_DATA_FILES[[PREDICTION_SET]][["MLB"]]
  ),
  NPB = file.path(
    PROJECT_ROOT,
    "data",
    "processed",
    "standardized_npb",
    STANDARDIZED_DATA_FILES[[PREDICTION_SET]][["NPB"]]
  )
)

# モデル個別スクリプトから呼び出すパッケージ確認関数です。
# 未導入パッケージを使ったモデルだけが分かるよう、モデル実行前に明示的なエラーを出します。
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

# カンマ、空文字、文字列混在の数値列を readr::parse_number() で数値化します。
# salary は標準化済みでもCSV上は文字として読まれる可能性があるため、この関数を通します。
parse_numeric_clean <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA_character_
  suppressWarnings(readr::parse_number(x_chr, locale = readr::locale(grouping_mark = ",")))
}

# 野球式の投球回表記を実数に変換します。
# 例: 5.1 は 5 + 1/3、5.2 は 5 + 2/3 として扱います。
# 既に通常の小数で入っている値や変換不能な値は as.numeric() の結果を使います。
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

# 利き腕などのダミー表記を0/1に変換します。
# 日本語表記の「左」「両」は1、「右」は0として扱います。
parse_dummy <- function(x) {
  x_chr <- trimws(as.character(x))
  dplyr::case_when(
    is.na(x) | x_chr == "" ~ NA_real_,
    x_chr %in% c("1", "TRUE", "True", "true", "左", "両") ~ 1,
    x_chr %in% c("0", "FALSE", "False", "false", "右") ~ 0,
    TRUE ~ suppressWarnings(as.numeric(x_chr))
  )
}

# 入力CSVに、モデル作成に必要な列が揃っているか確認します。
# ここで止めることで、後段の transmute() やモデル推定で原因不明のエラーになるのを防ぎます。
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

# 指定リーグの標準化済みデータを読み込み、モデル投入用の列名・型へ整えます。
# 目的変数と説明変数に欠損がある行は、モデルごとの挙動差を避けるため共通で除外します。
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
      salary = parse_numeric_clean(.data[[DEPENDENT_VARIABLE]]),
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

# 各リーグのデータを train/test に分割します。
# rsample::initial_split() の prop は指定せず、デフォルトの0.75を使います。
# full はクロスリーグ予測で相手リーグ全体を評価するために保持します。
split_league_data <- function(data) {
  split <- rsample::initial_split(data)
  list(
    train = rsample::training(split),
    test = rsample::testing(split),
    full = data
  )
}

# Rのformulaインターフェースを使うモデル向けに salary ~ 説明変数 の式を作ります。
model_formula <- function() {
  stats::as.formula(
    paste(DEPENDENT_VARIABLE, "~", paste(EXPLANATORY_VARIABLES_PER9, collapse = " + "))
  )
}

# formula系モデルの学習用データです。
# salary と説明変数だけに絞ることで、player_name や year が誤ってモデルに入るのを防ぎます。
model_frame <- function(data) {
  data[, c(DEPENDENT_VARIABLE, EXPLANATORY_VARIABLES_PER9), drop = FALSE]
}

# 予測時に使う説明変数だけのデータフレームです。
predictor_frame <- function(data) {
  data[, EXPLANATORY_VARIABLES_PER9, drop = FALSE]
}

# glmnet、xgboost、lightgbm など行列入力を要求するモデル向けの説明変数行列です。
model_matrix <- function(data) {
  as.matrix(predictor_frame(data))
}

# 学習データと予測対象データの説明変数列が完全一致しているか確認します。
# クロスリーグ予測時の列欠落・列順差異を明示的に検出します。
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

# 予測値と実測値から評価指標を計算します。
# R_squared は 1 - SSE/SST として算出し、実測値の分散が0の場合はNAにします。
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

# 評価パターン名を、横持ち比較表の列名に変換します。
# 例: trained_on=MLB, predicted_on=NPB, full_cross_league -> MLB_train_NPB_full。
prediction_pattern <- function(trained_on, predicted_on, prediction_data_type) {
  paste0(
    trained_on,
    "_train_",
    predicted_on,
    "_",
    ifelse(prediction_data_type == "test", "test", "full")
  )
}

# 個票レベルの予測結果CSVに出す行を作ります。
# actual_salary / predicted_salary / squared_error に加え、学習リーグと予測対象リーグを記録します。
prediction_rows <- function(data, predicted, trained_on, predicted_on, prediction_data_type, model_name) {
  actual <- data[[DEPENDENT_VARIABLE]]

  tibble(
    model_name = model_name,
    trained_on = trained_on,
    predicted_on = predicted_on,
    prediction_data_type = prediction_data_type,
    league = predicted_on,
    player_name = data$player_name,
    year = data$year,
    actual_salary = actual,
    predicted_salary = as.numeric(predicted),
    squared_error = (actual - as.numeric(predicted))^2
  )
}

# model_summary.csv に出す評価指標行を作ります。
# linear_regression 基準の相対値は全モデル集約後に計算するため、ここでは一旦NAで置きます。
metric_rows <- function(data, predicted, trained_on, predicted_on, prediction_data_type, model_name) {
  calc_metrics(data[[DEPENDENT_VARIABLE]], as.numeric(predicted)) %>%
    mutate(
      model_name = model_name,
      trained_on = trained_on,
      predicted_on = predicted_on,
      prediction_data_type = prediction_data_type,
      mse_relative_to_linear = NA_real_,
      rmse_relative_to_linear = NA_real_,
      mae_relative_to_linear = NA_real_,
      .before = 1
    )
}

# 重要度を -1〜1 の範囲に標準化します。
# 最大絶対値で割るため、正負の方向は維持されます。
standardize_importance <- function(x) {
  scale_value <- max(abs(x), na.rm = TRUE)

  if (!is.finite(scale_value) || scale_value == 0) {
    return(rep(0, length(x)))
  }

  x / scale_value
}

# Permutation Importance の符号を読みやすい方向ラベルに変換します。
# シャッフルでMSEが悪化すれば worsened、改善すれば improved とします。
permutation_direction <- function(x) {
  dplyr::case_when(
    x > 0 ~ "worsened",
    x < 0 ~ "improved",
    TRUE ~ "no_change"
  )
}

# 共通指標としての Permutation Importance を計算します。
# 各説明変数を1つずつシャッフルし、baseline_mse からどれだけMSEが変化したかを記録します。
# 同一リーグ test / クロスリーグ full の各評価対象ごとに別々の baseline_mse を使います。
permutation_importance <- function(
  model,
  data,
  predict_function,
  baseline_mse,
  model_name,
  trained_on,
  predicted_on,
  prediction_data_type
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
        prediction_data_type = prediction_data_type,
        variable = variable,
        permutation_importance = permuted_mse - baseline_mse
      )
    }
  )

  bind_rows(rows) %>%
    mutate(
      permutation_importance_abs = abs(.data[["permutation_importance"]]),
      permutation_importance_standardized = standardize_importance(.data[["permutation_importance"]]),
      permutation_importance_standardized_abs = abs(.data[["permutation_importance_standardized"]]),
      permutation_importance_direction = permutation_direction(.data[["permutation_importance"]])
    )
}

# モデル固有の重要度を持たないモデル向けの空実装です。
# SVMなどはPermutation Importanceのみを出力できます。
empty_model_specific_importance <- function(model, train_data) {
  tibble(variable = EXPLANATORY_VARIABLES_PER9)
}

# 線形回帰・LASSOの係数を、標準化係数として比較できる形に変換します。
# coefficient * sd(x) / sd(y) により、変数単位の違いをある程度取り除きます。
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

# モデル固有重要度でNAになった値を0に置き換えます。
# 使われなかった変数も比較表に残すための補助関数です。
tidyr_free_na_to_zero <- function(x) {
  x[is.na(x)] <- 0
  x
}

# 名前付きベクトルの重要度を、全説明変数を含むtibbleに揃えます。
# rpart、ranger など、重要度を持つ変数だけ返すモデルで列を揃えるために使います。
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

# AdaBoost.R2 の弱学習器予測を、alpha重み付き平均で統合します。
weighted_mean_matrix <- function(prediction_matrix, weights) {
  as.numeric(prediction_matrix %*% weights / sum(weights))
}

# rpart回帰木を弱学習器にした簡易AdaBoost.R2を実装します。
# 既存 predictions 側と同じく、基本設定は複雑にチューニングせずデフォルト寄りにしています。
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

# fit_adaboost_r2() で作ったモデルから予測値を作ります。
# 弱学習器が1本だけの場合と複数本の場合の両方に対応します。
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

# AdaBoost内の各rpart弱学習器が持つ variable.importance を alpha で重み付けして合算します。
# これはモデル固有重要度であり、共通のPermutation Importanceとは別列として出力されます。
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

# 学習リーグごとに評価対象を定義します。
# MLBで学習したモデルは MLB test と NPB full、NPBで学習したモデルは NPB test と MLB full を予測します。
evaluation_sets_for_model <- function(trained_on, split_data) {
  if (trained_on == "MLB") {
    return(list(
      list(predicted_on = "MLB", prediction_data_type = "test", data = split_data$MLB$test),
      list(predicted_on = "NPB", prediction_data_type = "full_cross_league", data = split_data$NPB$full)
    ))
  }

  list(
    list(predicted_on = "NPB", prediction_data_type = "test", data = split_data$NPB$test),
    list(predicted_on = "MLB", prediction_data_type = "full_cross_league", data = split_data$MLB$full)
  )
}

# 各モデルスクリプトから呼ばれる共通パイプラインです。
# fit_function / predict_function / model_specific_importance_function を差し替えることで、
# 線形回帰からLightGBMまで同じ出力形式で処理できます。
run_model_pipeline <- function(
  model_name,
  fit_function,
  predict_function,
  model_specific_importance_function = empty_model_specific_importance
) {
  model_dir <- file.path(PREDICTIONS_ROOT, model_name)
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

  # 1. 標準化済みのMLB/NPBデータを読み込み、各リーグで train/test に分割します。
  set.seed(111)
  full_data <- list(
    MLB = read_league_data("MLB"),
    NPB = read_league_data("NPB")
  )
  split_data <- list(
    MLB = split_league_data(full_data$MLB),
    NPB = split_league_data(full_data$NPB)
  )

  assert_matching_predictors(split_data$MLB$train, split_data$MLB$test, "MLB", "MLB")
  assert_matching_predictors(split_data$NPB$train, split_data$NPB$test, "NPB", "NPB")
  assert_matching_predictors(split_data$MLB$train, split_data$NPB$full, "MLB", "NPB")
  assert_matching_predictors(split_data$NPB$train, split_data$MLB$full, "NPB", "MLB")

  # 2. MLB train と NPB train のそれぞれで、同じ手法のモデルを別々に推定します。
  fitted_models <- lapply(
    c("MLB", "NPB"),
    function(league) {
      message("Training ", model_name, " on ", league, " train data.")
      fit_function(split_data[[league]]$train)
    }
  )
  names(fitted_models) <- c("MLB", "NPB")

  prediction_results <- list()
  model_summary <- list()
  variable_importance <- list()

  # 3. 学習済みモデルごとに、同一リーグtestとクロスリーグfullへ予測します。
  for (trained_on in names(fitted_models)) {
    trained_model <- fitted_models[[trained_on]]
    train_data <- split_data[[trained_on]]$train
    specific_importance <- model_specific_importance_function(trained_model, train_data)

    for (target in evaluation_sets_for_model(trained_on, split_data)) {
      predicted_on <- target$predicted_on
      prediction_data_type <- target$prediction_data_type
      target_data <- target$data

      assert_matching_predictors(train_data, target_data, trained_on, predicted_on)

      # 4. 予測値、個票レベル誤差、評価指標を作成します。
      predictions <- as.numeric(predict_function(trained_model, target_data))
      metrics <- metric_rows(target_data, predictions, trained_on, predicted_on, prediction_data_type, model_name)

      prediction_results[[length(prediction_results) + 1]] <- prediction_rows(
        target_data,
        predictions,
        trained_on,
        predicted_on,
        prediction_data_type,
        model_name
      )

      model_summary[[length(model_summary) + 1]] <- metrics

      # 5. 評価対象データごとのMSEを基準に、Permutation Importanceを計算します。
      permutation_rows <- permutation_importance(
        trained_model,
        target_data,
        predict_function,
        metrics$mse[[1]],
        model_name,
        trained_on,
        predicted_on,
        prediction_data_type
      )

      # 6. 共通のPermutation Importanceに、モデル固有重要度があれば横結合します。
      variable_importance[[length(variable_importance) + 1]] <- permutation_rows %>%
        left_join(specific_importance, by = "variable")
    }
  }

  prediction_results <- bind_rows(prediction_results)
  model_summary <- bind_rows(model_summary)
  variable_importance <- bind_rows(variable_importance)

  # 7. モデル別ディレクトリに、個票予測・評価指標・変数重要度を出力します。
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

  # 8. 既に実行済みの他モデルも含めて、全体比較表を更新します。
  write_global_comparison_tables()

  message("Wrote outputs for ", model_name, " to ", model_dir)

  invisible(list(
    prediction_results = prediction_results,
    model_summary = model_summary,
    variable_importance = variable_importance
  ))
}

# 全モデルの model_summary.csv を読み直し、linear_regression 基準の相対指標を付与します。
# 同じ trained_on / predicted_on / prediction_data_type の線形回帰値で割ります。
summary_with_relative_metrics <- function(model_summary) {
  model_summary <- model_summary %>%
    select(any_of(BASE_SUMMARY_COLUMNS)) %>%
    distinct(across(all_of(SUMMARY_KEY_COLUMNS)), .keep_all = TRUE)

  linear_refs <- model_summary %>%
    filter(.data[["model_name"]] == "linear_regression") %>%
    select(
      "trained_on",
      "predicted_on",
      "prediction_data_type",
      linear_mse = "mse",
      linear_rmse = "rmse",
      linear_mae = "mae"
    ) %>%
    distinct(
      .data[["trained_on"]],
      .data[["predicted_on"]],
      .data[["prediction_data_type"]],
      .keep_all = TRUE
    )

  model_summary %>%
    left_join(
      linear_refs,
      by = c("trained_on", "predicted_on", "prediction_data_type")
    ) %>%
    mutate(
      mse_relative_to_linear = .data[["mse"]] / .data[["linear_mse"]],
      rmse_relative_to_linear = .data[["rmse"]] / .data[["linear_rmse"]],
      mae_relative_to_linear = .data[["mae"]] / .data[["linear_mae"]]
    ) %>%
    select(
      "model_name",
      "trained_on",
      "predicted_on",
      "prediction_data_type",
      "n_obs",
      "mse",
      "rmse",
      "mae",
      "r_squared",
      "mse_relative_to_linear",
      "rmse_relative_to_linear",
      "mae_relative_to_linear",
      everything(),
      -any_of(c("linear_mse", "linear_rmse", "linear_mae"))
    ) %>%
    mutate(
      model_name = factor(.data[["model_name"]], levels = MODEL_ORDER),
      pattern = factor(
        prediction_pattern(
          .data[["trained_on"]],
          .data[["predicted_on"]],
          .data[["prediction_data_type"]]
        ),
        levels = PREDICTION_PATTERN_ORDER
      )
    ) %>%
    arrange(.data[["model_name"]], .data[["pattern"]]) %>%
    mutate(model_name = as.character(.data[["model_name"]])) %>%
    select(-"pattern")
}

# 相対指標を付与した model_summary を、各モデルディレクトリへ書き戻します。
# これにより各モデル単体のCSVにも relative_to_linear 列が残ります。
rewrite_model_summaries <- function(model_summary) {
  for (current_model in unique(model_summary$model_name)) {
    model_dir <- file.path(PREDICTIONS_ROOT, current_model)
    output_path <- file.path(model_dir, paste0(current_model, "_model_summary.csv"))
    model_rows <- model_summary %>% filter(.data[["model_name"]] == .env$current_model)
    readr::write_csv(model_rows, output_path, na = "")
  }
}

# 全体の model_comparison_table.csv を横持ち形式で作ります。
# 行は model_name x metric、列は4つの評価パターンです。
model_comparison_wide <- function(model_summary) {
  rows <- list()
  row_id <- 1

  for (current_model in MODEL_ORDER[MODEL_ORDER %in% unique(model_summary$model_name)]) {
    model_rows <- model_summary %>% filter(.data[["model_name"]] == .env$current_model)

    for (metric in COMPARISON_METRICS) {
      out <- tibble(model_name = current_model, metric = metric)

      for (pattern in PREDICTION_PATTERN_ORDER) {
        pattern_rows <- model_rows %>%
          mutate(pattern = prediction_pattern(
            .data[["trained_on"]],
            .data[["predicted_on"]],
            .data[["prediction_data_type"]]
          )) %>%
          filter(.data[["pattern"]] == .env$pattern)

        out[[pattern]] <- if (nrow(pattern_rows) == 0) NA_real_ else pattern_rows[[metric]][[1]]
      }

      rows[[row_id]] <- out
      row_id <- row_id + 1
    }
  }

  bind_rows(rows)
}

# variable_importance_table.csv 用に、各モデル・各評価パターンの上位10変数を横持ち化します。
# 並び順は abs(permutation_importance) の大きい順です。
variable_importance_top_table <- function(variable_importance) {
  groups <- variable_importance %>%
    group_by(
      .data[["model_name"]],
      .data[["trained_on"]],
      .data[["predicted_on"]],
      .data[["prediction_data_type"]]
    ) %>%
    group_split()

  bind_rows(lapply(groups, function(group_data) {
    group_data <- group_data %>%
      arrange(desc(abs(.data[["permutation_importance"]]))) %>%
      slice_head(n = 10)

    out <- tibble(
      model_name = group_data$model_name[[1]],
      trained_on = group_data$trained_on[[1]],
      predicted_on = group_data$predicted_on[[1]],
      prediction_data_type = group_data$prediction_data_type[[1]]
    )

    for (i in seq_len(10)) {
      out[[paste0("top", i, "_variable")]] <- if (nrow(group_data) >= i) group_data$variable[[i]] else NA_character_
      out[[paste0("top", i, "_importance")]] <- if (nrow(group_data) >= i) group_data$permutation_importance[[i]] else NA_real_
      out[[paste0("top", i, "_importance_abs")]] <- if (nrow(group_data) >= i) group_data$permutation_importance_abs[[i]] else NA_real_
      out[[paste0("top", i, "_importance_standardized")]] <- if (nrow(group_data) >= i) group_data$permutation_importance_standardized[[i]] else NA_real_
      out[[paste0("top", i, "_importance_standardized_abs")]] <- if (nrow(group_data) >= i) group_data$permutation_importance_standardized_abs[[i]] else NA_real_
      out[[paste0("top", i, "_direction")]] <- if (nrow(group_data) >= i) group_data$permutation_importance_direction[[i]] else NA_character_
    }

    out
  }))
}

# 各モデルのCSVを集約し、standardized_predictions/{set}/ 直下の比較表を更新します。
# モデル実行後に毎回呼ばれるため、途中段階でも実行済みモデル分の比較表が作成されます。
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
    model_summary <- bind_rows(lapply(summary_files, readr::read_csv, show_col_types = FALSE)) %>%
      summary_with_relative_metrics()

    rewrite_model_summaries(model_summary)
    readr::write_csv(model_comparison_wide(model_summary), file.path(PREDICTIONS_ROOT, "model_comparison_table.csv"), na = "")
  }

  if (length(importance_files) > 0) {
    variable_importance <- bind_rows(lapply(importance_files, readr::read_csv, show_col_types = FALSE))
    readr::write_csv(
      variable_importance_top_table(variable_importance),
      file.path(PREDICTIONS_ROOT, "variable_importance_table.csv"),
      na = ""
    )
  }

  invisible(TRUE)
}
