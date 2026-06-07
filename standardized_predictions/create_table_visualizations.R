# 処理内容:
# - standardized_predictions/{all|starting_pitchers|relief} の比較CSVを読み込みます。
# - model_comparison_table.csv は MLB_train / NPB_train ごとに、5モデル x 2段のJPG表へ整形します。
# - variable_importance_table.csv は MLB_train / NPB_train ごとに、上位10変数のJPG表へ整形します。
# - 予測モデルやCSVの数値は変更せず、表示用の表画像だけを作成します。

# JPG表の作成に必要なパッケージを確認します。
required_packages <- c("dplyr", "readr", "tibble", "gridExtra")
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
  library(grid)
  library(gridExtra)
})

# 実行位置からプロジェクトルートを検出します。
# Rscript standardized_predictions/create_table_visualizations.R でも、
# プロジェクトルートからの手動実行でも動くように候補を複数確認します。
detect_project_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  candidates <- c(
    if (length(file_arg) > 0) {
      file.path(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE)), "..")
    },
    getwd(),
    file.path(getwd(), "..")
  )

  for (candidate in candidates) {
    candidate <- normalizePath(candidate, mustWork = FALSE)
    if (dir.exists(file.path(candidate, "standardized_predictions"))) {
      return(candidate)
    }
  }

  normalizePath(getwd(), mustWork = FALSE)
}

project_root <- detect_project_root()
standardized_root <- file.path(project_root, "standardized_predictions")

# 可視化対象の3セットと、画像を分ける学習リーグです。
prediction_sets <- c("all", "starting_pitchers", "relief")
train_leagues <- c("MLB", "NPB")

# 表示列のモデル順です。比較表では5モデルずつ上下2ブロックに分割します。
model_order <- c(
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

# model_comparison_table.csv から可視化する指標です。
# MSE/RMSE/MAE は linear_regression 基準の相対値、R_squared は元値です。
comparison_metrics <- c(
  "mse_relative_to_linear",
  "rmse_relative_to_linear",
  "mae_relative_to_linear",
  "r_squared"
)

# CSV上の評価パターン列の順序です。
# MLB_train 画像では前半2列、NPB_train 画像では後半2列を使います。
prediction_pattern_order <- c(
  "MLB_train_MLB_test",
  "MLB_train_NPB_full",
  "NPB_train_NPB_test",
  "NPB_train_MLB_full"
)

# 表示用に数値の桁数を整えます。
# 変数重要度など広い範囲の値を想定し、大きい値はカンマ区切り、小さい値は小数表示にします。
format_number <- function(x) {
  vapply(
    x,
    function(value) {
      if (is.na(value)) {
        return("")
      }

      abs_value <- abs(value)

      if (abs_value >= 1e12) {
        return(formatC(value, format = "e", digits = 2))
      }

      if (abs_value >= 100) {
        return(formatC(round(value), format = "f", digits = 0, big.mark = ","))
      }

      if (abs_value >= 10) {
        out <- formatC(round(value, 1), format = "f", digits = 1, big.mark = ",")
        return(sub("[.]0$", "", out))
      }

      if (abs_value >= 1) {
        out <- formatC(round(value, 3), format = "f", digits = 3)
        return(sub("[.]?0+$", "", out))
      }

      out <- formatC(round(value, 4), format = "f", digits = 4)
      sub("[.]?0+$", "", out)
    },
    character(1)
  )
}

# 既存 predictions 側の表示と互換にするための順位計算関数です。
# 現在の model_comparison_table JPG では順位を表示せず、値のみ表示しています。
rank_metric <- function(x, higher_is_better = FALSE) {
  rank(if (higher_is_better) -x else x, ties.method = "min", na.last = "keep")
}

# 順位と値を1つのセルにまとめる補助関数です。
# 現在は比較表では使わず、過去形式を保つため残しています。
rank_value_cell <- function(rank_value, raw_value) {
  paste0(rank_value, "\n(", format_number(raw_value), ")")
}

# モデル比較表の指標値を3桁小数で表示します。
# 例: 1 -> 1.000、0.4183941 -> 0.418。
format_comparison_value <- function(x) {
  vapply(
    x,
    function(value) {
      if (is.na(value)) {
        return("")
      }

      formatC(round(value, 3), format = "f", digits = 3)
    },
    character(1)
  )
}

# 変数名と重要度を1セルにまとめます。
# 例: age と 0.1234 を age\n(0.1234) の形にします。
importance_cell <- function(variable, value) {
  ifelse(
    is.na(variable) | variable == "",
    "",
    paste0(variable, "\n(", format_number(value), ")")
  )
}

# 画像タイトルに使う予測セット名を読みやすい英語表記にします。
pretty_set_name <- function(prediction_set) {
  dplyr::case_when(
    prediction_set == "all" ~ "All Pitchers",
    prediction_set == "starting_pitchers" ~ "Starting Pitchers",
    prediction_set == "relief" ~ "Relief Pitchers",
    TRUE ~ prediction_set
  )
}

# CSV上の指標名を、画像内の短い表示名へ変換します。
metric_label <- function(metric) {
  dplyr::case_when(
    metric == "mse_relative_to_linear" ~ "MSE rel.",
    metric == "rmse_relative_to_linear" ~ "RMSE rel.",
    metric == "mae_relative_to_linear" ~ "MAE rel.",
    metric == "r_squared" ~ "R Squared",
    TRUE ~ metric
  )
}

# 評価パターン列名から学習リーグだけを取り出します。
# 例: MLB_train_NPB_full -> MLB。
pattern_trained_on <- function(pattern) {
  sub("_train_.*$", "", pattern)
}

# 評価パターン列名から予測対象データのラベルを作ります。
# 例: MLB_train_NPB_full -> NPB_full。
pattern_target_label <- function(pattern) {
  target <- sub("^[A-Z]+_train_", "", pattern)
  target
}

# variable_importance_table.csv の行情報から、画像列に使う予測対象ラベルを作ります。
prediction_row_label <- function(predicted_on, prediction_data_type) {
  paste(
    predicted_on,
    ifelse(prediction_data_type == "test", "test", "full")
  )
}

# model_comparison_table.csv を、指定学習リーグ用のJPG表示テーブルへ変換します。
# 1ブロック5モデル、各モデルに same-league test と cross-league full の2列を持たせます。
# 10モデルを横1行に並べると画像が横長になるため、5モデルずつ上下2ブロックにしています。
make_model_comparison_display <- function(path, trained_on) {
  data <- readr::read_csv(path, show_col_types = FALSE)
  pattern_columns <- prediction_pattern_order[prediction_pattern_order %in% names(data)]
  train_patterns <- pattern_columns[pattern_trained_on(pattern_columns) == trained_on]
  metric_rows <- c("Data", metric_label(comparison_metrics))

  # 1ブロックあたりのモデル数を固定します。
  # 現在は10モデルなので、5モデル x 2ブロックの表になります。
  models_per_block <- 5
  model_blocks <- split(model_order, ceiling(seq_along(model_order) / models_per_block))
  block_column_count <- 1 + models_per_block * length(train_patterns)

  # 各ブロックのヘッダー行を作ります。
  # モデル名は2列のうち左側だけに表示し、右側は空欄にしてグループ感を出します。
  block_headers <- function(block_models) {
    headers <- "Metric"

    for (model_index in seq_len(models_per_block)) {
      model <- if (length(block_models) >= model_index) block_models[[model_index]] else ""
      headers <- c(headers, model, rep("", length(train_patterns) - 1))
    }

    headers
  }

  # 1ブロック分の Data / MSE rel. / RMSE rel. / MAE rel. / R Squared 行を作ります。
  # データ行には NPB_test / MLB_full など、各モデル内の2列ラベルを入れます。
  block_rows <- function(block_models) {
    display_matrix <- matrix(
      "",
      nrow = length(metric_rows),
      ncol = block_column_count
    )

    display_matrix[, 1] <- metric_rows
    col_index <- 2

    for (model_index in seq_len(models_per_block)) {
      model <- if (length(block_models) >= model_index) block_models[[model_index]] else NA_character_

      for (pattern in train_patterns) {
        display_matrix[1, col_index] <- pattern_target_label(pattern)

        if (!is.na(model)) {
          for (metric_index in seq_along(comparison_metrics)) {
            metric <- comparison_metrics[[metric_index]]
            metric_row <- data %>%
              filter(.data[["model_name"]] == .env$model, .data[["metric"]] == .env$metric)

            # CSVから該当モデル・該当指標・該当評価パターンの値を取り出します。
            display_matrix[metric_index + 1, col_index] <- if (nrow(metric_row) == 0) {
              ""
            } else {
              format_comparison_value(metric_row[[pattern]][[1]])
            }
          }
        }

        col_index <- col_index + 1
      }
    }

    display_matrix
  }

  display_matrix <- block_rows(model_blocks[[1]])

  # 2ブロック目以降は、本文中にヘッダー行を挿入してから同じ形式の行を足します。
  if (length(model_blocks) > 1) {
    for (block_index in seq(2, length(model_blocks))) {
      display_matrix <- rbind(
        display_matrix,
        block_headers(model_blocks[[block_index]]),
        block_rows(model_blocks[[block_index]])
      )
    }
  }

  # data.frameの実際の列名は一意にする必要があるため内部名を付け、
  # 画像描画時に使う表示用ヘッダーは属性として別に保持します。
  display_data <- as.data.frame(display_matrix, stringsAsFactors = FALSE)
  names(display_data) <- paste0("column_", seq_len(ncol(display_data)))
  attr(display_data, "display_colnames") <- block_headers(model_blocks[[1]])
  display_data
}

# variable_importance_table.csv を、指定学習リーグ用のJPG表示テーブルへ変換します。
# 各Top行に、モデル x 予測対象データの列を並べます。
make_variable_importance_display <- function(path, trained_on) {
  data <- readr::read_csv(path, show_col_types = FALSE) %>%
    filter(.data[["trained_on"]] == .env$trained_on) %>%
    mutate(
      model_name = factor(.data[["model_name"]], levels = model_order),
      target_label = prediction_row_label(.data[["predicted_on"]], .data[["prediction_data_type"]])
    ) %>%
    arrange(.data[["model_name"]], .data[["predicted_on"]])

  rows <- lapply(seq_len(10), function(i) {
    out <- tibble(Rank = paste0("Top ", i))

    for (model in model_order) {
      model_rows <- data %>% filter(as.character(.data[["model_name"]]) == .env$model)

      for (row_index in seq_len(nrow(model_rows))) {
        current_row <- model_rows[row_index, , drop = FALSE]
        column_name <- paste(model, current_row$target_label[[1]], sep = "\n")
        variable_column <- paste0("top", i, "_variable")
        importance_column <- paste0("top", i, "_importance")
        out[[column_name]] <- importance_cell(
          current_row[[variable_column]][[1]],
          current_row[[importance_column]][[1]]
        )
      }
    }

    out
  })

  bind_rows(rows)
}

# 表示用data.frameをJPG画像として描画します。
# table_type により、比較表と重要度表で画像幅・文字サイズを変えます。
draw_table_jpg <- function(data, output_path, title, subtitle, table_type = c("comparison", "importance")) {
  table_type <- match.arg(table_type)
  n_rows <- nrow(data) + 1
  n_cols <- ncol(data)

  image_width <- if (table_type == "importance") max(7200, n_cols * 420) else max(4800, n_cols * 430)
  image_height <- if (table_type == "comparison") max(1050, n_rows * 80 + 230) else max(900, n_rows * 125 + 360)
  body_font_size <- if (table_type == "importance") 7 else 9
  header_font_size <- if (table_type == "importance") 8 else 10

  theme <- gridExtra::ttheme_minimal(
    base_size = body_font_size,
    core = list(
      fg_params = list(fontface = "plain", col = "#172033", lineheight = 0.9),
      bg_params = list(fill = rep(c("#FFFFFF", "#F7F9FC"), length.out = nrow(data)), col = "#D8DEE9")
    ),
    colhead = list(
      fg_params = list(fontface = "bold", col = "#FFFFFF", fontsize = header_font_size),
      bg_params = list(fill = "#243B53", col = "#243B53")
    )
  )

  display_colnames <- attr(data, "display_colnames", exact = TRUE)

  # display_colnames 属性があれば、内部列名ではなく表示用ヘッダーで描画します。
  table_grob <- gridExtra::tableGrob(
    data,
    rows = NULL,
    cols = if (is.null(display_colnames)) names(data) else display_colnames,
    theme = theme
  )
  table_grob$widths <- grid::unit(rep(1 / n_cols, n_cols), "npc")

  # 比較表の2ブロック目に入る本文中ヘッダー行も、通常の列ヘッダーと同じ様式にします。
  # gridExtra上では本文行として扱われるため、背景色と文字色を手動で上書きします。
  if (table_type == "comparison") {
    repeated_header_rows <- which(data[[1]] == "Metric")

    if (length(repeated_header_rows) > 0) {
      repeated_header_table_rows <- repeated_header_rows + 1
      background_indexes <- which(
        table_grob$layout$name == "core-bg" &
          table_grob$layout$t %in% repeated_header_table_rows
      )
      foreground_indexes <- which(
        table_grob$layout$name == "core-fg" &
          table_grob$layout$t %in% repeated_header_table_rows
      )

      for (index in background_indexes) {
        table_grob$grobs[[index]]$gp$fill <- "#243B53"
        table_grob$grobs[[index]]$gp$col <- "#243B53"
      }

      for (index in foreground_indexes) {
        table_grob$grobs[[index]]$gp$col <- "#FFFFFF"
        table_grob$grobs[[index]]$gp$font <- NULL
        table_grob$grobs[[index]]$gp$fontface <- "bold"
        table_grob$grobs[[index]]$gp$fontsize <- header_font_size
      }
    }
  }

  # JPGデバイスを開き、白背景、タイトル、サブタイトル、表本体の順に描画します。
  grDevices::jpeg(output_path, width = image_width, height = image_height, res = 220, quality = 96)
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = "#FFFFFF", col = NA))

  layout <- grid::grid.layout(
    nrow = 3,
    ncol = 1,
    heights = grid::unit(c(0.08, 0.06, 0.86), "npc")
  )
  grid::pushViewport(grid::viewport(layout = layout))

  grid::pushViewport(grid::viewport(layout.pos.row = 1))
  grid::grid.text(
    title,
    x = grid::unit(0.02, "npc"),
    y = grid::unit(0.55, "npc"),
    just = c("left", "center"),
    gp = grid::gpar(fontsize = 18, fontface = "bold", col = "#102A43")
  )
  grid::popViewport()

  grid::pushViewport(grid::viewport(layout.pos.row = 2))
  grid::grid.text(
    subtitle,
    x = grid::unit(0.02, "npc"),
    y = grid::unit(0.55, "npc"),
    just = c("left", "center"),
    gp = grid::gpar(fontsize = 10, col = "#52606D")
  )
  grid::popViewport()

  grid::pushViewport(grid::viewport(layout.pos.row = 3))
  grid::pushViewport(grid::viewport(
    y = grid::unit(1, "npc"),
    height = sum(table_grob$heights),
    just = c("center", "top")
  ))
  grid::grid.draw(table_grob)
  grid::popViewport(3)

  grDevices::dev.off()

  invisible(output_path)
}

# 学習リーグ別JPGの出力ファイル名を作ります。
# 例: model_comparison_table_MLB_train.jpg。
output_path_for_train <- function(prediction_dir, base_name, trained_on) {
  file.path(
    prediction_dir,
    base_name,
    paste0(base_name, "_", trained_on, "_train.jpg")
  )
}

# 1つの予測セットについて、モデル比較表と変数重要度表をJPG化します。
create_visualizations_for_set <- function(prediction_set) {
  prediction_dir <- file.path(standardized_root, prediction_set)
  model_comparison_path <- file.path(prediction_dir, "model_comparison_table.csv")
  variable_importance_path <- file.path(prediction_dir, "variable_importance_table.csv")

  if (!file.exists(model_comparison_path)) {
    stop("Missing file: ", model_comparison_path, call. = FALSE)
  }

  if (!file.exists(variable_importance_path)) {
    stop("Missing file: ", variable_importance_path, call. = FALSE)
  }

  display_name <- pretty_set_name(prediction_set)

  dir.create(file.path(prediction_dir, "model_comparison_table"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(prediction_dir, "variable_importance_table"), recursive = TRUE, showWarnings = FALSE)

  for (trained_on in train_leagues) {
    # 学習リーグごとに表を分けて作ることで、MLB_train と NPB_train を別画像で比較できます。
    model_comparison_display <- make_model_comparison_display(model_comparison_path, trained_on)
    variable_importance_display <- make_variable_importance_display(variable_importance_path, trained_on)

    draw_table_jpg(
      data = model_comparison_display,
      output_path = output_path_for_train(prediction_dir, "model_comparison_table", trained_on),
      title = paste(display_name, "Model Comparison -", trained_on, "train"),
      subtitle = "Columns are grouped by model; each model has same-league test and cross-league full prediction values.",
      table_type = "comparison"
    )

    draw_table_jpg(
      data = variable_importance_display,
      output_path = output_path_for_train(prediction_dir, "variable_importance_table", trained_on),
      title = paste(display_name, "Variable Importance -", trained_on, "train"),
      subtitle = "Each Top cell shows variable name, then permutation importance in parentheses. Columns separate same-league test and cross-league full prediction.",
      table_type = "importance"
    )
  }

  message("Wrote table JPGs to ", prediction_dir)

  invisible(TRUE)
}

# all / starting_pitchers / relief の3セットをまとめて可視化します。
create_all_visualizations <- function() {
  invisible(lapply(prediction_sets, create_visualizations_for_set))
}

# Rscriptで直接実行された場合だけ、全セットのJPG生成を開始します。
if (any(grepl("^--file=", commandArgs(trailingOnly = FALSE)))) {
  create_all_visualizations()
}
