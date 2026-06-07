# 処理内容:
# - 各予測モデルが出力した評価表と変数重要度表を読み込みます。
# - 比較しやすい表示形式に整え、全投手・先発・救援ごとのJPG表画像を作成します。

# 必要なRパッケージを一覧化し、未導入なら処理開始前に止めます。
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

# パッケージ読み込み時の起動メッセージを抑え、ログを読みやすくします。
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(grid)
  library(gridExtra)
})

# 実行場所からプロジェクトルートを検出し、以降の相対パス解決に使います。
detect_project_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  candidates <- c(
    if (length(file_arg) > 0) {
      file.path(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE)), "..")
    },
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "..", "..")
  )

  for (candidate in candidates) {
    candidate <- normalizePath(candidate, mustWork = FALSE)
    if (dir.exists(file.path(candidate, "predictions"))) {
      return(candidate)
    }
  }

  normalizePath(getwd(), mustWork = FALSE)
}

# 検出したプロジェクトルートを、このスクリプト内の入出力パスの起点にします。
project_root <- detect_project_root()

# 可視化対象にする投手カテゴリをまとめます。
prediction_sets <- c("all", "starting_pitchers", "relief_pitchers")

# 大きさに応じて数値の桁数や指数表記を調整します。
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

      if (abs_value >= 1000) {
        return(formatC(round(value), format = "f", digits = 0, big.mark = ","))
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

# 評価指標を良い順に順位付けします。
rank_metric <- function(x, higher_is_better = FALSE) {
  rank(if (higher_is_better) -x else x, ties.method = "min", na.last = "keep")
}

# 順位と元の指標値を1つの表セル文字列にまとめます。
rank_value_cell <- function(rank_value, raw_value) {
  paste0(rank_value, "\n(", format_number(raw_value), ")")
}

# 変数名と重要度を1つの表セル文字列にまとめます。
importance_cell <- function(variable, value) {
  ifelse(
    is.na(variable) | variable == "",
    "",
    paste0(variable, "\n(", format_number(value), ")")
  )
}

# 予測セット名を画像タイトル向けの読みやすい表記へ変換します。
pretty_set_name <- function(prediction_set) {
  dplyr::case_when(
    prediction_set == "all" ~ "All Pitchers",
    prediction_set == "starting_pitchers" ~ "Starting Pitchers",
    prediction_set == "relief_pitchers" ~ "Relief Pitchers",
    TRUE ~ prediction_set
  )
}

# モデル比較CSVを読み込み、順位付きの表示テーブルに変換します。
make_model_comparison_display <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) %>%
    group_by(.data[["trained_on"]], .data[["predicted_on"]]) %>%
    mutate(
      mse_rank = rank_metric(.data[["mse"]]),
      rmse_rank = rank_metric(.data[["rmse"]]),
      mae_rank = rank_metric(.data[["mae"]]),
      r_squared_rank = rank_metric(.data[["r_squared"]], higher_is_better = TRUE)
    ) %>%
    ungroup() %>%
    arrange(.data[["trained_on"]], .data[["predicted_on"]], .data[["mse_rank"]], .data[["model_name"]]) %>%
    transmute(
      Model = .data[["model_name"]],
      Trained = .data[["trained_on"]],
      Predicted = .data[["predicted_on"]],
      MSE = rank_value_cell(.data[["mse_rank"]], .data[["mse"]]),
      RMSE = rank_value_cell(.data[["rmse_rank"]], .data[["rmse"]]),
      MAE = rank_value_cell(.data[["mae_rank"]], .data[["mae"]]),
      `R Squared` = rank_value_cell(.data[["r_squared_rank"]], .data[["r_squared"]]),
      N = format_number(.data[["n_obs"]])
    )
}

# 変数重要度CSVを読み込み、上位変数の表示テーブルに変換します。
make_variable_importance_display <- function(path) {
  data <- readr::read_csv(path, show_col_types = FALSE) %>%
    arrange(.data[["trained_on"]], .data[["predicted_on"]], .data[["model_name"]])

  out <- data %>%
    transmute(
      Model = .data[["model_name"]],
      Trained = .data[["trained_on"]],
      Predicted = .data[["predicted_on"]]
    )

  for (i in seq_len(10)) {
    variable_column <- paste0("top", i, "_variable")
    importance_column <- paste0("top", i, "_importance")
    out[[paste0("Top ", i)]] <- importance_cell(data[[variable_column]], data[[importance_column]])
  }

  out
}

# 表示テーブルをJPG画像として描画・保存します。
draw_table_jpg <- function(data, output_path, title, subtitle, table_type = c("comparison", "importance")) {
  table_type <- match.arg(table_type)
  n_rows <- nrow(data) + 1
  n_cols <- ncol(data)

  image_width <- if (table_type == "importance") max(6200, n_cols * 460) else max(3600, n_cols * 450)
  image_height <- max(900, n_rows * 120 + 360)
  body_font_size <- if (table_type == "importance") 8 else 9
  header_font_size <- if (table_type == "importance") 9 else 10

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

  table_grob <- gridExtra::tableGrob(data, rows = NULL, theme = theme)
  table_grob$widths <- grid::unit(rep(1 / n_cols, n_cols), "npc")

  grDevices::jpeg(output_path, width = image_width, height = image_height, res = 220, quality = 96)
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = "#FFFFFF", col = NA))

  layout <- grid::grid.layout(
    nrow = 3,
    ncol = 1,
    heights = grid::unit(c(0.07, 0.05, 0.88), "npc")
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

# モデル比較表を、指標を行にした転置表示へ変換します。
transpose_model_comparison_table <- function(data) {
  model_names <- data[["Model"]]
  values <- data %>%
    select(-all_of(c("Model", "Trained", "Predicted")))

  transposed <- as.data.frame(t(as.matrix(values)), stringsAsFactors = FALSE)
  names(transposed) <- model_names
  transposed <- tibble::rownames_to_column(transposed, var = "Metric")
  tibble::as_tibble(transposed)
}

# 変数重要度表を、上位順位を行にした転置表示へ変換します。
transpose_variable_importance_table <- function(data) {
  model_names <- data[["Model"]]
  values <- data %>%
    select(-all_of(c("Model", "Trained", "Predicted")))

  transposed <- as.data.frame(t(as.matrix(values)), stringsAsFactors = FALSE)
  names(transposed) <- model_names
  transposed <- tibble::rownames_to_column(transposed, var = "Rank")
  tibble::as_tibble(transposed)
}

# 学習リーグと予測リーグの組み合わせごとに表を分割します。
split_by_trained_predicted <- function(data) {
  split(
    data,
    paste(data[["Trained"]], data[["Predicted"]], sep = "_")
  )
}

# 分割画像の出力ファイル名を学習/予測リーグ付きで作ります。
split_output_path <- function(prediction_dir, base_name, trained_on, predicted_on) {
  file.path(
    prediction_dir,
    base_name,
    paste0(
      base_name,
      "_trained_",
      tolower(trained_on),
      "_predicted_",
      tolower(predicted_on),
      ".jpg"
    )
  )
}

# 学習/予測リーグ別に分割した表画像をまとめて保存します。
draw_split_tables <- function(
  display_data,
  prediction_dir,
  base_name,
  title_prefix,
  subtitle,
  table_type,
  transpose_function
) {
  dir.create(file.path(prediction_dir, base_name), recursive = TRUE, showWarnings = FALSE)
  split_tables <- split_by_trained_predicted(display_data)

  for (table_data in split_tables) {
    trained_on <- table_data[["Trained"]][[1]]
    predicted_on <- table_data[["Predicted"]][[1]]
    transposed_data <- transpose_function(table_data)

    draw_table_jpg(
      data = transposed_data,
      output_path = split_output_path(prediction_dir, base_name, trained_on, predicted_on),
      title = paste(title_prefix, "-", trained_on, "trained,", predicted_on, "predicted"),
      subtitle = subtitle,
      table_type = table_type
    )
  }

  invisible(TRUE)
}

# 1つの予測セットについて比較表と重要度表の画像を作成します。
create_visualizations_for_set <- function(prediction_set) {
  prediction_dir <- file.path(project_root, "predictions", prediction_set)
  model_comparison_path <- file.path(prediction_dir, "model_comparison_table.csv")
  variable_importance_path <- file.path(prediction_dir, "variable_importance_table.csv")

  if (!file.exists(model_comparison_path)) {
    stop("Missing file: ", model_comparison_path, call. = FALSE)
  }

  if (!file.exists(variable_importance_path)) {
    stop("Missing file: ", variable_importance_path, call. = FALSE)
  }

  display_name <- pretty_set_name(prediction_set)

  model_comparison_display <- make_model_comparison_display(model_comparison_path)
  variable_importance_display <- make_variable_importance_display(variable_importance_path)

  draw_split_tables(
    display_data = model_comparison_display,
    prediction_dir = prediction_dir,
    base_name = "model_comparison_table",
    title_prefix = paste(display_name, "Model Comparison"),
    subtitle = "Each metric cell shows rank within the trained/predicted league pair, then the rounded value. R Squared is ranked high to low.",
    table_type = "comparison",
    transpose_function = transpose_model_comparison_table
  )

  draw_split_tables(
    display_data = variable_importance_display,
    prediction_dir = prediction_dir,
    base_name = "variable_importance_table",
    title_prefix = paste(display_name, "Variable Importance"),
    subtitle = "Each Top cell shows variable name, then standardized permutation importance in parentheses.",
    table_type = "importance",
    transpose_function = transpose_variable_importance_table
  )

  message("Wrote table JPGs to ", prediction_dir)

  invisible(TRUE)
}

# 全投手・先発・救援の全予測セットについて表画像を一括作成します。
create_all_visualizations <- function() {
  invisible(lapply(prediction_sets, create_visualizations_for_set))
}

# Rscriptで直接実行された場合だけ、一括処理を起動します。
if (any(grepl("^--file=", commandArgs(trailingOnly = FALSE)))) {
  create_all_visualizations()
}
