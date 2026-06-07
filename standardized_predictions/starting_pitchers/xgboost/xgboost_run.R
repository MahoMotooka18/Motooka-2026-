# 処理内容:
# - xgboost::xgboost() によるXGBoost回帰を実行します。
# - objective = "reg:squarederror"、nrounds = 100 に固定しています。
# - モデル固有重要度としてGain, Cover, Frequencyを出力します。

# スクリプト位置を取得します。
script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)))
  }

  getwd()
}

# 共通処理とxgboostパッケージを準備します。
source(file.path(dirname(script_dir()), "_common", "standardized_model_helpers.R"))
ensure_package("xgboost")

# XGBoost回帰モデルを推定します。
fit_model <- function(train_data) {
  xgboost::xgboost(
    data = model_matrix(train_data),
    label = train_data[[DEPENDENT_VARIABLE]],
    objective = "reg:squarederror",
    nrounds = 100,
    verbose = 0
  )
}

# 学習済みXGBoostモデルで予測します。
predict_model <- function(model, new_data) {
  as.numeric(stats::predict(model, model_matrix(new_data)))
}

# xgb.importance() からGain, Cover, Frequencyを取得し、全説明変数の表に揃えます。
model_importance <- function(model, train_data) {
  importance <- xgboost::xgb.importance(
    feature_names = EXPLANATORY_VARIABLES_PER9,
    model = model
  )

  tibble(
    variable = importance$Feature,
    xgboost_gain = importance$Gain,
    xgboost_cover = importance$Cover,
    xgboost_frequency = importance$Frequency
  ) %>%
    right_join(tibble(variable = EXPLANATORY_VARIABLES_PER9), by = "variable") %>%
    mutate(
      xgboost_gain = tidyr_free_na_to_zero(.data[["xgboost_gain"]]),
      xgboost_cover = tidyr_free_na_to_zero(.data[["xgboost_cover"]]),
      xgboost_frequency = tidyr_free_na_to_zero(.data[["xgboost_frequency"]])
    )
}

# 共通パイプラインを実行します。
run_model_pipeline("xgboost", fit_model, predict_model, model_importance)
