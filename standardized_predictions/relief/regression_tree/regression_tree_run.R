# 処理内容:
# - rpart::rpart() による回帰木モデルを実行します。
# - rpartのデフォルト設定を使い、モデル固有重要度としてrpartのvariable.importanceを出力します。

# 実行スクリプトの場所を取得します。
script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)))
  }

  getwd()
}

# 共通ヘルパーとrpartパッケージを準備します。
source(file.path(dirname(script_dir()), "_common", "standardized_model_helpers.R"))
ensure_package("rpart")

# 回帰木を推定します。
fit_model <- function(train_data) {
  rpart::rpart(model_formula(), data = model_frame(train_data))
}

# 学習済み回帰木で予測します。
predict_model <- function(model, new_data) {
  as.numeric(stats::predict(model, newdata = predictor_frame(new_data)))
}

# rpartが返すvariable.importanceを全説明変数の表に揃え、最大絶対値で標準化します。
model_importance <- function(model, train_data) {
  normalize_named_importance(model$variable.importance, "rpart_importance") %>%
    mutate(rpart_importance_standardized = standardize_importance(.data[["rpart_importance"]]))
}

# 共通パイプラインを実行します。
run_model_pipeline("regression_tree", fit_model, predict_model, model_importance)
