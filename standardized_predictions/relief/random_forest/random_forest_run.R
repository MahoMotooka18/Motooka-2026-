# 処理内容:
# - ranger::ranger() によるRandom Forest回帰を実行します。
# - rangerのimpurity importanceをモデル固有重要度として出力します。
# - 共通指標としてPermutation Importanceも別途計算します。

# スクリプト位置を取得します。
script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)))
  }

  getwd()
}

# 共通処理とrangerパッケージを準備します。
source(file.path(dirname(script_dir()), "_common", "standardized_model_helpers.R"))
ensure_package("ranger")

# Random Forest回帰モデルを推定します。
# importance = "impurity" により、ranger::importance() で不純度ベース重要度を取得できます。
fit_model <- function(train_data) {
  ranger::ranger(
    formula = model_formula(),
    data = model_frame(train_data),
    importance = "impurity"
  )
}

# rangerモデルの予測値を取り出します。
predict_model <- function(model, new_data) {
  as.numeric(stats::predict(model, data = predictor_frame(new_data))$predictions)
}

# rangerの不純度ベース重要度を全説明変数に揃え、標準化値も追加します。
model_importance <- function(model, train_data) {
  normalize_named_importance(ranger::importance(model), "ranger_impurity_importance") %>%
    mutate(
      ranger_impurity_importance_standardized =
        standardize_importance(.data[["ranger_impurity_importance"]])
    )
}

# 共通パイプラインを実行します。
run_model_pipeline("random_forest", fit_model, predict_model, model_importance)
