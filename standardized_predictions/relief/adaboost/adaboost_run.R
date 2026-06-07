# 処理内容:
# - 共通ヘルパー内のfit_adaboost_r2()で、rpart回帰木を弱学習器にしたAdaBoost.R2を実行します。
# - モデル固有重要度として、各弱学習器のrpart重要度をalphaで重み付けして合算した値を出力します。

# スクリプト位置を取得します。
script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)))
  }

  getwd()
}

# AdaBoost.R2の実装と共通パイプラインを読み込みます。
source(file.path(dirname(script_dir()), "_common", "standardized_model_helpers.R"))

# AdaBoost.R2モデルを推定します。
fit_model <- function(train_data) {
  fit_adaboost_r2(train_data)
}

# AdaBoost.R2モデルで予測します。
predict_model <- function(model, new_data) {
  predict_adaboost_r2(model, new_data)
}

# 共通パイプラインを実行し、AdaBoost固有重要度も併せて出力します。
run_model_pipeline("adaboost", fit_model, predict_model, adaboost_r2_importance)
