# 処理内容:
# - e1071::svm() によるSVM回帰を実行します。
# - パラメータはe1071::svm()のデフォルトを使います。
# - SVMには共通指標としてPermutation Importanceを計算し、モデル固有重要度は追加しません。

# スクリプト位置を取得します。
script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)))
  }

  getwd()
}

# 共通処理とe1071パッケージを準備します。
source(file.path(dirname(script_dir()), "_common", "standardized_model_helpers.R"))
ensure_package("e1071")

# SVM回帰モデルを推定します。
fit_model <- function(train_data) {
  e1071::svm(model_formula(), data = model_frame(train_data))
}

# 学習済みSVMモデルで予測します。
predict_model <- function(model, new_data) {
  as.numeric(stats::predict(model, newdata = predictor_frame(new_data)))
}

# モデル固有重要度関数を渡さないため、共通パイプライン側でPermutation Importanceのみ出力します。
run_model_pipeline("svm_regression", fit_model, predict_model)
