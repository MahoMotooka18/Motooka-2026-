# 処理内容:
# - ipred::bagging() によるバギング回帰を実行します。
# - パラメータはipred::bagging()のデフォルトを使います。
# - 共通のPermutation Importanceを変数重要度として出力します。

# スクリプト位置を取得します。
script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)))
  }

  getwd()
}

# 共通処理とipredパッケージを準備します。
source(file.path(dirname(script_dir()), "_common", "standardized_model_helpers.R"))
ensure_package("ipred")

# バギング回帰モデルを推定します。
fit_model <- function(train_data) {
  ipred::bagging(model_formula(), data = model_frame(train_data))
}

# 学習済みバギングモデルで予測します。
predict_model <- function(model, new_data) {
  as.numeric(stats::predict(model, newdata = predictor_frame(new_data)))
}

# モデル固有重要度関数は渡さず、Permutation Importanceのみを使います。
run_model_pipeline("bagging", fit_model, predict_model)
