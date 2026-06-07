# 処理内容:
# - 先発投手向けにSVM回帰の学習・予測関数を定義します。
# - 共通ヘルパーへモデル固有の関数を渡し、NPB/MLB間の予測結果と評価表を出力します。

# Rscriptで実行されたスクリプト自身のディレクトリを取得します。
script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)))
  }

  getwd()
}

# 共通処理またはモデル本体のスクリプトを読み込みます。
source(file.path(dirname(script_dir()), "_common", "per9_model_helpers.R"))
# モデル実装に必要なパッケージを個別に確認します。
ensure_package("e1071")

# SVM回帰モデルを学習データに当てはめます。
fit_model <- function(train_data) {
  e1071::svm(model_formula(), data = model_frame(train_data))
}

# 学習済みSVM回帰モデルで新しいデータの年俸を予測します。
predict_model <- function(model, new_data) {
  as.numeric(stats::predict(model, newdata = predictor_frame(new_data)))
}

# 定義したモデル関数を共通パイプラインに渡し、学習・予測・CSV出力を実行します。
run_model_pipeline("svm_regression", fit_model, predict_model)
