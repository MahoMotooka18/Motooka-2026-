# 処理内容:
# - 全投手向けにAdaBoost.R2の学習・予測関数を定義します。
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

# AdaBoost.R2モデルを学習データに当てはめます。
fit_model <- function(train_data) {
  fit_adaboost_r2(train_data)
}

# 学習済みAdaBoost.R2モデルで新しいデータの年俸を予測します。
predict_model <- function(model, new_data) {
  predict_adaboost_r2(model, new_data)
}

# 定義したモデル関数を共通パイプラインに渡し、学習・予測・CSV出力を実行します。
run_model_pipeline("adaboost", fit_model, predict_model, adaboost_r2_importance)
