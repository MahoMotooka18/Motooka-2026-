# 処理内容:
# - 全投手向けにランダムフォレストの学習・予測関数を定義します。
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
ensure_package("ranger")

# ランダムフォレストモデルを学習データに当てはめます。
fit_model <- function(train_data) {
  ranger::ranger(
    formula = model_formula(),
    data = model_frame(train_data),
    importance = "impurity"
  )
}

# 学習済みランダムフォレストモデルで新しいデータの年俸を予測します。
predict_model <- function(model, new_data) {
  as.numeric(stats::predict(model, data = predictor_frame(new_data))$predictions)
}

# ランダムフォレスト固有の変数重要度を抽出し、共通形式へ整えます。
model_importance <- function(model, train_data) {
  normalize_named_importance(ranger::importance(model), "ranger_impurity_importance") %>%
    mutate(
      ranger_impurity_importance_standardized =
        standardize_importance(.data[["ranger_impurity_importance"]])
    )
}

# 定義したモデル関数を共通パイプラインに渡し、学習・予測・CSV出力を実行します。
run_model_pipeline("random_forest", fit_model, predict_model, model_importance)
