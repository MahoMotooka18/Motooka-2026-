# 処理内容:
# - 全投手向けにLightGBMの学習・予測関数を定義します。
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
ensure_package("lightgbm")

# LightGBMモデルを学習データに当てはめます。
fit_model <- function(train_data) {
  train_matrix <- model_matrix(train_data)
  train_dataset <- lightgbm::lgb.Dataset(
    data = train_matrix,
    label = train_data[[DEPENDENT_VARIABLE]]
  )

  lightgbm::lgb.train(
    params = list(objective = "regression", metric = "l2", verbosity = -1),
    data = train_dataset,
    nrounds = 100,
    verbose = -1
  )
}

# 学習済みLightGBMモデルで新しいデータの年俸を予測します。
predict_model <- function(model, new_data) {
  as.numeric(stats::predict(model, model_matrix(new_data)))
}

# LightGBM固有の変数重要度を抽出し、共通形式へ整えます。
model_importance <- function(model, train_data) {
  importance <- lightgbm::lgb.importance(model)

  tibble(
    variable = importance$Feature,
    lightgbm_gain = importance$Gain,
    lightgbm_cover = importance$Cover,
    lightgbm_frequency = importance$Frequency
  ) %>%
    right_join(tibble(variable = EXPLANATORY_VARIABLES_PER9), by = "variable") %>%
    mutate(
      lightgbm_gain = tidyr_free_na_to_zero(.data[["lightgbm_gain"]]),
      lightgbm_cover = tidyr_free_na_to_zero(.data[["lightgbm_cover"]]),
      lightgbm_frequency = tidyr_free_na_to_zero(.data[["lightgbm_frequency"]])
    )
}

# 定義したモデル関数を共通パイプラインに渡し、学習・予測・CSV出力を実行します。
run_model_pipeline("lightgbm", fit_model, predict_model, model_importance)
