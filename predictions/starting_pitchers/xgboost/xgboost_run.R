# 処理内容:
# - 先発投手向けにXGBoostの学習・予測関数を定義します。
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
ensure_package("xgboost")

# XGBoostモデルを学習データに当てはめます。
fit_model <- function(train_data) {
  xgboost::xgboost(
    data = model_matrix(train_data),
    label = train_data[[DEPENDENT_VARIABLE]],
    objective = "reg:squarederror",
    nrounds = 100,
    verbose = 0
  )
}

# 学習済みXGBoostモデルで新しいデータの年俸を予測します。
predict_model <- function(model, new_data) {
  as.numeric(stats::predict(model, model_matrix(new_data)))
}

# XGBoost固有の変数重要度を抽出し、共通形式へ整えます。
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

# 定義したモデル関数を共通パイプラインに渡し、学習・予測・CSV出力を実行します。
run_model_pipeline("xgboost", fit_model, predict_model, model_importance)
