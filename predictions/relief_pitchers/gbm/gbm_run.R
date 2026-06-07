# 処理内容:
# - 救援投手向けにGBMの学習・予測関数を定義します。
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
ensure_package("gbm")

# GBMモデルを学習データに当てはめます。
fit_model <- function(train_data) {
  gbm::gbm(
    formula = model_formula(),
    distribution = "gaussian",
    data = model_frame(train_data),
    verbose = FALSE
  )
}

# 学習済みGBMモデルで新しいデータの年俸を予測します。
predict_model <- function(model, new_data) {
  as.numeric(stats::predict(model, newdata = predictor_frame(new_data), n.trees = model$n.trees))
}

# GBM固有の変数重要度を抽出し、共通形式へ整えます。
model_importance <- function(model, train_data) {
  importance <- suppressWarnings(summary(model, plotit = FALSE))

  tibble(
    variable = importance$var,
    gbm_relative_influence = importance$rel.inf
  ) %>%
    right_join(tibble(variable = EXPLANATORY_VARIABLES_PER9), by = "variable") %>%
    mutate(
      gbm_relative_influence = tidyr_free_na_to_zero(.data[["gbm_relative_influence"]]),
      gbm_relative_influence_standardized =
        standardize_importance(.data[["gbm_relative_influence"]])
    )
}

# 定義したモデル関数を共通パイプラインに渡し、学習・予測・CSV出力を実行します。
run_model_pipeline("gbm", fit_model, predict_model, model_importance)
