# 処理内容:
# - 先発投手向けに線形回帰の学習・予測関数を定義します。
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

# 線形回帰モデルを学習データに当てはめます。
fit_model <- function(train_data) {
  stats::lm(model_formula(), data = model_frame(train_data))
}

# 学習済み線形回帰モデルで新しいデータの年俸を予測します。
predict_model <- function(model, new_data) {
  as.numeric(stats::predict(model, newdata = predictor_frame(new_data)))
}

# 線形回帰固有の変数重要度を抽出し、共通形式へ整えます。
model_importance <- function(model, train_data) {
  coefficients <- stats::coef(model)
  coefficient_table <- summary(model)$coefficients

  tibble(
    variable = EXPLANATORY_VARIABLES_PER9,
    coefficient = as.numeric(coefficients[EXPLANATORY_VARIABLES_PER9]),
    standardized_coefficient = standardized_coefficients(coefficients, train_data),
    p_value = as.numeric(coefficient_table[EXPLANATORY_VARIABLES_PER9, "Pr(>|t|)"])
  )
}

# 定義したモデル関数を共通パイプラインに渡し、学習・予測・CSV出力を実行します。
run_model_pipeline("linear_regression", fit_model, predict_model, model_importance)
