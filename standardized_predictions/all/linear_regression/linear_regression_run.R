# 処理内容:
# - 標準化済みデータに対して線形回帰モデルを実行します。
# - 共通パイプライン側で MLB train / NPB train の2モデルを作成し、
#   同一リーグtestとクロスリーグfullに予測します。
# - モデル固有重要度として係数、標準化係数、p値を出力します。

# このスクリプト自身が置かれているディレクトリを返します。
# どの作業ディレクトリからRscriptを実行しても、隣接する _common を読めるようにします。
script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)))
  }

  getwd()
}

# 共通の前処理・分割・評価・出力関数を読み込みます。
source(file.path(dirname(script_dir()), "_common", "standardized_model_helpers.R"))

# stats::lm() により、salaryを目的変数、EXPLANATORY_VARIABLES_PER9を説明変数とする線形回帰を推定します。
fit_model <- function(train_data) {
  stats::lm(model_formula(), data = model_frame(train_data))
}

# 学習済みlmモデルを使い、新しいデータのsalaryを予測します。
predict_model <- function(model, new_data) {
  as.numeric(stats::predict(model, newdata = predictor_frame(new_data)))
}

# 線形回帰のモデル固有重要度を作ります。
# 係数そのもの、標準化係数、t検定のp値を説明変数ごとに保存します。
model_importance <- function(model, train_data) {
  coefficients <- stats::coef(model)
  coefficient_table <- summary(model)$coefficients
  p_values <- rep(NA_real_, length(EXPLANATORY_VARIABLES_PER9))
  names(p_values) <- EXPLANATORY_VARIABLES_PER9
  matched <- intersect(EXPLANATORY_VARIABLES_PER9, rownames(coefficient_table))
  p_values[matched] <- coefficient_table[matched, "Pr(>|t|)"]

  tibble(
    variable = EXPLANATORY_VARIABLES_PER9,
    coefficient = as.numeric(coefficients[EXPLANATORY_VARIABLES_PER9]),
    standardized_coefficient = standardized_coefficients(coefficients, train_data),
    p_value = as.numeric(p_values[EXPLANATORY_VARIABLES_PER9])
  )
}

# 共通パイプラインを実行し、予測結果・評価指標・変数重要度CSVを出力します。
run_model_pipeline("linear_regression", fit_model, predict_model, model_importance)
