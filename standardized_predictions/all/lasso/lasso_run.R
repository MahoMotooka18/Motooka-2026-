# 処理内容:
# - glmnet::cv.glmnet() によるLASSO回帰を実行します。
# - alpha = 1 に固定し、lambdaはcv.glmnetのlambda.minを使って予測します。
# - モデル固有重要度として係数と標準化係数を出力します。

# スクリプト自身のディレクトリを返し、相対パスで _common を読み込めるようにします。
script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)))
  }

  getwd()
}

# 共通処理と、glmnet用の model_matrix() などを読み込みます。
source(file.path(dirname(script_dir()), "_common", "standardized_model_helpers.R"))
ensure_package("glmnet")

# LASSO回帰をクロスバリデーション付きで推定します。
# チューニングはglmnetのデフォルトに任せ、alphaのみLASSOを意味する1に固定します。
fit_model <- function(train_data) {
  glmnet::cv.glmnet(
    x = model_matrix(train_data),
    y = train_data[[DEPENDENT_VARIABLE]],
    alpha = 1
  )
}

# lambda.min の係数を使って予測します。
predict_model <- function(model, new_data) {
  as.numeric(stats::predict(model, newx = model_matrix(new_data), s = "lambda.min"))
}

# LASSOのモデル固有重要度として、lambda.minでの係数と標準化係数を保存します。
model_importance <- function(model, train_data) {
  coefficients <- as.matrix(stats::coef(model, s = "lambda.min"))[, 1]

  tibble(
    variable = EXPLANATORY_VARIABLES_PER9,
    coefficient = as.numeric(coefficients[EXPLANATORY_VARIABLES_PER9]),
    standardized_coefficient = standardized_coefficients(coefficients, train_data)
  )
}

# 共通パイプラインを実行します。
run_model_pipeline("lasso", fit_model, predict_model, model_importance)
