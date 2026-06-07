# 処理内容:
# - lightgbm::lgb.train() によるLightGBM回帰を実行します。
# - objective = "regression"、metric = "l2"、nrounds = 100 に固定しています。
# - モデル固有重要度としてGain, Cover, Frequencyを出力します。

# スクリプト位置を取得します。
script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)))
  }

  getwd()
}

# 共通処理とlightgbmパッケージを準備します。
source(file.path(dirname(script_dir()), "_common", "standardized_model_helpers.R"))
ensure_package("lightgbm")

# LightGBM用のDatasetを作り、回帰モデルを推定します。
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

# 学習済みLightGBMモデルで予測します。
predict_model <- function(model, new_data) {
  as.numeric(stats::predict(model, model_matrix(new_data)))
}

# lgb.importance() からGain, Cover, Frequencyを取得し、全説明変数の表に揃えます。
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

# 共通パイプラインを実行します。
run_model_pipeline("lightgbm", fit_model, predict_model, model_importance)
