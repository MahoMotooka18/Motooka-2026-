# 処理内容:
# - standardized_predictions 配下の3つの予測セットを順番に実行します。
# - 各セットで、同じ10種類のモデルスクリプトを Rscript で起動します。
# - どこか1つでも失敗した場合は、そのスクリプト名を出して処理を停止します。

# 役割別の予測セットです。
# all は全投手、starting_pitchers は先発のみ、relief は救援のみを対象にします。
prediction_sets <- c(
  "all",
  "starting_pitchers",
  "relief"
)

# 各予測セットで実行するモデルの順序です。
# ridge / elastic_net は標準化版では対象外です。
models <- c(
  "linear_regression",
  "lasso",
  "regression_tree",
  "svm_regression",
  "bagging",
  "random_forest",
  "adaboost",
  "gbm",
  "xgboost",
  "lightgbm"
)

# 予測セット x モデルの全組み合わせを順番に実行します。
# 各 *_run.R は自分のディレクトリを基準に共通ヘルパーを読み込むため、
# 同じコードでも all / starting_pitchers / relief ごとに異なる入力CSVと出力先になります。
for (prediction_set in prediction_sets) {
  for (model in models) {
    script <- file.path(
      "standardized_predictions",
      prediction_set,
      model,
      paste0(model, "_run.R")
    )
    message("RUN ", script)
    status <- system2("Rscript", script)

    # Rscript の終了ステータスが0でない場合、その場で全体処理を止めます。
    if (!identical(status, 0L)) {
      stop("failed: ", script, call. = FALSE)
    }
  }
}
