# 処理内容:
# - 救援投手向けのLasso回帰モデル実行スクリプトです。
# - 同じディレクトリのモデル本体を読み込み、共通パイプラインの実行を開始します。

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
source(file.path(script_dir(), "lasso_run.R"))
