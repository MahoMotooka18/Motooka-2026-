CURRENT_STAT_VARIABLES <- c(
  "games",
  "games_started",
  "games_finished",
  "complete_games",
  "shutouts",
  "saves",
  "holds",
  "innings_pitched",
  "batters_faced",
  "wins",
  "losses",
  "era",
  "hits_per_9",
  "home_runs_per_9",
  "strikeouts_per_9",
  "walks_per_9",
  "intentional_walks",
  "hit_by_pitch",
  "wild_pitches",
  "balks"
)

CURRENT_EXPLANATORY_VARIABLES <- c(
  "age",
  "lefty",
  "npb_dummy",
  "salary",
  CURRENT_STAT_VARIABLES,
  "relief_dummy",
  "closer_dummy",
  "FA_dummy",
  "team_wp",
  "team_payroll"
)

# 現在のコードで1試合あたりに変換している変数。
CURRENT_VARIABLES_TO_DIVIDE <- c(
  "games",
  "games_started",
  "games_finished",
  "complete_games",
  "shutouts",
  "saves",
  "holds",
  "innings_pitched",
  "batters_faced",
  "wins",
  "losses",
  "intentional_walks",
  "hit_by_pitch",
  "wild_pitches",
  "balks"
)

RELATED_DUMMY_VARIABLES <- list(
  prev1 = c(
    "missing_lag1",
    "continuous_lag1"
  ),
  prev2 = c(
    "missing_lag2_avg",
    "available_lag2_n",
    "continuous_lag2",
    "continuous_until_last_available_lag2"
  ),
  prev3 = c(
    "missing_lag3_avg",
    "available_lag3_n",
    "continuous_lag3",
    "continuous_until_last_available_lag3"
  )
)

PERIOD_NAMES <- names(RELATED_DUMMY_VARIABLES)

# 各データは現在の変数に、該当期間の過去年成績と関連ダミーだけを加える。
EXPLANATORY_VARIABLES <- setNames(
  lapply(PERIOD_NAMES, function(period) {
    c(
      CURRENT_EXPLANATORY_VARIABLES,
      paste0(period, "_", CURRENT_STAT_VARIABLES),
      RELATED_DUMMY_VARIABLES[[period]]
    )
  }),
  PERIOD_NAMES
)

# 現在成績と、該当期間の過去年成績を同じ方法で1試合あたりに変換する。
VARIABLES_TO_DIVIDE <- setNames(
  lapply(PERIOD_NAMES, function(period) {
    c(
      CURRENT_VARIABLES_TO_DIVIDE,
      paste0(period, "_", CURRENT_VARIABLES_TO_DIVIDE)
    )
  }),
  PERIOD_NAMES
)

validate_required_columns <- function(df, columns, league, period) {
  missing_columns <- setdiff(columns, names(df))
  if (length(missing_columns) > 0) {
    stop(
      league, "の", period, "用入力に必要な列がありません: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
}

create_period_data <- function(mlb_raw, npb_raw, period) {
  explanatory_variables <- EXPLANATORY_VARIABLES[[period]]
  variables_to_divide <- VARIABLES_TO_DIVIDE[[period]]

  input_variables <- setdiff(explanatory_variables, "npb_dummy")
  validate_required_columns(mlb_raw, input_variables, "MLB", period)
  validate_required_columns(npb_raw, input_variables, "NPB", period)

  MLB <- mlb_raw |>
    mutate(npb_dummy = 0) |>
    mutate(across(all_of(variables_to_divide), ~ .x / 162)) |>
    select(all_of(explanatory_variables)) |>
    drop_na()

  NPB <- npb_raw |>
    mutate(
      npb_dummy = 1,
      age = as.integer(sub("歳", "", age)) - 1
    ) |>
    mutate(across(all_of(variables_to_divide), ~ .x / 143)) |>
    select(all_of(explanatory_variables)) |>
    drop_na()

  bind_rows(MLB, NPB)
}

# 過去年成績・履歴関連ダミーを含まない、従来と同じ構造のデータを作る。
create_without_prev_data <- function(mlb_raw, npb_raw) {
  input_variables <- setdiff(CURRENT_EXPLANATORY_VARIABLES, "npb_dummy")
  validate_required_columns(mlb_raw, input_variables, "MLB", "without_prev")
  validate_required_columns(npb_raw, input_variables, "NPB", "without_prev")

  MLB <- mlb_raw |>
    mutate(npb_dummy = 0) |>
    mutate(across(all_of(CURRENT_VARIABLES_TO_DIVIDE), ~ .x / 162)) |>
    select(all_of(CURRENT_EXPLANATORY_VARIABLES)) |>
    drop_na()

  NPB <- npb_raw |>
    mutate(
      npb_dummy = 1,
      age = as.integer(sub("歳", "", age)) - 1
    ) |>
    mutate(across(all_of(CURRENT_VARIABLES_TO_DIVIDE), ~ .x / 143)) |>
    select(all_of(CURRENT_EXPLANATORY_VARIABLES)) |>
    drop_na()

  bind_rows(MLB, NPB)
}

MLB_RAW <- read.csv(INPUT_FILES[["mlb"]], check.names = FALSE)
NPB_RAW <- read.csv(INPUT_FILES[["npb"]], check.names = FALSE)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

Data_without_prev <- create_without_prev_data(MLB_RAW, NPB_RAW)
WITHOUT_PREV_OUTPUT_FILE <- file.path(OUTPUT_DIR, "data_without_prev.csv")
write.csv(Data_without_prev, WITHOUT_PREV_OUTPUT_FILE, row.names = FALSE)
message(
  "without_prev: ", nrow(Data_without_prev), " rows, ",
  ncol(Data_without_prev), " columns -> ", WITHOUT_PREV_OUTPUT_FILE
)

DATA_BY_PERIOD <- setNames(
  lapply(PERIOD_NAMES, function(period) {
    data <- create_period_data(MLB_RAW, NPB_RAW, period)
    output_file <- file.path(OUTPUT_DIR, paste0("data_", period, ".csv"))
    write.csv(data, output_file, row.names = FALSE)

    message(
      period, ": ", nrow(data), " rows, ", ncol(data),
      " columns -> ", output_file
    )
    data
  }),
  PERIOD_NAMES
)

# 対話実行時に確認しやすいよう、各期間のデータを個別名でも保持する。
Data_prev1 <- DATA_BY_PERIOD[["prev1"]]
Data_prev2 <- DATA_BY_PERIOD[["prev2"]]
Data_prev3 <- DATA_BY_PERIOD[["prev3"]]
