setwd("/Users/skysky/Desktop/Motooka-2026-/analysis")
library("dplyr")
library("tidyr")

#使用する説明変数の抽出
EXPLANATORY_VARIABLES <- c(
  "age",
  "lefty",
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
  "balks",
  "npb_dummy",
  "salary",
  "relief_dummy"
)

#1試合あたりに変換する変数
VARIABLES_TO_DIVIDE <- c(
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

#MLB
MLB <- read.csv("/Users/skysky/Desktop/Motooka-2026-/data/processed/standardized_mlb/standardized_mlb_pitchers.csv")
MLB <- MLB |> #欠損36件
    mutate(npb_dummy = 0) |>
    mutate(across(all_of(VARIABLES_TO_DIVIDE), ~ .x / 162)) |>
    select(all_of(EXPLANATORY_VARIABLES)) |>
    drop_na()
nrow(MLB) #8158
MLB |>
  filter(if_any(everything(), is.na)) |>
  View()

#NPB #欠損5件(防御率無限)
NPB <- read.csv("/Users/skysky/Desktop/Motooka-2026-/data/processed/standardized_npb/standardized_by_all_average/standardized_npb_pitchers_all.csv")
View(NPB)
NPB <- NPB |>
  mutate(npb_dummy = 1,
         age = as.integer(sub("歳", "", age)) - 1) |>
  mutate(across(all_of(VARIABLES_TO_DIVIDE), ~ .x / 143)) |>
  select(all_of(EXPLANATORY_VARIABLES)) |>
  drop_na()
nrow(NPB) #4543

Data <- bind_rows(MLB, NPB)
sum(is.na(Data)) #0
nrow(Data) #12701
View(Data)
write.csv(Data, "/Users/skysky/Desktop/Motooka-2026-/analysis/all/data.csv")