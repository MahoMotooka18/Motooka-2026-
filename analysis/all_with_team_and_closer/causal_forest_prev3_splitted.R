library("dplyr")
data <- read.csv("/Users/skysky/Desktop/Motooka-2026-/analysis/all_with_team_and_closer/data_prev3.csv")

set.seed(135)
View(data)
#key変数が混入するので消去
data <- data |>
  select(-any_of("X"))

View(data)

Y <- data$salary
D <- data$npb_dummy
X <- select(data, -salary, -npb_dummy)
x <- model.matrix( ~ 0 + ., X)

model <- grf::causal_forest(
  Y = Y,
  X = x,
  W = D
)



grf::average_treatment_effect(model)
#全体効果は正
# estimate     std.err 
# 0.19748391 0.00877485 

grf::average_treatment_effect(
  model,
  subset = data$closer_dummy == 0 & data$relief_dummy == 0
)

# 先発は正
# estimate    std.err 
# 0.14950349 0.01247828 
grf::average_treatment_effect(
  model,
  subset = data$closer_dummy == 0 & data$relief_dummy == 1
)
#中継ぎは正で、効果が大きい(NPB所属者のほうが年俸高い)
# estimate     std.err 
# 0.175910704 0.005873602 

grf::average_treatment_effect(
  model,
  subset = data$closer_dummy == 1 & data$relief_dummy == 0
)
#抑えは正で、さらに効果が大きい(NPB所属者のほうが年俸高い)
# estimate    std.err 
# 0.93707714 0.07625733

# tau(X)の分布
hist(model$predictions)

starter_subset <- data$closer_dummy == 0 &
  data$relief_dummy == 0

relief_subset <- data$closer_dummy == 0 &
  data$relief_dummy == 1

closer_subset <- data$closer_dummy == 1 &
  data$relief_dummy == 0

#先発
# Starter 内の平均で center
Z_stats_with_team_ave_starter <- data |>
  mutate(
    age_centered =
      age - mean(age[starter_subset], na.rm = TRUE),
    
    strikeouts_per_9_centered =
      strikeouts_per_9 -
      mean(strikeouts_per_9[starter_subset], na.rm = TRUE),
    
    walks_per_9_centered =
      walks_per_9 -
      mean(walks_per_9[starter_subset], na.rm = TRUE),
    
    home_runs_per_9_centered =
      home_runs_per_9 -
      mean(home_runs_per_9[starter_subset], na.rm = TRUE),
    
    wins_centered =
      wins - mean(wins[starter_subset], na.rm = TRUE),
    
    era_centered =
      era - mean(era[starter_subset], na.rm = TRUE),
    
    team_wp_centered =
      team_wp - mean(team_wp[starter_subset], na.rm = TRUE),
    
    team_payroll_centered =
      team_payroll -
      mean(team_payroll[starter_subset], na.rm = TRUE)
  ) |>
  select(
    age_centered,
    FA_dummy,
    strikeouts_per_9_centered,
    walks_per_9_centered,
    home_runs_per_9_centered,
    wins_centered,
    era_centered,
    team_wp_centered,
    team_payroll_centered
  ) |>
  data.matrix()


BLP_stats_with_team_ave_starter <- grf::best_linear_projection(
  model,
  A = Z_stats_with_team_ave_starter,
  subset = starter_subset
)

BLP_stats_with_team_ave_starter

# Relief の定義
# Relief 内の平均で center
Z_stats_with_team_ave_relief <- data |>
  mutate(
    age_centered =
      age - mean(age[relief_subset], na.rm = TRUE),
    
    strikeouts_per_9_centered =
      strikeouts_per_9 -
      mean(strikeouts_per_9[relief_subset], na.rm = TRUE),
    
    walks_per_9_centered =
      walks_per_9 -
      mean(walks_per_9[relief_subset], na.rm = TRUE),
    
    home_runs_per_9_centered =
      home_runs_per_9 -
      mean(home_runs_per_9[relief_subset], na.rm = TRUE),
    
    holds_centered =
      holds - mean(holds[relief_subset], na.rm = TRUE),
    
    era_centered =
      era - mean(era[relief_subset], na.rm = TRUE),
    
    team_wp_centered =
      team_wp - mean(team_wp[relief_subset], na.rm = TRUE),
    
    team_payroll_centered =
      team_payroll -
      mean(team_payroll[relief_subset], na.rm = TRUE)
  ) |>
  select(
    age_centered,
    FA_dummy,
    strikeouts_per_9_centered,
    walks_per_9_centered,
    home_runs_per_9_centered,
    holds_centered,
    era_centered,
    team_wp_centered,
    team_payroll_centered
  ) |>
  data.matrix()


BLP_stats_with_team_ave_relief <- grf::best_linear_projection(
  model,
  A = Z_stats_with_team_ave_relief,
  subset = relief_subset
)

BLP_stats_with_team_ave_relief

# Closer の定義

# Closer 内の平均で center
Z_stats_with_team_ave_closer <- data |>
  mutate(
    age_centered =
      age - mean(age[closer_subset], na.rm = TRUE),
    
    strikeouts_per_9_centered =
      strikeouts_per_9 -
      mean(strikeouts_per_9[closer_subset], na.rm = TRUE),
    
    walks_per_9_centered =
      walks_per_9 -
      mean(walks_per_9[closer_subset], na.rm = TRUE),
    
    home_runs_per_9_centered =
      home_runs_per_9 -
      mean(home_runs_per_9[closer_subset], na.rm = TRUE),
    
    saves_centered =
      saves - mean(saves[closer_subset], na.rm = TRUE),
    
    era_centered =
      era - mean(era[closer_subset], na.rm = TRUE),
    
    team_wp_centered =
      team_wp - mean(team_wp[closer_subset], na.rm = TRUE),
    
    team_payroll_centered =
      team_payroll -
      mean(team_payroll[closer_subset], na.rm = TRUE)
  ) |>
  select(
    age_centered,
    FA_dummy,
    strikeouts_per_9_centered,
    walks_per_9_centered,
    home_runs_per_9_centered,
    saves_centered,
    era_centered,
    team_wp_centered,
    team_payroll_centered
  ) |>
  data.matrix()


BLP_stats_with_team_ave_closer <- grf::best_linear_projection(
  model,
  A = Z_stats_with_team_ave_closer,
  subset = closer_subset
)

BLP_stats_with_team_ave_closer
