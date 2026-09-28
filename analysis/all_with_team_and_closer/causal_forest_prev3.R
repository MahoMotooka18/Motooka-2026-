library("dplyr")
data <- read.csv("/Users/skysky/Desktop/Motooka-2026-/analysis/all_with_team_and_closer/data_prev3.csv")

set.seed(135)
View(data)
#key変数が混入するので消去
data <- data |>
  select(-any_of(c("X", "missing_lag3_avg","available_lag3_n", "continuous_until_last_available_lag3"))) |>
  filter(continuous_lag3 == 1)

View(data)

Y <- data$salary
D <- data$npb_dummy
X <- select(data, -salary, -npb_dummy, -continuous_lag3)
x <- model.matrix( ~ 0 + ., X)

model <- grf::causal_forest(
  Y = Y,
  X = x,
  W = D
)



grf::average_treatment_effect(model)
#全体効果は正
# estimate     std.err 
# 0.11835357 0.01700648

grf::average_treatment_effect(
  model,
  subset = data$closer_dummy == 0 & data$relief_dummy == 0
)

# 先発は正だが効果小さい(有意じゃないかも)
# estimate    std.err 
# -0.13110717  0.03467801
# -0.12621238  0.03592192

grf::average_treatment_effect(
  model,
  subset = data$closer_dummy == 0 & data$relief_dummy == 1
)
#中継ぎは正で、効果が大きい(NPB所属者のほうが年俸高い)
# estimate     std.err 
# 0.18794170 0.01102706

grf::average_treatment_effect(
  model,
  subset = data$closer_dummy == 1 & data$relief_dummy == 0
)
#抑えは正で、さらに効果が大きい(NPB所属者のほうが年俸高い)
# estimate    std.err 
# 0.31865275 0.04075544 
# 0.65513409 0.09518731

# tau(X)の分布
hist(model$predictions)

#BLP(grfのbest_linear_projectionを使用)

#両方
Z_3 <- data |> 
  select(-any_of("continuous_lag1")) |> 
  transmute( age_centered = age - mean(age, na.rm = TRUE), 
             relief_dummy = relief_dummy,
             closer_dummy = closer_dummy) |>
  data.matrix()

# Best Linear Projection
BLP_age_and_relief <- grf::best_linear_projection(
  model,
  A = Z_3
)

BLP_age_and_relief

#.  Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)  0.1033334  0.0162446  6.3611 2.072e-10 ***
# age_centered 0.0241760  0.0022376 10.8046 < 2.2e-16 ***
# relief_dummy 0.1327944  0.0179259  7.4080 1.363e-13 ***


#全部のせ
Z_stats <- data |>
  mutate(
    across(
      c(age, strikeouts_per_9, walks_per_9, home_runs_per_9, wins, losses, era),
      ~ .x - mean(.x, na.rm = TRUE),
      .names = "{.col}_centered"
    ),
    relief_dummy = relief_dummy,
    closer_dummy = closer_dummy,
    npb_dummy = npb_dummy
  ) |>
  select(
    age_centered,
    relief_dummy,
    closer_dummy,
    FA_dummy,
    strikeouts_per_9_centered,
    walks_per_9_centered,
    home_runs_per_9_centered,
    wins_centered,
    era_centered
  ) |>
  data.matrix()

BLP_stats <- grf::best_linear_projection(
  model,
  A = Z_stats
)

BLP_stats

Z_stats_with_team <- data |>
  mutate(
    across(
      c(age, strikeouts_per_9, walks_per_9, home_runs_per_9, wins, losses, era),
      ~ .x - mean(.x, na.rm = TRUE),
      .names = "{.col}_centered"
    ),
    relief_dummy = relief_dummy,
    closer_dummy = closer_dummy,
    npb_dummy = npb_dummy
  ) |>
  select(
    age_centered,
    relief_dummy,
    closer_dummy,
    FA_dummy,
    strikeouts_per_9_centered,
    walks_per_9_centered,
    home_runs_per_9_centered,
    wins_centered,
    era_centered,
    team_wp,
    team_payroll
  ) |>
  data.matrix()


BLP_stats_with_team <- grf::best_linear_projection(
  model,
  A = Z_stats_with_team
)

BLP_stats_with_team

Z_stats_with_team_ave <- data |>
  mutate(
    across(
      c(age, strikeouts_per_9, walks_per_9, home_runs_per_9, 
        wins, losses, era, team_wp, team_payroll),
      ~ .x - mean(.x, na.rm = TRUE),
      .names = "{.col}_centered"
    ),
    relief_dummy = relief_dummy,
    closer_dummy = closer_dummy,
    npb_dummy = npb_dummy
  ) |>
  select(
    age_centered,
    relief_dummy,
    closer_dummy,
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


BLP_stats_with_team_ave <- grf::best_linear_projection(
  model,
  A = Z_stats_with_team_ave
)

BLP_stats_with_team_ave
#cobalt
#Balance tally for mean differences
#count
#Balanced, <0.1        12
#Not Balanced, >0.1    33
balance_raw <- cobalt::bal.tab(
  x = x,
  treat = D,
  stats = "mean.diffs",
  continuous = "raw",
  binary = "raw",
  disp = c("means", "sds"),
  un = TRUE,
  abs = FALSE,
  quick = FALSE
)

balance_raw

raw_difference_table <- balance_raw$Balance |>
  as.data.frame() |>
  rownames_to_column("variable") |>
  select(
    variable,
    Type,
    M.0.Un,
    SD.0.Un,
    M.1.Un,
    SD.1.Un,
    Diff.Un
  ) |>
  rename(
    mean_USA = M.0.Un,
    sd_USA = SD.0.Un,
    mean_Japan = M.1.Un,
    sd_Japan = SD.1.Un,
    Japan_minus_USA = Diff.Un
  )

raw_difference_table

#標準化平均差
balance_smd <- cobalt::bal.tab(
  x = X,
  treat = D,
  stats = c(
    "mean.diffs",
    "variance.ratios",
    "ks.statistics"
  ),
  continuous = "std",
  binary = "std",
  s.d.denom = "pooled",
  disp = c("means", "sds"),
  un = TRUE,
  thresholds = c(m = 0.1),
  abs = FALSE,
  quick = FALSE
)

balance_smd