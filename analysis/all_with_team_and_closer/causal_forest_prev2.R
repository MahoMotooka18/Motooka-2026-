library("dplyr")
data <- read.csv("/Users/skysky/Desktop/Motooka-2026-/analysis/all_with_team_and_closer/data_prev2.csv")

set.seed(135)
View(data)
#key変数が混入するので消去
data <- data |>
  select(-any_of(c("X", "missing_lag2_avg","available_lag2_n", "continuous_until_last_available_lag2"))) |>
  filter(continuous_lag2 == 1)

View(data)

Y <- data$salary
D <- data$npb_dummy
X <- select(data, -salary, -npb_dummy, -continuous_lag2)
x <- model.matrix( ~ 0 + ., X)

model <- grf::causal_forest(
  Y = Y,
  X = x,
  W = D
)

grf::average_treatment_effect(model)
#全体効果は正
# estimate     std.err 
# 0.16728279 0.01369609

grf::average_treatment_effect(
  model,
  subset = data$closer_dummy == 0 & data$relief_dummy == 0
)

# 先発は正だが効果小さい(有意じゃないかも)
# estimate    std.err 
# -0.02073955  0.02652222
# -0.02381496  0.02861769

grf::average_treatment_effect(
  model,
  subset = data$closer_dummy == 0 & data$relief_dummy == 1
)
#中継ぎは正で、効果が大きい(NPB所属者のほうが年俸高い)
# estimate     std.err 
# 0.206561351 0.009039825

grf::average_treatment_effect(
  model,
  subset = data$closer_dummy == 1 & data$relief_dummy == 0
)
#抑えは正で、さらに効果が大きい(NPB所属者のほうが年俸高い)
# estimate    std.err 
# 0.71651238 0.08564617

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

# Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)               -0.3615672  0.0994584 -3.6354 0.0002797 ***
#  age_centered               0.0283722  0.0048837  5.8096 6.553e-09 ***
#  relief_dummy               0.1945330  0.0320612  6.0676 1.370e-09 ***
#  closer_dummy               0.7219667  0.0915696  7.8844 3.670e-15 ***
#  FA_dummy                  -0.1561727  0.0433158 -3.6054 0.0003139 ***
#  strikeouts_per_9_centered -0.0171201  0.0060009 -2.8529 0.0043456 ** 
#  walks_per_9_centered       0.0028117  0.0045478  0.6182 0.5364356    
# home_runs_per_9_centered   0.0197610  0.0092933  2.1264 0.0335084 *  
#  wins_centered             -1.1536185  0.9108842 -1.2665 0.2053849    
# era_centered              -0.0045092  0.0024338 -1.8527 0.0639681 .  
# team_wp                    0.5564893  0.1840333  3.0239 0.0025053 ** 
# team_payroll               0.1193596  0.0518286  2.3030 0.0213112 * 
  
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
#Balanced, <0.1        13
#Not Balanced, >0.1    32
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