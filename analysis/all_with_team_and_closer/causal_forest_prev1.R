library("dplyr")
data <- read.csv("/Users/skysky/Desktop/Motooka-2026-/analysis/all_with_team_and_closer/data_prev1.csv")

set.seed(135)
View(data)
#key変数が混入するので消去
data <- data |>
  select(-any_of(c("X", "missing_lag1"))) |>
  filter(continuous_lag1 == 1)

View(data)

Y <- data$salary
D <- data$npb_dummy
X <- select(data, -salary, -npb_dummy, -continuous_lag1)
x <- model.matrix( ~ 0 + ., X)

model <- grf::causal_forest(
  Y = Y,
  X = x,
  W = D
)



grf::average_treatment_effect(model)
#全体効果は正
# estimate    std.err 
# 0.20345332 0.01107086 

grf::average_treatment_effect(
  model,
  subset = data$closer_dummy == 0 & data$relief_dummy == 0
)

# 先発は正だが効果小さい(有意じゃないかも)
# estimate    std.err 
# 0.08979307 0.02303959 

grf::average_treatment_effect(
  model,
  subset = data$closer_dummy == 0 & data$relief_dummy == 1
)
#中継ぎは正で、効果が大きい(NPB所属者のほうが年俸高い)
# estimate     std.err 
# 0.194243623 0.007323214 

grf::average_treatment_effect(
  model,
  subset = data$closer_dummy == 1 & data$relief_dummy == 0
)
#抑えは正で、さらに効果が大きい(NPB所属者のほうが年俸高い)
# estimate    std.err 
# 0.83085307 0.08129528

# tau(X)の分布
hist(model$predictions)

#BLP(grfのbest_linear_projectionを使用)

#両方
Z_3 <- data |> 
  select(-any_of("continuous_lag1")) |> 
  transmute( age_centered = age - mean(age, na.rm = TRUE), 
             relief_dummy = relief_dummy,
             closer_dummy = closer_dummy ) |>
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
# Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)                0.0522321  0.0206435  2.5302  0.011417 *  
#  age_centered               0.0201549  0.0031696  6.3588 2.133e-10 ***
#  relief_dummy               0.1657840  0.0236572  7.0078 2.598e-12 ***
#  closer_dummy               0.7739190  0.0859216  9.0073 < 2.2e-16 ***
#  strikeouts_per_9_centered -0.0092587  0.0044298 -2.0901  0.036637 *  
#  walks_per_9_centered      -0.0035814  0.0031241 -1.1464  0.251679    
#. home_runs_per_9_centered  -0.0010766  0.0055953 -0.1924  0.847422    
#. wins_centered              2.3028347  0.7828211  2.9417  0.003272 ** 
#  era_centered              -0.0030464  0.0015914 -1.9143  0.055612 . 
  
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
# (Intercept)               -0.2352918  0.0802877 -2.9306  0.003392 ** 
#  age_centered               0.0299973  0.0038908  7.7099 1.395e-14 ***
#  relief_dummy               0.1493609  0.0240857  6.2012 5.852e-10 ***
#  closer_dummy               0.7605792  0.0849387  8.9545 < 2.2e-16 ***
#  FA_dummy                  -0.1804630  0.0389217 -4.6366 3.593e-06 ***
#  strikeouts_per_9_centered -0.0107765  0.0043919 -2.4537  0.014157 *  
#  walks_per_9_centered      -0.0022831  0.0031678 -0.7207  0.471102    
# home_runs_per_9_centered   0.0018958  0.0054070  0.3506  0.725888    
#wins_centered              1.9406509  0.7738944  2.5076  0.012172 *  
#  era_centered              -0.0024932  0.0015853 -1.5726  0.115837    
# team_wp                    0.4854016  0.1494860  3.2471  0.001170 ** 
#  team_payroll               0.0933717  0.0437398  2.1347  0.032812 *
  
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

#cobalt
#Balance tally for mean differences
#count
#Balanced, <0.1        17
#Not Balanced, >0.1    28
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
