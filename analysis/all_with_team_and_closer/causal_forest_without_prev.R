library("dplyr")
data <- read.csv("/Users/skysky/Desktop/Motooka-2026-/analysis/all_with_team_and_closer/data_without_prev.csv")

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

#BLP(grfのbest_linear_projectionを使用)

#両方
Z_3 <- data |> 
  select(-any_of("continuous_lag1")) |> 
  transmute( age_centered = age - mean(age, na.rm = TRUE), 
             relief_dummy = relief_dummy ) |>
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
# (Intercept)                0.0739297  0.0169942  4.3503 1.371e-05 ***
#  age_centered               0.0293365  0.0031670  9.2631 < 2.2e-16 ***
#  relief_dummy               0.1705214  0.0191615  8.8992 < 2.2e-16 ***
#  closer_dummy               0.8518869  0.0789687 10.7877 < 2.2e-16 ***
#  FA_dummy                  -0.1573513  0.0356658 -4.4118 1.034e-05 ***
#  strikeouts_per_9_centered -0.0010235  0.0028969 -0.3533  0.723870    
#walks_per_9_centered      -0.0031019  0.0016413 -1.8900  0.058788 .  
#home_runs_per_9_centered  -0.0042544  0.0038582 -1.1027  0.270175    
#wins_centered              4.8491697  0.6786997  7.1448 9.533e-13 ***
#  era_centered              -0.0027069  0.0010116 -2.6759  0.007462 **
  
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

# Estimate  Std. Error t value  Pr(>|t|)    
# (Intercept)               -0.08871796  0.05518136 -1.6078  0.107916    
# age_centered               0.02902849  0.00314481  9.2306 < 2.2e-16 ***
#  relief_dummy               0.16482177  0.01960604  8.4067 < 2.2e-16 ***
#  closer_dummy               0.84885615  0.07866311 10.7910 < 2.2e-16 ***
#  FA_dummy                  -0.15889138  0.03559897 -4.4634 8.141e-06 ***
#  strikeouts_per_9_centered -0.00198569  0.00289307 -0.6864  0.492499    
#walks_per_9_centered      -0.00310166  0.00163176 -1.9008  0.057352 .  
#home_runs_per_9_centered  -0.00453127  0.00384031 -1.1799  0.238054    
#wins_centered              4.72244619  0.67808555  6.9644 3.470e-12 ***
#  era_centered              -0.00263966  0.00099827 -2.6442  0.008198 ** 
#  team_wp                    0.17116755  0.10921095  1.5673  0.117068    
# team_payroll               0.08189889  0.03379378  2.4235  0.015387 * 
  
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

#model



#cobalt
#Balance tally for mean differences
#count
#Balanced, <0.1        8
#Not Balanced, >0.1    17
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
