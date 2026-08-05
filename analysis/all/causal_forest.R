library("dplyr")
data <- read_csv("/Users/skysky/Desktop/Motooka-2026-/analysis/all/data.csv")

#key変数が混入するので消去
data <- data |>
  select(-any_of("...1"))

set.seed(123)

Y <- data$salary
D <- data$npb_dummy
X <- select(data, -salary, -npb_dummy, -relief_dummy)
x <- model.matrix( ~ 0 + ., X)

model <- grf::causal_forest(
  Y = Y,
  X = x,
  W = D
)


hist(model$predictions)
grf::average_treatment_effect(model)
#   estimate     std.err 
# 0.039811662 0.007781717

grf::average_treatment_effect(
  model,
  subset = data$relief_dummy == 0
)

# 先発はマイナス(MLB所属者のほうが年俸高い)
#   estimate     std.err 
# -0.12792174  0.01753618 


grf::average_treatment_effect(
  model,
  subset = data$relief_dummy == 1
)
# リリーフはプラスで、効果が大きい(NPB所属者のほうが年俸高い)
# estimate    std.err 
# 0.12996253 0.00717695 

#傾向スコアの分布
hist(
  model$W.hat,
  breaks = 30,
  main = "推定傾向スコアの分布",
  xlab = "NPB所属確率"
)

#傾向スコアの分位点確認
propensity_check <- data.frame(
  npb_dummy = D,
  W_hat = model$W.hat
)

propensity_check |>
  group_by(npb_dummy) |>
  summarise(
    n = n(),
    mean_W_hat = mean(W_hat),
    min_W_hat = min(W_hat),
    median_W_hat = median(W_hat),
    max_W_hat = max(W_hat)
  )

#傾向スコアの構成要素確認
propensity_model <- grf::regression_forest(
  X = x,
  Y = D
)

importance <- data.frame(
  variable = colnames(x),
  importance = grf::variable_importance(propensity_model)
) |>
  arrange(desc(importance))

head(importance, 20)