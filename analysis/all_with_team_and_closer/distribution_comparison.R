library(tidyverse)
data <- read.csv("/Users/skysky/Desktop/Motooka-2026-/analysis/all_standardized_by_fielders_salary_with_closer/data_without_prev.csv")
View(data)
# ------------------------------------------------------------
# 1. NPB / MLB × Relief / Non-relief の記述統計
# ------------------------------------------------------------

summary_stats <- data %>%
  filter(
    npb_dummy %in% c(0, 1),
    relief_dummy %in% c(0, 1),
    !is.na(salary)
  ) %>%
  mutate(
    League = if_else(npb_dummy == 1, "NPB", "MLB"),
    Group = case_when(
      relief_dummy == 1 ~ "Relief",
      closer_dummy == 1 ~ "Closer",
      TRUE ~ "Starter"
    )
  ) %>%
  group_by(Group, League) %>%
  summarise(
    Mean = mean(salary, na.rm = TRUE),
    Median = median(salary, na.rm = TRUE),
    SD = sd(salary, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = League,
    values_from = c(Mean, Median, SD),
    names_glue = "{League}_{.value}"
  )


# ------------------------------------------------------------
# 2. 2標本 Kolmogorov-Smirnov 検定
#    各 Relief group について NPB vs MLB を比較
# ------------------------------------------------------------

ks_results <- data %>%
  filter(
    npb_dummy %in% c(0, 1),
    relief_dummy %in% c(0, 1),
    !is.na(salary)
  ) %>%
  group_by(relief_dummy) %>%
  summarise(
    KS_D = as.numeric(
      ks.test(
        salary[npb_dummy == 1],
        salary[npb_dummy == 0],
        exact = FALSE
      )$statistic
    ),
    
    KS_p_value = ks.test(
      salary[npb_dummy == 1],
      salary[npb_dummy == 0],
      exact = FALSE
    )$p.value,
    
    .groups = "drop"
  ) %>%
  mutate(
    Group = case_when(
      relief_dummy == 1 ~ "Relief",
      closer_dummy == 1 ~ "Closer",
      TRUE ~ "Starter"
    )
  ) %>%
  select(-relief_dummy)


# ------------------------------------------------------------
# 3. 記述統計とKS検定を結合
# ------------------------------------------------------------

result_table <- summary_stats %>%
  left_join(
    ks_results,
    by = "Group"
  ) %>%
  select(
    Group,
    NPB_Mean,
    MLB_Mean,
    NPB_Median,
    MLB_Median,
    NPB_SD,
    MLB_SD,
    KS_D,
    KS_p_value
  ) %>%
  mutate(
    across(
      c(
        NPB_Mean,
        MLB_Mean,
        NPB_Median,
        MLB_Median,
        NPB_SD,
        MLB_SD,
        KS_D
      ),
      ~ round(.x, 3)
    ),
    
    #KS_p_value = format.pval(
      #KS_p_value,
      #digits = 3,
      #eps = 2.2e-16
    #)
  )


# ------------------------------------------------------------
# 4. 表を表示
# ------------------------------------------------------------

result_table