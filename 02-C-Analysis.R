library(tidyverse)
library(lubridate)
library(readxl)
library(zoo)
library(cnquant)
library(stargazer)
load("data/01-D-Sequential-Data.RData")
load("~/Documents/Stock-Data/D-Stock-Daily-Data.RData")

# Valid indices ----
# 开盘的价格不能超过阈值
Nonopen_Valid_Indices <- list(
  UP = sapply(thresholds, function(threshold) 
    with(Stock_Daily_Data3$UP, S_DQ_OPEN < S_DQ_PRECLOSE * (1 + threshold))), 
  DOWN = sapply(-thresholds, function(threshold) 
    with(Stock_Daily_Data3$DOWN, S_DQ_OPEN > S_DQ_PRECLOSE * (1 + threshold)))
)

# # Valid N
# N_Valid_Indices <- N %>% 
#   lapply(function(x) apply(x, 2, function(y) between(y, 2L, 4798L)))

# Probs_Theo valid indices
Probs_Theo_Valid_Indices <- Probs_Theo %>% 
  lapply(function(x) !is.na(x))

# Aggregate valid indices
Valid_Indices <- list(
  UP = Nonopen_Valid_Indices$UP & Probs_Theo_Valid_Indices$UP,
  DOWN = Nonopen_Valid_Indices$DOWN & Probs_Theo_Valid_Indices$DOWN
)

# 有效样本数量
Valid_Thre_Count <- Valid_Indices %>% 
  sapply(function(x) apply(x, 2, sum)) %>% 
  t() %>% 
  `colnames<-`(scales::percent(thresholds, accuracy = .1)) %>% 
  `rownames<-`(c("Upper", "Lower")) %>% 
  as_tibble(rownames = "Direction")

## 输出有效样本数量
Valid_Thre_Count


# Baseline results ----
# 保留需要的变量
Stock_Daily_Data3$UP <- Stock_Daily_Data3$UP %>% mutate(reach_prac = REACH_UP_LIMIT)
Stock_Daily_Data3$DOWN <- Stock_Daily_Data3$DOWN %>% mutate(reach_prac = REACH_DOWN_LIMIT)
Stock_Daily_Data3 <- Stock_Daily_Data3 %>% 
  lapply(select, S_INFO_WINDCODE, TRADE_DT, reach_prac)

# 把理论概率结果合并入原数据
Core_Data <- list(
  UP = Stock_Daily_Data3$UP %>% 
    mutate(prob_theo = Probs_Theo$UP[, 1]) %>% 
    filter(Valid_Indices$UP[, 1]), 
  DOWN = Stock_Daily_Data3$DOWN %>% 
    mutate(prob_theo = Probs_Theo$DOWN[, 1]) %>% 
    filter(Valid_Indices$DOWN[, 1])
)
# 生成一个总表
Core_Data$ALL <- bind_rows(Core_Data, .id = "Direction")

# 真实概率与理论概率
Prac_Vs_Theo <- Core_Data %>% 
  lapply(function(df) {
    df %>% 
      summarise(Prob_Prac = mean(reach_prac), 
                Prob_Theo = mean(prob_theo), 
                Diff = Prob_Prac - Prob_Theo, 
                t.stat = t.test(reach_prac, prob_theo, paired = TRUE)$statistic, 
                p.value = t.test(reach_prac, prob_theo, paired = TRUE)$p.value, 
                N = n())
  }) %>% 
  bind_rows(.id = "Direction")

## 调整格式并输出Prac_Vs_Theo
Prac_Vs_Theo <- Prac_Vs_Theo %>% 
  mutate(Direction = factor(Direction, levels = c("ALL", "UP", "DOWN"), 
                            labels = c("All", "Upper", "Lower"))) %>% 
  arrange(Direction) %>% 
  rename(`Prac. Prob.` = Prob_Prac, 
         `Theo. Prob.` = Prob_Theo, 
         Diff. = Diff, 
         `T Stat.` = t.stat, 
         `P Value` = p.value)
Prac_Vs_Theo


# Compare at different thresholds ----
# 实际概率
probs_prac <- list(
  UP = apply(Valid_Indices$UP, 2, function(x) 
    mean(Stock_Daily_Data3$UP$reach_prac[x])), 
  DOWN = apply(Valid_Indices$DOWN, 2, function(x) 
    mean(Stock_Daily_Data3$DOWN$reach_prac[x]))
)

# 理论概率
Probs_Theo_bak <- Probs_Theo
Probs_Theo$UP[!Valid_Indices$UP] <- NA
Probs_Theo$DOWN[!Valid_Indices$DOWN] <- NA
probs_theo <- Probs_Theo %>% 
  lapply(function(x) apply(x, 2, mean, na.rm = TRUE))

# exploratory plots
plot(probs_prac$UP, type = "b", col = "red")
lines(probs_theo$UP, type = "b", col = "cyan")

plot(probs_prac$DOWN, type = "b", col = "red")
lines(probs_theo$DOWN, type = "b", col = "cyan")

# explanatory plot
Diff_Thre <- list(prob_prac = probs_prac, prob_theo = probs_theo) %>% 
  lapply(simplify2array) %>% 
  lapply(as_tibble) %>% 
  lapply(mutate, threshold = thresholds) %>% 
  bind_rows(.id = "prob_type") %>% 
  gather(direction, prob, UP, DOWN) %>% 
  mutate(direction = factor(direction, levels = c("UP", "DOWN"), labels = c("Upper", "Lower")))

p_Diff_Thre <- Diff_Thre %>% 
  ggplot(aes(x = threshold, y = prob, col = prob_type)) + 
  geom_line(alpha = 0.5) + 
  geom_point(size = 2) + 
  scale_x_continuous("Starting threshold", 
                     breaks = thresholds, 
                     labels = scales::percent_format(accuracy = .1)) + 
  scale_y_continuous("Probability to reach price limit", 
                     limits = c(NA, 1), 
                     labels = scales::percent) + 
  scale_colour_discrete("Type of probablity", labels = c("Practical", "Theoretical")) + 
  facet_wrap(~ direction, ncol = 1L)

# 输出p_Diff_Thre
p_Diff_Thre


# Hard to value ----
# 生成各种hard to value指标
Hard_Data <- Stock_Daily_Data %>% 
  select(S_INFO_WINDCODE, TRADE_DT, S_DQ_AMOUNT, S_VAL_MV, S_VAL_PE_TTM, 
         S_DQ_FREETURNOVER, S_PRICE_DIV_DPS) %>% 
  filter(TRADE_DT >= format(start_date %>% ymd() %m-% months(6), "%Y%m%d"), 
         TRADE_DT <= end_date) %>% 
  # 将无交易的0变为NA，否则log会变成-Inf
  mutate_at(vars(S_DQ_AMOUNT, S_DQ_FREETURNOVER), 
            replace_certain, pattern = 0, replacement = NA) %>% 
  # 取log
  mutate_at(vars(S_DQ_AMOUNT:S_PRICE_DIV_DPS), log) %>% 
  # 取平均
  group_by(S_INFO_WINDCODE) %>% 
  mutate_at(vars(S_DQ_AMOUNT:S_PRICE_DIV_DPS), rollapply,
            width = list(-60:-1), FUN = mean, na.rm = TRUE, fill = NA) %>%
  ungroup() %>% 
  # 只保留样本期
  filter(TRADE_DT >= start_date) %>% 
  # 根据股息率是否缺失生成是否发放股息变量
  mutate(NON_DIV = is.na(S_PRICE_DIV_DPS))

# # 上市时间
# conn <- wind_sql_connect()
# Hard_Data <- tbl(conn, "AShareDescription") %>% 
#   select(S_INFO_WINDCODE, S_INFO_LISTDATE) %>% 
#   collect() %>% 
#   left_join(Hard_Data, .) %>% 
#   mutate(AGE_LIST = ymd(TRADE_DT) - ymd(S_INFO_LISTDATE))
# Hard_Data <- tbl(conn, "AShareIntroduction") %>% 
#   select(S_INFO_WINDCODE, S_INFO_FOUNDDATE) %>% 
#   collect() %>% 
#   left_join(Hard_Data, .) %>% 
#   mutate(AGE_FOUND = ymd(TRADE_DT) - ymd(S_INFO_FOUNDDATE))
# DBI::dbDisconnect(conn)

# # 对hard to value指标在每个交易日分组
# Hard_Data <- Hard_Data %>%
#   group_by(TRADE_DT) %>%
#   mutate_at(vars(S_DQ_AMOUNT:S_PRICE_DIV_DPS), divide, n_group = 5L) %>%
#   ungroup()

# 合并hard to value数据
Core_Data_Plus_Hard <- Core_Data %>% 
  lapply(left_join, Hard_Data)

# 测试hard to value分组指标的函数
test_group_var <- function(group, data = Core_Data_Plus_Hard, direction = "ALL") {
  group <- sym(group)
  data[[direction]] %>%
    group_by(!! group) %>%
    summarise(Prob_Prac = mean(reach_prac),
              Prob_Theo = mean(prob_theo),
              Diff = Prob_Prac - Prob_Theo,
              t.stat = t.test(reach_prac, prob_theo, paired = TRUE)$statistic, 
              p.value = t.test(reach_prac, prob_theo, paired = TRUE)$p.value,
              N = n()) %>%
    return()
}

# 分组变量
hard_vars <- Hard_Data %>%
  select(S_DQ_AMOUNT:NON_DIV) %>%
  names()

# # 分组结果
# hard_vars %>%
#   lapply(test_group_var)

# glm结果
glmfit_S_VAL_MV1 <- glm(
  reach_prac ~ prob_theo + S_VAL_MV, 
  family = binomial, 
  data = Core_Data_Plus_Hard$ALL
)
glmfit_S_VAL_MV2 <- glm(
  reach_prac ~ prob_theo + Direction + S_VAL_MV, 
  family = binomial, 
  data = Core_Data_Plus_Hard$ALL
)
glmfit_S_VAL_PE_TTM1 <- glm(
  reach_prac ~ prob_theo + S_VAL_PE_TTM, 
  family = binomial, 
  data = Core_Data_Plus_Hard$ALL
)
glmfit_S_VAL_PE_TTM2 <- glm(
  reach_prac ~ prob_theo + Direction + S_VAL_PE_TTM, 
  family = binomial, 
  data = Core_Data_Plus_Hard$ALL
)
glmfit_NON_DIV1 <- glm(
  reach_prac ~ prob_theo + NON_DIV, 
  family = binomial, 
  data = Core_Data_Plus_Hard$ALL
)
glmfit_NON_DIV2 <- glm(
  reach_prac ~ prob_theo + Direction + NON_DIV, 
  family = binomial, 
  data = Core_Data_Plus_Hard$ALL
)
glmfit_S_PRICE_DIV_DPS1 <- glm(
  reach_prac ~ prob_theo + S_PRICE_DIV_DPS, 
  family = binomial, 
  data = Core_Data_Plus_Hard$ALL
)
glmfit_S_PRICE_DIV_DPS2 <- glm(
  reach_prac ~ prob_theo + Direction + S_PRICE_DIV_DPS, 
  family = binomial, 
  data = Core_Data_Plus_Hard$ALL
)

## 生成回归表格（需到时改为latex格式）
stargazer(glmfit_S_VAL_MV1, glmfit_S_VAL_MV2, glmfit_S_VAL_PE_TTM1, glmfit_S_VAL_PE_TTM2, 
          type = "text",
          title = "Excess Probability Reaching Price Limit with Hard-to-Value Firms", 
          table.placement = "H", 
          dep.var.labels = "Reaching Price Limit", 
          covariate.labels = c("Theoretical Probability", "Upper", "Market Value", "Price to Earnings"), 
          header = FALSE)
stargazer(glmfit_NON_DIV1, glmfit_NON_DIV2, glmfit_S_PRICE_DIV_DPS1, glmfit_S_PRICE_DIV_DPS2, 
          type = "text",
          title = "Excess Probability Reaching Price Limit with Hard-to-Value Firms (Continued)", 
          table.placement = "H", 
          dep.var.labels = "Reaching Price Limit", 
          covariate.labels = c("Theoretical Probability", "Upper", "None-Divident", "Price to Dividend"), 
          header = FALSE)


# 交易活跃程度 ----
glmfit_S_DQ_AMOUNT1 <- glm(
  reach_prac ~ prob_theo + S_DQ_AMOUNT, 
  family = binomial, 
  data = Core_Data_Plus_Hard$ALL
)
glmfit_S_DQ_AMOUNT2 <- glm(
  reach_prac ~ prob_theo + Direction + S_DQ_AMOUNT, 
  family = binomial, 
  data = Core_Data_Plus_Hard$ALL
)
glmfit_S_DQ_FREETURNOVER1 <- glm(
  reach_prac ~ prob_theo + S_DQ_FREETURNOVER, 
  family = binomial, 
  data = Core_Data_Plus_Hard$ALL
)
glmfit_S_DQ_FREETURNOVER2 <- glm(
  reach_prac ~ prob_theo + Direction + S_DQ_FREETURNOVER, 
  family = binomial, 
  data = Core_Data_Plus_Hard$ALL
)

## 生成回归表格
stargazer(glmfit_S_DQ_AMOUNT1, glmfit_S_DQ_AMOUNT2, 
          glmfit_S_DQ_FREETURNOVER1, glmfit_S_DQ_FREETURNOVER2, 
          type = "text",
          title = "Excess Probability Reaching Price Limit with Trading-Energetic Firms", 
          table.placement = "H", 
          dep.var.labels = "Reaching Price Limit", 
          covariate.labels = c("Theoretical Probability", "Upper", "Trading Volume", "Turnover"), 
          header = FALSE)


# # Investor sentiment ----
# # 读取ISI数据
# ISI_Data <- read_csmar_data("data/ISI综合情绪指数表/QX_ISI.xls") %>% 
#   arrange(SgnMonth) %>% 
#   mutate(YEARMON = str_remove(SgnMonth, "-")) %>% 
#   filter(YEARMON >= "201410", YEARMON <= "201810") %>% 
#   mutate(ISI = divide(ISI, n_group = 3L)) %>%
#   mutate(ISI = factor(ISI, levels = 3:1, labels = c("High", "Medium", "Low"))) %>%
#   select(YEARMON, ISI)
# 
# # 合并ISI数据
# Core_Data_Plus_Hard <- Core_Data_Plus_Hard %>% 
#   lapply(mutate, YEARMON = substr(TRADE_DT, 1L, 6L)) %>% 
#   lapply(left_join, ISI_Data)
# 
# # ISI results
# test_group_var("ISI", direction = "UP")
# test_group_var("ISI", direction = "DOWN")
# test_group_var("ISI")
# 
# # glm
# glm(
#   reach_prac ~ prob_theo + ISI, 
#   family = binomial, 
#   data = Core_Data_Plus_Hard$DOWN
# ) %>% 
#   summary()
# 
# # CICSI
# CICSI_Data <- read_csmar_data("data/CICSI综合情绪指数表/QX_CICSI.xls") %>% 
#   arrange(SgnMonth) %>% 
#   mutate(YEARMON = str_remove(SgnMonth, "-")) %>% 
#   filter(YEARMON >= "201410", YEARMON <= "201810") %>% 
#   mutate(CICSI = divide(CICSI, n_group = 3L)) %>%
#   mutate(CICSI = factor(CICSI, levels = 3:1, labels = c("High", "Medium", "Low"))) %>%
#   select(YEARMON, CICSI)
# Core_Data_Plus_Hard <- Core_Data_Plus_Hard %>% 
#   lapply(left_join, CICSI_Data)
# test_group_var("CICSI", direction = "UP")
# test_group_var("CICSI", direction = "DOWN")
# test_group_var("CICSI")
# glm(
#   reach_prac ~ prob_theo + CICSI, 
#   family = binomial, 
#   data = Core_Data_Plus_Hard$DOWN
# ) %>% 
#   summary()
# 
# 
# # Market state ----
# load("~/Documents/Stock-Data/D-Index-Daily-Data.RData")
# Market_State_Data <- Index_Daily_Data %>% 
#   filter(S_INFO_WINDCODE == "000001.SH") %>% 
#   filter(TRADE_DT >= "20140101") %>% 
#   transmute(TRADE_DT, Market_State = rollapply(S_DQ_PCTCHANGE, width = list(-120:-1), FUN = mean, na.rm = TRUE, fill = NA)) %>% 
#   mutate(Market_State = Market_State >= 0) %>% 
#   filter(TRADE_DT >= start_date)
# Core_Data_Plus_Hard <- Core_Data_Plus_Hard %>% 
#   lapply(left_join, Market_State_Data)
# glm(
#   reach_prac ~ prob_theo + Market_State, 
#   family = binomial, 
#   data = Core_Data_Plus_Hard$DOWN
# ) %>% 
#   summary()


# Trigger ----
# 按是否Trigger分为两类
Trigger_Data <- list(
  UP = Stock_Daily_Data3$UP %>% 
    mutate(prob_theo = Probs_Theo_bak$UP[, 1]) %>% 
    mutate(Trigger = Nonopen_Valid_Indices$UP[, 1]) %>% 
    filter(Probs_Theo_Valid_Indices$UP[, 1]), 
  DOWN = Stock_Daily_Data3$DOWN %>% 
    mutate(prob_theo = Probs_Theo_bak$DOWN[, 1]) %>% 
    mutate(Trigger = Nonopen_Valid_Indices$DOWN[, 1]) %>% 
    filter(Probs_Theo_Valid_Indices$DOWN[, 1])
)
Trigger_Data$ALL <- bind_rows(Trigger_Data, .id = "Direction")

## 输出Trigger结果
Trigger_Results_Table <- test_group_var("Trigger", data = Trigger_Data) %>% 
  mutate(Trigger = factor(Trigger, labels = c("No", "Yes"))) %>% 
  rename(`Prac. Prob.` = Prob_Prac, 
         `Theo. Prob.` = Prob_Theo, 
         Diff. = Diff, 
         `T Stat.` = t.stat, 
         `P Value` = p.value)
Trigger_Results_Table


# 机构持股比例 ----
# 下载机构持股数据
conn <- wind_sql_connect()
AShareinstHolderDerData <- tbl(conn, "AShareinstHolderDerData") %>%
  select(S_INFO_WINDCODE, REPORT_PERIOD, S_HOLDER_PCT) %>%
  collect()
DBI::dbDisconnect(conn)

# 合并出总的机构持股比例
Inst_Hold_Data <- AShareinstHolderDerData %>% 
  # 合并机构持股比例
  group_by(S_INFO_WINDCODE, REPORT_PERIOD) %>% 
  summarise(INST_HOLD = sum(S_HOLDER_PCT, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # 分组
  group_by(REPORT_PERIOD) %>%
  mutate(INST_HOLD = divide(INST_HOLD, n_group = 5)) %>%
  ungroup() %>% 
  # 生成季度时间
  mutate(REPORT_PERIOD = as.yearqtr(REPORT_PERIOD, format = "%Y%m%d"))
rm(AShareinstHolderDerData)

# 合并机构持股比例数据
Core_Data_Plus_Inst <- Core_Data %>% 
  lapply(mutate, REPORT_PERIOD = as.yearqtr(TRADE_DT, format = "%Y%m%d") - 0.25) %>% 
  lapply(left_join, Inst_Hold_Data)

## 机构持股比例结果
Inst_Results_Table <- test_group_var("INST_HOLD", data = Core_Data_Plus_Inst) %>% 
  na.omit() %>% 
  rename(`Inst. Hold.` = INST_HOLD, 
         `Prac. Prob.` = Prob_Prac, 
         `Theo. Prob.` = Prob_Theo, 
         Diff. = Diff, 
         `T Stat.` = t.stat, 
         `P Value` = p.value)
Inst_Results_Table

# # glm
# glm(
#   reach_prac ~ prob_theo + Direction + INST_HOLD, 
#   family = binomial, 
#   data = Core_Data_Plus_Inst$ALL
# ) %>% 
#   summary()


# 保存数据 ----
save.image("data/02-D-Analysis.RData")
save(Valid_Thre_Count, 
     Prac_Vs_Theo, 
     Trigger_Results_Table, 
     Inst_Results_Table, 
     list = grep("glmfit", ls(), value = TRUE), 
     file = "data/02-D-For-Rmd-Paper.RData")
