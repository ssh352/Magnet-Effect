library(tidyverse)
library(lubridate)
library(zoo)
library(parallel)
library(cnquant)
load("data/01-D-Sequential-Data.RData")
load("~/Documents/Stock-Data/D-Stock-Daily-Data.RData")

# Valid indices ----
# 开盘的价格不能超过阈值
Nonopen_Valid_Indices <- list(
  UP = sapply(thresholds, function(threshold) with(Stock_Daily_Data3$UP, S_DQ_OPEN < S_DQ_PRECLOSE * (1 + threshold))), 
  DOWN = sapply(-thresholds, function(threshold) with(Stock_Daily_Data3$DOWN, S_DQ_OPEN > S_DQ_PRECLOSE * (1 + threshold)))
)
Nonopen_Valid_Indices %>% 
  lapply(function(x) apply(x, 2, sum))

# Valid N
N_Valid_Indices <- N %>% 
  lapply(function(x) apply(x, 2, function(y) between(y, 2L, 4798L)))
N_Valid_Indices %>% 
  lapply(function(x) apply(x, 2, sum, na.rm = TRUE))

# Probs_Theo valid indices
Probs_Theo_Valid_Indices <- Probs_Theo %>% 
  lapply(function(x) !is.na(x))
Probs_Theo_Valid_Indices %>% 
  lapply(function(x) apply(x, 2, sum))

# Aggregate valid indices
Valid_Indices <- list(
  UP = Nonopen_Valid_Indices$UP & N_Valid_Indices$UP & Probs_Theo_Valid_Indices$UP,
  DOWN = Nonopen_Valid_Indices$DOWN & N_Valid_Indices$DOWN & Probs_Theo_Valid_Indices$DOWN
)
Valid_Indices <- list(
  UP = Nonopen_Valid_Indices$UP & Probs_Theo_Valid_Indices$UP,
  DOWN = Nonopen_Valid_Indices$DOWN & Probs_Theo_Valid_Indices$DOWN
)
Valid_Indices <- list(
  UP = Probs_Theo_Valid_Indices$UP,
  DOWN = Probs_Theo_Valid_Indices$DOWN
)
Valid_Indices %>% 
  lapply(function(x) apply(x, 2, sum))


# Compare at different thresholds ----
# 实际概率
probs_prac <- list(
  UP = apply(Valid_Indices$UP, 2, function(x) mean(Stock_Daily_Data3$UP$REACH_UP_LIMIT[x])), 
  DOWN = apply(Valid_Indices$DOWN, 2, function(x) mean(Stock_Daily_Data3$DOWN$REACH_DOWN_LIMIT[x]))
)

# 理论概率
Probs_Theo$UP[!Valid_Indices$UP] <- NA
Probs_Theo$DOWN[!Valid_Indices$DOWN] <- NA
probs_theo <- Probs_Theo %>% 
  lapply(function(x) apply(x, 2, mean, na.rm = TRUE))

# 做图对比
plot(probs_prac$UP, type = "b", col = "red")
lines(probs_theo$UP, type = "b", col = "cyan")

plot(probs_prac$DOWN, type = "b", col = "red")
lines(probs_theo$DOWN, type = "b", col = "cyan")


# Hard to value ----
# 上市时间
conn <- wind_sql_connect()
tbl(conn, "AShareDescription")

# 各种hard to value指标
Stock_Daily_Data <- Stock_Daily_Data %>% 
  select(S_INFO_WINDCODE, TRADE_DT, S_DQ_CLOSE, S_DQ_AMOUNT, S_VAL_MV, S_DQ_MV, 
         S_VAL_PE, S_VAL_PB_NEW, S_VAL_PE_TTM, S_DQ_TURN, S_DQ_FREETURNOVER)

# 待修改
Stock_Daily_Data2_UP2 <- Stock_Daily_Data2_UP %>% 
  mutate(Prob_UP = Probs$UP[, 1]) %>% 
  filter(!is.na(Prob_UP)) %>% 
  left_join(select(
    Stock_Daily_Data, S_INFO_WINDCODE, TRADE_DT, S_DQ_MV, S_VAL_PE, S_VAL_PB_NEW, S_VAL_PE_TTM, 
    S_DQ_FREETURNOVER)) %>% 
  mutate_at(vars(S_DQ_MV:S_DQ_FREETURNOVER), divide, n_group = 5L)

Stock_Daily_Data2_UP2 %>% 
  group_by(S_DQ_FREETURNOVER) %>% 
  summarise(mean(REACH_UP_LIMIT) - mean(Prob_UP))
