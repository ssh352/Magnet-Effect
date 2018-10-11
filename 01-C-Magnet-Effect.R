library(tidyverse)
library(lubridate)
library(zoo)
library(parallel)
library(cnquant)

# Initialize ----
# 到基础股票数据的路径
# path_to_basic <- "~/Documents/Stock-Data"
path_to_basic <- "~/einzbern/Stock-Data"
# 读取基础股票数据
load(paste0(path_to_basic, "/D-Stock-Daily-Data.RData"))

# 最终样本期间为对应有Tick数据的时间
start_date <- "20141010"  # Tick数据开始的时间
end_date <- "20181010"  # Tick数据结束的时间

# 并行运算使用核心数量
ncl <- 20L
socket_type <- "PSOCK"

# 使用哪些板的股票
# board_codes <- c("000", "001", "600", "601", "603")  # only主板
board_codes <- c("000", "001", "600", "601", "603", "002", "300")  # 全部
# 分别到主板和中小创业板数据的路径
# path_to_mainboard <- "/mnt/sdc2/WIND_DATA/ID_BT_SHARES_A"
path_to_mainboard <- "G:/WIND_DATA/ID_BT_SHARES_A"
path_to_sme <- "H:/WIND_DATA/ID_BT_SHARES_S"
path_to_ge <- "H:/WIND_DATA/ID_BT_SHARES_G"

# 涨跌幅阈值
threshold <- 0.09
thresholds <- seq(0.09, 0.099, by = 0.001)


# # 留存一份初始数据（标号为0#的） ----
# # 注意这里直接筛选到了最终样本期，而另一部分数据因为需要滚动计算开始时比样本期更长
# Stock_Daily_Data0 <- Stock_Daily_Data %>% 
#   # 需要的变量
#   select(S_INFO_WINDCODE:S_DQ_CLOSE, S_DQ_PCTCHANGE, S_DQ_TRADESTATUS, UP_DOWN_LIMIT_STATUS, ST) %>% 
#   # 样本期
#   filter(TRADE_DT >= start_date) %>% 
#   filter(TRADE_DT <= end_date) %>% 
#   # 使用哪些板的股票
#   filter(substr(S_INFO_WINDCODE, 1L, 3L) %in% board_codes)
# 
# Stock_Daily_Data01 <- Stock_Daily_Data0 %>% 
#   # 非ST
#   filter(!ST) %>% 
#   # 正常交易状态
#   filter(S_DQ_TRADESTATUS %in% c("交易", "DR", "XD", "XR")) %>% 
#   # 去掉最高价超过涨停价的有问题的样本点，20141009之后只有一个20151218复牌的000520，2块到20块
#   filter(S_DQ_HIGH <= round(S_DQ_PRECLOSE * 1.1 + .Machine$double.eps ^ 0.5, 2))


# 初筛样本，需要的变量和时间区间 ----
Stock_Daily_Data <- Stock_Daily_Data %>% 
  # 需要的变量
  select(S_INFO_WINDCODE:S_DQ_CLOSE, S_DQ_PCTCHANGE, S_DQ_TRADESTATUS, 
         UP_DOWN_LIMIT_STATUS, ST) %>% 
  # 样本期，因为需要滚动窗口，这里比Tick样本期间多取6个月
  filter(TRADE_DT >= format(start_date %>% ymd() %m-% months(13), "%Y%m%d")) %>% 
  filter(TRADE_DT <= end_date) %>% 
  # 使用哪些板的股票
  filter(substr(S_INFO_WINDCODE, 1L, 3L) %in% board_codes)


# 生成需要变量 ----
Stock_Daily_Data <- Stock_Daily_Data %>% 
  # 计算涨跌阈值和涨跌停价格，及是否达到
  mutate(UP_LIMIT = round(S_DQ_PRECLOSE * 1.1 + .Machine$double.eps ^ 0.5, 2), 
         DOWN_LIMIT = round(S_DQ_PRECLOSE * 0.9 + .Machine$double.eps ^ 0.5, 2), 
         UP_THRESHOLD = S_DQ_PRECLOSE * (1 + threshold), 
         DOWN_THRESHOLD = S_DQ_PRECLOSE * (1 - threshold), 
         REACH_UP_LIMIT = S_DQ_HIGH == UP_LIMIT, 
         REACH_DOWN_LIMIT = S_DQ_LOW == DOWN_LIMIT, 
         REACH_UP_THRESHOLD = S_DQ_HIGH >= UP_THRESHOLD, 
         REACH_DOWN_THRESHOLD = S_DQ_LOW <= DOWN_THRESHOLD)


# 日度数据滚动估计每日的参数mu和sigma ----
# 估计完后调整到标准样本期
# 估计均值和标准差的函数
mu_sigma <- function(x) c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))

system.time(
Stock_Daily_Data <- Stock_Daily_Data %>% 
  # 生成对数收益率
  mutate(Ri = log(1 + S_DQ_PCTCHANGE / 100)) %>% 
  # rolling window用对数收益率的均值和标准差估计几何布朗运动的参数mu和sigma
  group_by(S_INFO_WINDCODE) %>% 
  do(data.frame(
    ., 
    rollapply(.$Ri, list(-260:-21), mu_sigma, fill = NA), 
    stringsAsFactors = FALSE
  )) %>% 
  ungroup() %>% 
  select(-Ri, -starts_with("rollapply")) %>% 
  rename(mu = X1, sigma = X2) %>% 
  mutate(mu = mu + 1 / 2 * sigma ^ 2) %>% 
  # 滚动窗口计算完之后只保留有Tick数据的期间
  filter(TRADE_DT >= start_date)
)


# # 添加前一天的Kline数据路径 ----
# Stock_Daily_Data1 <- Stock_Daily_Data %>% 
#   mutate(kline_path_lag1 = case_when(
#     substr(S_INFO_WINDCODE, 1L, 3L) %in% c("000", "001") ~ 
#       paste0(path_to_mainboard, "/KLine/SZ/", TRADE_DT, "/", substr(S_INFO_WINDCODE, 1L, 6L), ".csv"), 
#     substr(S_INFO_WINDCODE, 1L, 3L) %in% c("600", "601", "603") ~ 
#       paste0(path_to_mainboard, "/KLine/SH/", TRADE_DT, "/", substr(S_INFO_WINDCODE, 1L, 6L), ".csv"), 
#     substr(S_INFO_WINDCODE, 1L, 3L) %in% c("002") ~ 
#       paste0(path_to_sme, "/KLine/SZ/", TRADE_DT, "/", substr(S_INFO_WINDCODE, 1L, 6L), ".csv"), 
#     substr(S_INFO_WINDCODE, 1L, 3L) %in% c("300") ~ 
#       paste0(path_to_ge, "/KLine/SZ/", TRADE_DT, "/", substr(S_INFO_WINDCODE, 1L, 6L), ".csv")
#   )) %>% 
#   # 类似的也可以用前一天的高频数据估计试试，生成前一天的路径，因为要lag日期，所以放在filter之前
#   group_by(S_INFO_WINDCODE) %>% 
#   mutate(kline_path_lag1 = lag(kline_path_lag1)) %>% 
#   ungroup()
Stock_Daily_Data1 <- Stock_Daily_Data


# 二筛样本，非ST等破坏个股连续时间样本的要求 ----
Stock_Daily_Data1 <- Stock_Daily_Data1 %>% 
  # 非ST
  filter(!ST) %>% 
  # 正常交易状态
  filter(S_DQ_TRADESTATUS %in% c("交易", "DR", "XD", "XR")) %>% 
  # 去掉最高价超过涨停价的有问题的样本点，20141009之后只有一个20151218复牌的000520，2块到20块
  filter(S_DQ_HIGH <= UP_LIMIT)


# 三筛样本，最大绝对涨幅超过threshold（如9%）的样本点 ----
Stock_Daily_Data2 <- Stock_Daily_Data1 %>% 
  # 只需保留过阈值的交易日
  filter(REACH_UP_THRESHOLD | REACH_DOWN_THRESHOLD) %>% 
  # 对涨跌分别去掉开盘时就超过阈值的
  filter(!(S_DQ_OPEN == UP_LIMIT & S_DQ_LOW > DOWN_THRESHOLD), 
         !(S_DQ_OPEN == DOWN_LIMIT & S_DQ_HIGH < UP_THRESHOLD))


# # 日内数据估计每日的参数mu和sigma（an alternative way） ----
# # 从日内Kline数据估计每日的参数mu和sigma的函数
# mu_sigma_intraday <- function(path) {
#   # path为日内数据文件路径
#   tryCatch({
#     path %>% 
#       read_csv(col_types = cols_only(close = col_integer())) %>% 
#       with(mu_sigma(log(close / lag(close))))
#   }, error = function(e) c(NA, NA))
# }
# 
# system.time(
# Stock_Daily_Data2 <- Stock_Daily_Data2 %>% 
#   # 计算
#   data.frame(
#     t(sapply(.$kline_path_lag1, mu_sigma_intraday, USE.NAMES = FALSE)), 
#     stringsAsFactors = FALSE
#   ) %>% 
#   as_tibble() %>% 
#   select(-kline_path_lag1) %>% 
#   rename(mu_hf = X1, sigma_hf = X2) %>% 
#   mutate(sigma_hf = sigma_hf * sqrt(240), 
#          mu_hf = mu_hf * 240 + 1 / 2 * sigma_hf ^ 2)
# )


# Tick数据确定涨跌超过阈值时间点 ----
# 从Tick文件中匹配第一次超过特定价格的时间的函数（并行）
# 添加需要的变量
Stock_Daily_Data2 <- Stock_Daily_Data2 %>% 
  # 生成tick文件路径
  mutate(tick_path = case_when(
    substr(S_INFO_WINDCODE, 1L, 3L) %in% c("000", "001") ~ 
      paste0(path_to_mainboard, "/Tick/SZ/", TRADE_DT, "/", substr(S_INFO_WINDCODE, 1L, 6L), ".csv"), 
    substr(S_INFO_WINDCODE, 1L, 3L) %in% c("600", "601", "603") ~ 
      paste0(path_to_mainboard, "/Tick/SH/", TRADE_DT, "/", substr(S_INFO_WINDCODE, 1L, 6L), ".csv"), 
    substr(S_INFO_WINDCODE, 1L, 3L) %in% c("002") ~ 
      paste0(path_to_sme, "/Tick/SZ/", TRADE_DT, "/", substr(S_INFO_WINDCODE, 1L, 6L), ".csv"), 
    substr(S_INFO_WINDCODE, 1L, 3L) %in% c("300") ~ 
      paste0(path_to_ge, "/Tick/SZ/", TRADE_DT, "/", substr(S_INFO_WINDCODE, 1L, 6L), ".csv")
  ))

# 将数据集分割成涨跌两部分
Stock_Daily_Data3 <- list(
  UP = filter(Stock_Daily_Data2, REACH_UP_THRESHOLD), 
  DOWN = filter(Stock_Daily_Data2, REACH_DOWN_THRESHOLD)
)

# 生成阈值序列
Thresholds <- list(
  UP = sapply(thresholds, 
              function(threshold) 
                as.integer(Stock_Daily_Data3$UP$S_DQ_PRECLOSE * (1 + threshold) * 10000)), 
  DOWN = sapply(-thresholds, 
                function(threshold) 
                  as.integer(Stock_Daily_Data3$DOWN$S_DQ_PRECLOSE * (1 + threshold) * 10000))
)

# 从Tick文件中匹配超过阈值的时间点的函数
# 输入为index，输出为integer vector
match_time_up <- function(index) {
  tryCatch({
    Stock_Daily_Data3$UP$tick_path[index] %>% 
      read_csv(col_types = cols_only(time = col_integer(), 
                                     high = col_integer())) %>% 
      with(time[sapply(Thresholds$UP[index, ], function(threshold) detect_index(high, ~ . >= threshold)) %>% replace_certain(0L, NA_integer_)])
  }, error = function(e) rep.int(NA_integer_, length(thresholds)))
}
match_time_down <- function(index) {
  tryCatch({
    Stock_Daily_Data3$DOWN$tick_path[index] %>% 
      read_csv(col_types = cols_only(time = col_integer(), 
                                     low = col_integer())) %>% 
      with(time[sapply(Thresholds$DOWN[index, ], function(threshold) detect_index(low, ~ . > 0 & . <= threshold)) %>% replace_certain(0L, NA_integer_)])
  }, error = function(e) rep.int(NA_integer_, length(thresholds)))
}

# start cluster
cl <- makeCluster(ncl, type = socket_type)
clusterEvalQ(cl, {
  library(tidyverse)
  library(cnquant)
})
clusterExport(cl, c(
  "Stock_Daily_Data3", 
  "Thresholds", 
  "thresholds"
))

# 主要计算，匹配时间，返回值为integer matrix
system.time({
  t1 <- list(
    UP = parSapply(cl, seq_len(nrow(Stock_Daily_Data3$UP)), match_time_up) %>% t(), 
    DOWN = parSapply(cl, seq_len(nrow(Stock_Daily_Data3$DOWN)), match_time_down) %>% t()
  )
})


# MCMC涨停概率 ----
# tick时间映射到[0, 14400s]的函数
ticktime2second <- function(ticktime) {
  ticktime = parse_tick_time(ticktime)
  ticktime = if_else(ticktime <= parse_time("113000", format = "%H%M%S"), 
                     as.integer(ticktime - parse_time("093000", format = "%H%M%S")), 
                     as.integer(ticktime - parse_time("130000", format = "%H%M%S")) + 7200L)
  return(ticktime)
}

# 时间变为单位秒
t1 <- t1 %>% 
  lapply(function(x) apply(x, 2, ticktime2second))
# 由t1推出T
T <- t1 %>% 
  lapply(function(x) (14400L - x) / 14400L)
# N
N <- t1 %>% 
  lapply(function(x) round((14400L - x) / 3))
# Thresholds_Mod，每个threshold到涨跌停的增长比例
Thresholds_Mod <- list(
  UP = Thresholds$UP %>% 
    apply(2, function(x) Stock_Daily_Data3$UP$UP_LIMIT * 10000 / x), 
  DOWN = Thresholds$DOWN %>% 
    apply(2, function(x) Stock_Daily_Data3$DOWN$DOWN_LIMIT * 10000 / x)
)

# 计算触及涨停板概率的函数
prob_reach_up <- function(index) {
  tryCatch({
    trajectories <- Sim.DiffProc::GBM(N = N$UP[index, 1], 
                                      M = 1000, 
                                      T = T$UP[index, 1], 
                                      theta = Stock_Daily_Data3$UP$mu[index], 
                                      sigma = Stock_Daily_Data3$UP$sigma[index])
    probs <- rep.int(NA, length(thresholds))
    for (i in seq_along(probs)) {
      probs[i] <- trajectories[time(trajectories) <= T$UP[index, i], ] %>% 
        apply(2, max) %>% 
        {mean(. >= Thresholds_Mod$UP[index, i])}
    }
    probs
  }, error = function(e) rep.int(NA, length(thresholds)))
}
prob_reach_down <- function(index) {
  tryCatch({
    trajectories <- Sim.DiffProc::GBM(N = N$DOWN[index, 1], 
                                      M = 1000, 
                                      T = T$DOWN[index, 1], 
                                      theta = Stock_Daily_Data3$DOWN$mu[index], 
                                      sigma = Stock_Daily_Data3$DOWN$sigma[index])
    probs <- rep.int(NA, length(thresholds))
    for (i in seq_along(probs)) {
      probs[i] <- trajectories[time(trajectories) <= T$DOWN[index, i], ] %>% 
        apply(2, min) %>% 
        {mean(. <= Thresholds_Mod$DOWN[index, i])}
    }
    probs
  }, error = function(e) rep.int(NA, length(thresholds)))
}

# clusterExport
clusterExport(cl, c(
  "N", 
  "T", 
  "Thresholds_Mod"
))

set.seed(4L)

# 主要计算，达到各个threshold后达到涨跌停的概率，返回值为numeric matrix
system.time({
  Probs_Theo <- list(
    UP = parSapply(cl, seq_len(nrow(Stock_Daily_Data3$UP)), prob_reach_up) %>% t(), 
    DOWN = parSapply(cl, seq_len(nrow(Stock_Daily_Data3$DOWN)), prob_reach_down) %>% t()
  )
})

# 结束并行cluster
stopCluster(cl)

# 保存数据
save.image("data/01-D-Sequential-Data.RData")
