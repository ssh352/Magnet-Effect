library(tidyverse)
library(lubridate)
library(zoo)
library(parallel)
options(mc.cores = 4L)
library(cnquant)
load("~/Documents/Stock-Data/D-Stock-Daily-Data.RData")

# 初筛样本，需要的变量和样本期，initialize ----
start_date <- "20141009"  # Tick数据开始的时间
end_date <- "20180330"  # Tick数据结束的时间
Stock_Daily_Data0 <- Stock_Daily_Data %>% 
  # 需要的变量
  select(S_INFO_WINDCODE:S_DQ_AMOUNT, S_DQ_TRADESTATUS, UP_DOWN_LIMIT_STATUS, ST) %>% 
  # 样本期
  filter(TRADE_DT >= start_date) %>% 
  filter(TRADE_DT <= end_date) %>% 
  # ! 目前只用主板数据
  filter(substr(S_INFO_WINDCODE, 1L, 3L) == "000" | substr(S_INFO_WINDCODE, 1L, 1L) == "6") %>% 
  # 非ST
  filter(!ST) %>% 
  # 正常交易状态
  filter(S_DQ_TRADESTATUS %in% c("交易", "DR", "XD", "XR")) %>% 
  # 去掉最高价超过涨停价的有问题的样本点，20141009之后只有一个20151218复牌的000520，2块到20块
  filter(S_DQ_HIGH <= S_DQ_PRECLOSE * 1.1)

Stock_Daily_Data <- Stock_Daily_Data %>% 
  # 需要的变量
  select(S_INFO_WINDCODE:S_DQ_AMOUNT, S_DQ_TRADESTATUS, UP_DOWN_LIMIT_STATUS, ST) %>% 
  # 样本期，因为需要滚动窗口，这里比Tick样本期间多一部分
  filter(TRADE_DT >= "20140309") %>% 
  # ! 目前只用主板数据
  filter(substr(S_INFO_WINDCODE, 1L, 3L) == "000" | substr(S_INFO_WINDCODE, 1L, 1L) == "6")

# 到红色5T含有主板数据移动硬盘下A股数据的路径
path_to_red_ashare <- "/mnt/sdc2/WIND_DATA/ID_BT_SHARES_A"

# 涨跌幅阈值
threshold <- 0.09


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
    rollapply(.$Ri, 120, mu_sigma, fill = NA), 
    stringsAsFactors = FALSE
  )) %>% 
  ungroup() %>% 
  select(-Ri, -rollapply...Ri..120..mu_sigma..fill...NA.) %>% 
  rename(mu = X1, sigma = X2) %>% 
  mutate(mu = mu + 1 / 2 * sigma ^ 2) %>% 
  # 滚动窗口计算完之后只保留有Tick数据的期间
  filter(TRADE_DT >= start_date) %>% 
  filter(TRADE_DT <= end_date)
)


# 二筛样本，最大绝对涨幅超过threshold（如9%）的样本点 ----
Stock_Daily_Data <- Stock_Daily_Data %>% 
  # 生成kline文件路径
  mutate(kline_path = if_else(
    substr(S_INFO_WINDCODE, 1L, 1L) == 6, 
    paste0(path_to_red_ashare, "/KLine/SH/", TRADE_DT, "/", substr(S_INFO_WINDCODE, 1L, 6L), ".csv"), 
    paste0(path_to_red_ashare, "/KLine/SZ/", TRADE_DT, "/", substr(S_INFO_WINDCODE, 1L, 6L), ".csv")
  )) %>% 
  # 类似的也可以用前一天的高频数据估计试试，生成前一天的路径，因为要lag日期，所以放在filter之前
  group_by(S_INFO_WINDCODE) %>% 
  mutate(kline_path_lag1 = lag(kline_path)) %>% 
  ungroup() %>% 
  # 只需保留过阈值的交易日
  filter(REACH_UP_THRESHOLD | REACH_DOWN_THRESHOLD) %>% 
  # 对涨跌分别去掉开盘时就超过阈值的
  filter(!(S_DQ_OPEN >= UP_THRESHOLD & S_DQ_LOW > DOWN_THRESHOLD), 
         !(S_DQ_OPEN <= DOWN_THRESHOLD & S_DQ_HIGH < UP_THRESHOLD)) %>% 
  # 非ST
  filter(!ST) %>% 
  # 正常交易状态
  filter(S_DQ_TRADESTATUS %in% c("交易", "DR", "XD", "XR")) %>% 
  # 去掉最高价超过涨停价的有问题的样本点，20141009之后只有一个20151218复牌的000520，2块到20块
  filter(S_DQ_HIGH <= UP_LIMIT)


# 日内数据估计每日的参数mu和sigma（an alternative way） ----
# 从日内Kline数据估计每日的参数mu和sigma的函数
mu_sigma_intraday <- function(path) {
  # path为日内数据文件路径
  tryCatch({
    path %>% 
      read_csv(col_types = cols_only(close = col_integer())) %>% 
      with(mu_sigma(log(close / lag(close))))
  }, error = function(e) c(NA, NA))
}

system.time(
Stock_Daily_Data <- Stock_Daily_Data %>% 
  # 计算
  data.frame(
    t(sapply(.$kline_path, mu_sigma_intraday, USE.NAMES = FALSE)), 
    stringsAsFactors = FALSE
  ) %>% 
  as_tibble() %>% 
  select(-kline_path, -kline_path_lag1) %>% 
  rename(mu_hf = X1, sigma_hf = X2) %>% 
  mutate(sigma_hf = sigma_hf * sqrt(240), 
         mu_hf = mu_hf * 240 + 1 / 2 * sigma_hf ^ 2)
)


# Tick数据确定涨跌超过阈值时间点 ----
# 从Tick文件中匹配第一次超过特定价格的时间的函数
match_time_up <- function(path, threshold) {
  # path为tick文件路径
  # threshold为涨跌整数价格阈值
  # 返回第一次价格超过阈值的时间
  tryCatch({
    path %>% 
      read_csv(col_types = cols_only(time = col_integer(), 
                                     high = col_integer())) %>% 
      with(time[detect_index(high, ~ . >= threshold)])
  }, error = function(e) NA)
}

match_time_down <- function(path, threshold) {
  # path为tick文件路径
  # threshold为涨跌整数价格阈值
  # 返回第一次价格超过阈值的时间
  tryCatch({
    path %>% 
      read_csv(col_types = cols_only(time = col_integer(), 
                                     low = col_integer())) %>% 
      with(time[detect_index(low, ~ . > 0 & . <= threshold)])
  }, error = function(e) NA)
}

# vectorize match_time
match_time_vector <- function(path, threshold, index, direction = "up") {
  t1 <- rep.int(NA_integer_, length(threshold))
  if (direction == "up") {
    t1[index] <- mcmapply(match_time_up, path[index], threshold[index], USE.NAMES = FALSE, mc.cores = 8L)
  } else {
    t1[index] <- mcmapply(match_time_down, path[index], threshold[index], USE.NAMES = FALSE, mc.cores = 8L)
  }
  return(t1)
}

# 添加需要的变量
Stock_Daily_Data2 <- Stock_Daily_Data %>% 
  # 生成tick文件路径
  mutate(tick_path = if_else(
    substr(S_INFO_WINDCODE, 1L, 1L) == 6, 
    paste0(path_to_red_ashare, "/Tick/SH/", TRADE_DT, "/", substr(S_INFO_WINDCODE, 1L, 6L), ".csv"), 
    paste0(path_to_red_ashare, "/Tick/SZ/", TRADE_DT, "/", substr(S_INFO_WINDCODE, 1L, 6L), ".csv")
  )) %>% 
  # tick文件中需要用到的整数阈值价格
  mutate(UP_THRESHOLD_INT = as.integer(UP_THRESHOLD * 10000), 
         DOWN_THRESHOLD_INT = as.integer(DOWN_THRESHOLD * 10000))

# 计算t1
system.time(
Stock_Daily_Data3 <- Stock_Daily_Data2 %>% 
  mutate(UP_t1 = match_time_vector(tick_path, 
                                   UP_THRESHOLD_INT, 
                                   index = S_DQ_HIGH >= UP_THRESHOLD, 
                                   direction = "up"), 
         DOWN_t1 = match_time_vector(tick_path, 
                                     DOWN_THRESHOLD_INT, 
                                     index = S_DQ_LOW <= DOWN_THRESHOLD, 
                                     direction = "down"))
)


# MCMC涨停概率 ----
# tick时间映射到[0, 14400s]的函数
ticktime2second <- function(ticktime) {
  ticktime = parse_tick_time(ticktime)
  ticktime = if_else(ticktime <= parse_time("113000", format = "%H%M%S"), 
                     as.integer(ticktime - parse_time("093000", format = "%H%M%S")), 
                     as.integer(ticktime - parse_time("130000", format = "%H%M%S")) + 7200L)
  return(ticktime)
}

# 必要数据处理
Stock_Daily_Data4 <- Stock_Daily_Data3 %>% 
  select(-tick_path, -UP_THRESHOLD_INT, -DOWN_THRESHOLD_INT) %>% 
  # 时间变为单位秒
  mutate_at(vars(UP_t1, DOWN_t1), ticktime2second) %>% 
  # 计算simulation的N
  mutate(UP_N = as.integer(round((14400 - UP_t1) / 3)), 
         DOWN_N = as.integer(round((14400 - DOWN_t1) / 3))) %>% 
  # 时间变为单位1
  mutate_at(vars(UP_t1, DOWN_t1), ~ . / 14400) %>% 
  # 删除开会便到达阈值的点和最后时刻才达到阈值的点
  filter((REACH_UP_THRESHOLD & between(UP_N, 2L, 4798L)) | 
         (REACH_DOWN_THRESHOLD & between(DOWN_N, 2L, 4798L)))

# 计算触及涨停板概率的函数
prob_reach_up <- function(N, x0, t0, theta, sigma, up_limit) {
  tryCatch({
    Sim.DiffProc::GBM(N = N, 
                      M = 1000, 
                      x0 = x0, 
                      t0 = t0, 
                      theta = theta, 
                      sigma = sigma) %>% 
      sapply(max) %>% 
      {mean(. >= up_limit)}
  }, error = function(e) NA)
}

prob_reach_down <- function(N, x0, t0, theta, sigma, down_limit) {
  tryCatch({
    Sim.DiffProc::GBM(N = N, 
                      M = 1000, 
                      x0 = x0, 
                      t0 = t0, 
                      theta = theta, 
                      sigma = sigma) %>% 
      sapply(min) %>% 
      {mean(. <= down_limit)}
  }, error = function(e) NA)
}

# 计算触及涨停的概率
system.time(
  Stock_Daily_Data4_UP <- Stock_Daily_Data4 %>% 
    filter(REACH_UP_THRESHOLD & between(UP_N, 2L, 4798L)) %>% 
    mutate(PROB_REACH_UP_LIMIT = mcmapply(prob_reach_up, UP_N, UP_THRESHOLD, UP_t1, mu, sigma, UP_LIMIT, USE.NAMES = FALSE), 
           PROB_REACH_UP_LIMIT_hf = mcmapply(prob_reach_up, UP_N, UP_THRESHOLD, UP_t1, mu_hf, sigma_hf, UP_LIMIT, USE.NAMES = FALSE))
)

# 计算触及跌停的概率
system.time(
  Stock_Daily_Data4_DOWN <- Stock_Daily_Data4 %>% 
    filter(REACH_DOWN_THRESHOLD & between(DOWN_N, 2L, 4798L)) %>% 
    mutate(PROB_REACH_DOWN_LIMIT = mcmapply(prob_reach_down, DOWN_N, DOWN_THRESHOLD, DOWN_t1, mu, sigma, DOWN_LIMIT, USE.NAMES = FALSE), 
           PROB_REACH_DOWN_LIMIT_hf = mcmapply(prob_reach_down, DOWN_N, DOWN_THRESHOLD, DOWN_t1, mu_hf, sigma_hf, DOWN_LIMIT, USE.NAMES = FALSE))
)

save.image("data/01-D-First-Complete-Data.RData")


# 描述性统计 ----
# 日最大收益率的分布
Stock_Daily_Data0 %>% 
  mutate(HIGH_PCTCHANGE = S_DQ_HIGH / S_DQ_PRECLOSE - 1) %>% 
  ggplot(aes(x = HIGH_PCTCHANGE)) + 
  geom_histogram(binwidth = 0.0001)


# 理论概率与真实概率 ----
Stock_Daily_Data4_UP %>% 
  summarise(REACH_UP_LIMIT_mean = mean(REACH_UP_LIMIT), 
            PROB_REACH_UP_LIMIT_mean = mean(PROB_REACH_UP_LIMIT, na.rm = TRUE), 
            REACH_UP_diff = REACH_UP_LIMIT_mean - PROB_REACH_UP_LIMIT_mean, 
            REACH_UP_p.value = t.test(REACH_UP_LIMIT, PROB_REACH_UP_LIMIT)$p.value)

