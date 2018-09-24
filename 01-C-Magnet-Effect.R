library(tidyverse)
library(lubridate)
library(zoo)
library(Sim.DiffProc)
library(cnquant)
load("~/Documents/Stock Data/D-Stock-Daily-Data.RData")

# 初筛样本，需要的变量和样本期 ----
start_date <- "20141009"  # Tick数据开始的时间
end_date <- "20180330"  # Tick数据结束的时间
Stock_Daily_Data <- Stock_Daily_Data %>% 
  # 需要的变量
  select(S_INFO_WINDCODE:S_DQ_AMOUNT, S_DQ_TRADESTATUS, UP_DOWN_LIMIT_STATUS, ST) %>% 
  # 样本期
  filter(TRADE_DT >= "20140309")


# 滚动估计每日的参数mu和sigma ----
# 估计均值和标准差的函数
mu_sigma <- function(x) c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))

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
  select(-Ri, 
         -rollapply...Ri..120..mu_sigma..fill...NA.) %>% 
  rename(mu = X1, sigma = X2) %>% 
  mutate(mu = mu + 1 / 2 * sigma ^ 2)


# 二筛样本，最大（绝对）涨幅超过9%的样本点 ----
# 先只看大涨的
Stock_Daily_Data2 <- Stock_Daily_Data %>% 
  # 样本期
  filter(TRADE_DT >= start_date) %>% 
  # 后面无参数估计值的样本点
  filter(!is.na(mu)) %>% 
  # 非ST
  filter(!ST) %>% 
  select(-ST) %>% 
  # 正常交易状态
  filter(S_DQ_TRADESTATUS %in% c("交易", "DR", "XD", "XR")) %>% 
  # 计算涨停价
  mutate(UP_LIMIT_PRICE = round(S_DQ_PRECLOSE * 1.1 + .Machine$double.eps ^ 0.5, 2)) %>% 
  # 去掉最高价超过涨停价的有问题的样本点
  # 20141009之后只有一个20151218复牌的000520，2块到20块
  filter(S_DQ_HIGH <= UP_LIMIT_PRICE) %>% 
  # 计算最大涨幅
  mutate(MAX_PCTCHANGE = S_DQ_HIGH / S_DQ_PRECLOSE - 1) %>% 
  # 当日涨幅超过9%且开盘时不到9%
  filter(MAX_PCTCHANGE >= 0.09) %>% 
  filter(S_DQ_OPEN / S_DQ_PRECLOSE - 1 < 0.09)

# 统计涨到9%后达到涨停的条件概率
Stock_Daily_Data2 %>% 
  mutate(REACH_UP = S_DQ_HIGH == UP_LIMIT_PRICE) %>% 
  summarise(mean(REACH_UP), 
            mean(UP_DOWN_LIMIT_STATUS == 1, na.rm = TRUE))


# Tick数据确定特定涨幅时间点 ----
# 添加需要的变量
Stock_Daily_Data3 <- Stock_Daily_Data2 %>% 
  filter(TRADE_DT < end_date) %>% 
  mutate(S_INFO_WINDCODE = substr(S_INFO_WINDCODE, 1, 6)) %>% 
  unite(path, TRADE_DT, S_INFO_WINDCODE, sep = "/", remove = FALSE) %>% 
  mutate(path = if_else(
    substr(S_INFO_WINDCODE, 1, 1) == 6, 
    paste0("/Volumes/Seagate Backup Plus Drive/WIND_DATA/ID_BT_SHARES_A/Tick/SH/", path, ".csv"), 
    paste0("/Volumes/Seagate Backup Plus Drive/WIND_DATA/ID_BT_SHARES_A/Tick/SZ/", path, ".csv")
  )) %>% 
  mutate(up09 = as.integer(S_DQ_PRECLOSE * 1.09 * 10000))

# 从Tick文件中匹配第一次超过特定价格的时间的函数
match_time <- function(path, up09) {
  # path为tick文件路径
  # up09为涨幅阈值
  # 返回第一次价格超过阈值的时间
  tryCatch({
    path %>% 
      read_csv(col_types = cols_only(time = col_integer(), 
                                     high = col_integer())) %>% 
      with(time[detect_index(high, function(x) x >= up09)])
  }, error = function(e) NA)
}

# 计算t1
system.time(
Stock_Daily_Data4 <- Stock_Daily_Data3 %>% 
  mutate(t1 = mapply(match_time, path, up09, USE.NAMES = FALSE))
)


# 日内数据估计每日的参数mu和sigma ----
# 从日内Kline数据估计每日的参数mu和sigma的函数
mu_sigma_intraday <- function(path) {
  # path为tick文件路径
  tryCatch({
    path %>% 
      read_csv(col_types = cols_only(close = col_integer())) %>% 
      with(mu_sigma(log(close / lag(close))))
  }, error = function(e) c(NA, NA))
}

Stock_Daily_Data4 %>% 
  slice(1:10000) %>% 
  unite(path, TRADE_DT, S_INFO_WINDCODE, sep = "/", remove = FALSE) %>% 
  mutate(path = if_else(
    substr(S_INFO_WINDCODE, 1, 1) == 6, 
    paste0("/Volumes/Seagate Backup Plus Drive/WIND_DATA/ID_BT_SHARES_A/Kline/SH/", path, ".csv"), 
    paste0("/Volumes/Seagate Backup Plus Drive/WIND_DATA/ID_BT_SHARES_A/Kline/SZ/", path, ".csv")
  )) %>% 
  do(data.frame(
    ., 
    t(sapply(.$path, mu_sigma_intraday, USE.NAMES = FALSE)), 
    stringsAsFactors = FALSE
  )) %>% 
  rename(mu2 = X1, sigma2 = X2) %>% 
  mutate(sigma = sigma * sqrt(240), 
         mu = mu * 240 + 1 / 2 * sigma ^ 2)




# MCMC涨停概率 ----
Stock_Daily_Data5 <- Stock_Daily_Data4 %>% 
  select(-path) %>% 
  mutate(t1 = parse_tick_time(t1), 
         t1 = if_else(t1 <= parse_time("113000", format = "%H%M%S"), 
                      as.integer(t1 - parse_time("093000", format = "%H%M%S")), 
                      as.integer(t1 - parse_time("130000", format = "%H%M%S")) + 7200L), 
         up09 = up09 / 10000, 
         N = round((14400 - t1) / 3), 
         t1 = t1 / 14400) %>% 
  filter(!is.na(t1)) %>% 
  filter(t1 < 1) %>% 
  mutate(REACH_UP = S_DQ_HIGH == UP_LIMIT_PRICE)

# 计算触及涨停板概率的函数
prob_reach_up <- function(N, x0, t0, theta, sigma, up_limit_price) {
  GBM(N = N, 
      M = 1000, 
      x0 = x0, 
      t0 = t0, 
      theta = theta, 
      sigma = sigma) %>% 
    sapply(max) %>% 
    {mean(. >= up_limit_price)}
}

# 计算触及涨停板的概率
system.time(
  Stock_Daily_Data6 <- Stock_Daily_Data5 %>% 
    slice(1:1000) %>% 
    mutate(PROB_REACH_UP = mapply(prob_reach_up, N, up09, t1, mu, sigma, UP_LIMIT_PRICE, USE.NAMES = FALSE))
)

# 理论概率与真实概率
Stock_Daily_Data6 %>% 
  filter(t1 != 0) %>% 
  filter(N < 4800) %>% 
  summarise(mean(REACH_UP), 
            mean(UP_DOWN_LIMIT_STATUS == 1, na.rm = TRUE), 
            mean(PROB_REACH_UP))


# 日最大收益率的分布
Stock_Daily_Data2 %>%
  select(MAX_PCTCHANGE) %>%
  ggplot(aes(x = MAX_PCTCHANGE)) +
  geom_histogram(binwidth = 0.0001)

Stock_Daily_Data2 %>% 
  # select(MAX_PCTCHANGE) %>% 
  summarise(nall = n(), 
            p009 = sum(MAX_PCTCHANGE > 0.09) / nall, 
            pup = sum(UP_DOWN_LIMIT_STATUS == 1) / nall, 
            cup009 = pup / p009)



