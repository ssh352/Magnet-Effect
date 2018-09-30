Stock_Daily_Data00 <- Stock_Daily_Data4 %>% 
  select(S_INFO_WINDCODE:S_DQ_LOW, mu:sigma)

prob_reach <- function(Stock_Daily_Data, threshold) {
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
  
  
  # Tick数据确定涨跌超过阈值时间点 ----
  # 添加需要的变量
  Stock_Daily_Data <- Stock_Daily_Data %>% 
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
  Stock_Daily_Data <- Stock_Daily_Data %>% 
    mutate(UP_t1 = match_time_vector(tick_path, 
                                     UP_THRESHOLD_INT, 
                                     index = S_DQ_HIGH >= UP_THRESHOLD, 
                                     direction = "up"), 
           DOWN_t1 = match_time_vector(tick_path, 
                                       DOWN_THRESHOLD_INT, 
                                       index = S_DQ_LOW <= DOWN_THRESHOLD, 
                                       direction = "down"))
  
  
  # MCMC涨停概率 ----
  # 必要数据处理
  Stock_Daily_Data <- Stock_Daily_Data %>% 
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
  
  # 计算触及涨停的概率
  Stock_Daily_Data4_UP <- Stock_Daily_Data %>% 
    filter(REACH_UP_THRESHOLD & between(UP_N, 2L, 4798L)) %>% 
    mutate(PROB_REACH_UP_LIMIT = mcmapply(prob_reach_up, UP_N, UP_THRESHOLD, UP_t1, mu, sigma, UP_LIMIT, USE.NAMES = FALSE))
  
  # 计算触及跌停的概率
  Stock_Daily_Data4_DOWN <- Stock_Daily_Data %>% 
    filter(REACH_DOWN_THRESHOLD & between(DOWN_N, 2L, 4798L)) %>% 
    mutate(PROB_REACH_DOWN_LIMIT = mcmapply(prob_reach_down, DOWN_N, DOWN_THRESHOLD, DOWN_t1, mu, sigma, DOWN_LIMIT, USE.NAMES = FALSE))
  
  
  # return ----
  list(Stock_Daily_Data4_UP = Stock_Daily_Data4_UP, 
       Stock_Daily_Data4_DOWN = Stock_Daily_Data4_DOWN) %>% 
    return()
}

res <- vector("list", 9L)
thresholds <- seq(0.091, 0.099, by = 0.001)

for (i in 1:4) {
  res[i] <- prob_reach(Stock_Daily_Data00, thresholds[i])
}

save.image("data/01-D-Seq-Data.RData")
