# 日内数据估计每日的参数mu和sigma ----
bar <- Stock_Daily_Data %>% 
  filter(TRADE_DT >= "20141009", 
         TRADE_DT <= "20180330") %>% 
  sample_n(100) %>% 
  mutate(S_INFO_WINDCODE = substr(S_INFO_WINDCODE, 1, 6)) %>% 
  unite(path, TRADE_DT, S_INFO_WINDCODE, sep = "/", remove = FALSE) %>% 
  mutate(path = if_else(
    substr(S_INFO_WINDCODE, 1, 1) == 6, 
    paste0("/Volumes/Seagate Backup Plus Drive/WIND_DATA/ID_BT_SHARES_A/KLine/SH/", path, ".csv"), 
    paste0("/Volumes/Seagate Backup Plus Drive/WIND_DATA/ID_BT_SHARES_A/KLine/SZ/", path, ".csv")
  ))

bar %>% 
  # with(t(sapply(path, mu_sigma_intraday, USE.NAMES = FALSE)))
  do(data.frame(
    ., 
    t(sapply(.$path, mu_sigma_intraday, USE.NAMES = FALSE)), 
    stringsAsFactors = FALSE
  )) %>% 
  rename(mu2 = X1, sigma2 = X2) %>% 
  mutate(sigma2 = sigma2 * sqrt(240), 
         mu2 = mu2 * 240 + 1 / 2 * sigma2 ^ 2) %>% 
  summarise(sd(sigma, na.rm = T), sd(sigma2, na.rm = T))


