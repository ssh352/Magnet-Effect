### 筛选样本条件
+ 样本期
+ 主板
+ 非ST
+ 交易状态正常

### Diagram
+ greater reach-limit possibility
+ up v.s. down
+ hit v.s. last(when ending)
+ robustness: daily v.s. intraday estimate
+ the closer, the stronger effect
+ the harder to value, the stronger effect
+ the stronger investor sentiment, the stronger effect

# rhetorics
+ Technically, we exclude observations where stock hits the price limit within 3 seconds from the stock market opening
+ If there was really bad news overnight, the price will hit the lower price limit at market opening, thus the maximum price percent change would be fixed at around -10% during the day.
+ Note that there exist more stock-days that have a maximum or minimum price percent change at zero, which are clipped for a neater prospect.
