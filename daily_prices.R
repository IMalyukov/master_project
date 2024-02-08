daily_prices <- merge(IMOEXmonthly$CLOSE, MCXSMmonthly$CLOSE, join = "inner")
daily_prices <- merge(daily_prices, MOEXINNmonthly$CLOSE, join = "inner")
daily_prices <- merge(daily_prices, RGBITRmonthly$CLOSE, join = "inner")
daily_prices <- merge(daily_prices, RUCBITRmonthly$CLOSE, join = "inner")
daily_prices <- merge(daily_prices, RUMBITRmonthly$CLOSE, join = "inner")
colnames(daily_prices) <- c("imoex", "mcxsm",
                              "moexinn", "rgbitr",
                              "rucbitr", "rumbitr")
daily_prices

plot(daily_prices)

daily_ret <- CalculateReturns(daily_prices)[-1]
daily_ret <- daily_ret[,-2]
daily_ret
plot(daily_ret)
summary(daily_ret)
colMeans(daily_ret)*sqrt(252)
chart.Correlation(daily_ret)
chart.RollingCorrelation(daily_ret$imoex, daily_ret$rgbitr, width = 60, main = "Корреляция между индексом Мосбиржи и индексом государственных облигаций")






