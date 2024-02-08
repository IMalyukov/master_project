#### downloading data ####
symbols <- c("RUPCI", "RUPAI",
             "RUPMI")

for (i in 1:3) {
  x <- symbols[i]
  value <- fread(paste0("data/",symbols[i],".csv"), )
  value <- value[,c(3,6,7,8,9,10)]
  value$CLOSE <- as.numeric(gsub(",", ".", value$CLOSE))
  value$OPEN <- as.numeric(gsub(",", ".", value$OPEN))
  value$HIGH <- as.numeric(gsub(",", ".", value$HIGH))
  value$LOW <- as.numeric(gsub(",", ".", value$LOW))
  value$VALUE <- as.numeric(gsub(",", ".", value$VALUE))
  #colnames(value) <- c("DATE", "OPEN", "HIGH", "LOW", "CLOSE", "VOL")
  value$TRADEDATE <- as.Date(as.character(value$TRADEDATE), "%d.%m.%Y")
  value_xts <- xts(value[,-1], order.by = value$TRADEDATE)
  eval(call("<-", as.name(x), value_xts))
}
rm(value)
rm(value_xts)

###### DAILY PEN_INDEX #####

pension_index_daily <- merge(RUPCI$CLOSE, RUPMI$CLOSE, join = "inner")
pension_index_daily <- merge(pension_index_daily, RUPAI$CLOSE, join = "inner")
colnames(pension_index_daily) <- c("rupci", "rupmi",
                            "rupai")
pension_index_daily

pension_index_daily_ret <- CalculateReturns(pension_index_daily)[-1]
pension_index_daily_ret

plot(pension_index_daily_ret)
plot(pension_index_daily)

##### MONTHLY PEN INDEX ####

rupci.m <- to.period(RUPCI$CLOSE, indexAt = "firstof", period = "months", OHLC = F)
rupmi.m <- to.period(RUPMI$CLOSE, indexAt = "firstof", period = "months", OHLC = F)
rupai.m <- to.period(RUPAI$CLOSE, indexAt = "firstof", period = "months", OHLC = F)

for (i in 1:3) {
  x <- symbols[i]
  eval(call("rm", as.name(x)))
}

pension_index_monthly <- merge(rupci.m, rupmi.m, join = "inner")
pension_index_monthly <- merge(pension_index_monthly, rupai.m, join = "inner")
colnames(pension_index_monthly) <- c("rupci", "rupmi",
                                   "rupai")
pension_index_monthly

pension_index_monthly_ret <- CalculateReturns(pension_index_monthly)[-1]
pension_index_monthly_ret

plot(pension_index_monthly, main = "Пенсионные индексы")
legend(x = "bottomright",
       # legend specifies text label(s)
       legend = c("RUPCI", "RUPMI", "RUPAI"),
       # col specifies color(s)
       col = c("black", "red", "green"),
       # lty specifies line type(s)
       lty = c(1, 1, 1))
plot(as.timeSeries(data(pension_index_monthly_ret)), plot.type = "single")

as.timeSeries(data(pension_index_monthly_ret))
plot.xts(pension_index_monthly)

library(timeSeries)






