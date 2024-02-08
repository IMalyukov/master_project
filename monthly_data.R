library(data.table)
library(xts)
library(ROI.plugin.optimx)
library(ROI.plugin.quadprog)
library(PortfolioAnalytics)
library(astsa)


#### downloading data ####
symbols <- c("IMOEXmonthly", "MCXSMmonthly",
             "MOEXINNmonthly", "RGBITRmonthly",
             "RUCBITRmonthly", "RUMBITRmonthly")

for (i in 1:6) {
  x <- symbols[i]
  value <- fread(paste0(symbols[i],".csv"), )
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

monthly_prices <- merge(IMOEXmonthly$CLOSE, MCXSMmonthly, join = "inner")

#### rebuild periodicity #####
periodicity(IMOEXmonthly) 

imoex.m <- to.period(IMOEXmonthly$CLOSE, indexAt = "firstof", period = "months", OHLC = F)
mcxsm.m <- to.period(MCXSMmonthly$CLOSE, indexAt = "firstof", period = "months", OHLC = F)
moexinn.m <- to.period(MOEXINNmonthly$CLOSE, indexAt = "firstof", period = "months", OHLC = F)
rgbitr.m <- to.period(RGBITRmonthly$CLOSE, indexAt = "firstof", period = "months", OHLC = F)
rucbitr.m <- to.period(RUCBITRmonthly$CLOSE, indexAt = "firstof", period = "months", OHLC = F)
rumbitr.m <- to.period(RUMBITRmonthly$CLOSE, indexAt = "firstof", period = "months", OHLC = F)

for (i in 1:6) {
  x <- symbols[i]
  eval(call("rm", as.name(x)))
}

#### merging ######
monthly_prices <- merge(imoex.m, mcxsm.m, join = "inner")
monthly_prices <- merge(monthly_prices, moexinn.m, join = "inner")
monthly_prices <- merge(monthly_prices, rgbitr.m, join = "inner")
monthly_prices <- merge(monthly_prices, rucbitr.m, join = "inner")
monthly_prices <- merge(monthly_prices, rumbitr.m, join = "inner")
colnames(monthly_prices) <- c("imoex.close", "mcxsm.close",
                              "moexinn.close", "rgbitr.close",
                              "rucbitr.close", "rumbitr.close")

#### exploratory data analysis ####

summary(monthly_prices)
monthly_prices <- monthly_prices[,-2]
chart.Correlation(monthly_prices)

monthly_returns <- CalculateReturns(monthly_prices, method = "discrete")
monthly_returns <- monthly_returns[-1,]
class(monthly_returns)
class(daily_ret)

monthly_returns_log <- CalculateReturns(monthly_prices, method = "log")
monthly_returns_log <- monthly_returns_log[-1,]

monthly_returns_discrete <- CalculateReturns(monthly_prices, method = "discrete")
monthly_returns_discrete <- monthly_returns_discrete[-1,]
  
 
summary(monthly_returns)
chart.Correlation(monthly_returns)

summary(monthly_returns_log)
chart.Correlation(monthly_returns_log)

summary(monthly_returns_discrete)
chart.Correlation(monthly_returns_discrete)


chart.RollingCorrelation(monthly_returns$imoex.close, monthly_returns$rgbitr.close, width = 3, main = "Корреляция между индексом Мосбиржи и индексом государственных облигаций")
cor_matrix <- cor(monthly_returns)
#install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, method = "number", type = "lower")
?corrplot

chart.TimeSeries(monthly_prices)
chart.TimeSeries(monthly_returns)
plot(monthly_returns)



shapiro.test(as.vector(monthly_returns$imoex.close))
shapiro.test(as.vector(monthly_returns$mcxsm.close))
shapiro.test(as.vector(monthly_returns$moexinn.close))
shapiro.test(as.vector(monthly_returns$rgbitr.close))
shapiro.test(as.vector(monthly_returns$rucbitr.close))
shapiro.test(as.vector(monthly_returns$rumbitr.close))

par(mfrow = c(3,2))

plot.ts(monthly_returns$imoex.close)
plot.ts(monthly_returns$mcxsm.close)
plot.ts(monthly_returns$moexinn.close)
plot.ts(monthly_returns$rgbitr.close)
plot.ts(monthly_returns$rucbitr.close)
plot.ts(monthly_returns$rumbitr.close)

acf2(monthly_prices$imoex.close)

par(mfrow = c(3,2))

acf2(monthly_returns$imoex.close)
acf2(monthly_returns$mcxsm.close)
acf2(monthly_returns$moexinn.close)
acf2(monthly_returns$rgbitr.close)
acf2(monthly_returns$rucbitr.close)
acf2(monthly_returns$rumbitr.close)

library(tseries)
adf.test(monthly_returns$imoex.close)
adf.test(monthly_returns$mcxsm.close)
adf.test(monthly_returns$moexinn.close)
adf.test(monthly_returns$rgbitr.close)
adf.test(monthly_returns$rucbitr.close)
adf.test(monthly_returns$rumbitr.close)

kpss.test(monthly_returns$imoex.close, null="Trend")
kpss.test(monthly_returns$mcxsm.close)
kpss.test(monthly_returns$moexinn.close)
kpss.test(monthly_returns$rgbitr.close)
kpss.test(monthly_returns$rucbitr.close)
kpss.test(monthly_returns$rumbitr.close)



pp.test(monthly_returns$imoex.close)
pp.test(monthly_returns$mcxsm.close)
pp.test(monthly_returns$moexinn.close)
pp.test(monthly_returns$rgbitr.close)
pp.test(monthly_returns$rucbitr.close)
pp.test(monthly_returns$rumbitr.close)

library(urca)
summary(ur.ers(monthly_returns$imoex.close, model = "trend", 
        lag.max = 2))



par(mfrow = c(1,2))

chart.Histogram(monthly_returns$imoex.close, method = c("add.density", "add.normal"), main = "Monthly IMOEX")
chart.Histogram(monthly_returns$imoex.close, method = c("add.density", "add.normal"), main = "Daily IMOEX")

par(mfrow = c(1,2))

chart.Histogram(monthly_returns$rgbitr.close, method = c("add.density", "add.normal"), main = "Monthly RGBITR")
chart.Histogram(Return.calculate(RGBITRmonthly$CLOSE), method = c("add.density", "add.normal"), main = "Daily RGBITR")

skewness(monthly_returns$imoex.close)
kurtosis(monthly_returns$imoex.close)
high.moments <- matrix(nrow = 4, ncol = 6)

daily <- merge(IMOEXmonthly$CLOSE, MCXSMmonthly$CLOSE, join = "inner")
daily <- merge(daily, MOEXINNmonthly$CLOSE, join = "inner")
daily <- merge(daily, RGBITRmonthly$CLOSE, join = "inner")
daily <- merge(daily, RUCBITRmonthly$CLOSE, join = "inner")
daily <- merge(daily, RUMBITRmonthly$CLOSE, join = "inner")
daily_ret <- Return.calculate(daily)
daily_ret <- daily_ret[-1]

for (i in 1:6) {
  
  high.moments[1,i] <- eval(call("skewness", monthly_returns[,i]))
  high.moments[2,i] <- eval(call("kurtosis", monthly_returns[,i]))
  high.moments[3,i] <- eval(call("skewness", daily_ret[,i]))
  high.moments[4,i] <- eval(call("kurtosis", daily_ret[,i]))
}
high.moments
high.moments <- data.frame(high.moments,
                           row.names = c("Коэффициент асимметрии (ежемесячные данные)",
                                         "Эксцесс (ежемесячные данные)",
                                         "Коэффициент асимметрии (дневные данные)",
                                         "Эксцесс (дневные данные)"))
colnames(high.moments) <- c("Индекс Мосбиржи",
                            "Индекс компаний малой и средней капитализации",
                            "Индекс инноваций",
                            "Индекс гос. облигаций",
                            "Индекс корпоративных облигаций",
                            "Индекс муниципальных облигаций")
high.moments %>% gt(rownames_to_stub = T)


#### First model. Standard Markowitz Model. #####

port_spec <- portfolio.spec(colnames(monthly_returns["2013-12/2019-12"]))
port_spec

port_spec <- add.constraint(portfolio = port_spec,
                            type = "full_investment")
port_spec <- add.constraint(portfolio = port_spec,
                            type = "long_only")
port_spec <- add.objective(portfolio = port_spec,
                           type = "return",
                           name = "mean")
port_spec <- add.objective(portfolio = port_spec,
                           type = "risk",
                           name = "StdDev", risk_aversion = 1.5)
#port_spec <- add.objective(portfolio = port_spec,
 #                          type = "risk_budget",
  #                         name = "StdDev", min_prisk = 0.05, max_prisk = 0.1)

opt <- optimize.portfolio(monthly_returns["2013-12/2019-12"],
                          portfolio = port_spec,
                          optimize_method = "ROI",
                          trace = T)
chart.RiskReward(opt,
                 risk.col = "StdDev",
                return.col = "mean",
               chart.assets = T)


extractWeights(opt)
chart.Weights(opt)

meansd.ef <- create.EfficientFrontier(R = monthly_returns["/2017-12"],
                                      portfolio = port_spec,
                                      type = "mean-sd",
                                      n.portfolios = 500,
)


chart.EfficientFrontier(meansd.ef,
                        type="l",
                        match.col = "StdDev", n.portfolios = 500, xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "SR", rf = 0.04/12, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)

#chart.RiskBudget(opt, match.col = "StdDev", risk.type = "percentage")

returns_train <- Return.portfolio(R = monthly_returns["/2022-12"], weights = extractWeights(opt))
colnames(returns_train) <- "portfolio"
returns_train <- merge(returns_train, monthly_returns["/2022-12"])

table.AnnualizedReturns(returns_train, scale = 12) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = returns_train["/2022-12"]) 

returns_test <- Return.portfolio(R = monthly_returns["2017-12/2022-12"], weights = extractWeights(opt))
colnames(returns_test) <- "portfolio"
returns_test <- merge(returns_test, monthly_returns["2017-12/2022-12"])

table.AnnualizedReturns(returns_test, scale = 12) %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = returns_test)


#### Second model. non-Standard Markowitz Model with rebalancing #########

opt_rebal_base <- optimize.portfolio.rebalancing(R = monthly_returns["/2022-12"], 
                                                 portfolio = port_spec, 
                                                 optimize_method = "ROI", 
                                                 rebalance_on = "quarters",
                                                 trace = T,
                                                 training_period = 24,
                                                 rolling_window = 24)

opt_rebal <- optimize.portfolio.rebalancing(R = monthly_returns["/2022-12"],
                                            portfolio = port_spec,
                                            optimize_method = "ROI",
                                            #rp = rp,
                                            trace = TRUE,
                                            search_size = 1000,
                                            rebalance_on = "quarters",
                                            training_period = 24,
                                            rolling_window = 24)

opt_rebal_base <- optimize.portfolio.rebalancing(R = monthly_returns["/2022-12"], 
                                                 portfolio = port_spec, 
                                                 optimize_method = "random",
                                                 trace = TRUE,
                                                 search_size = 1000,
                                                 rebalance_on = "quarters", 
                                                 training_period = 24,
                                                 rolling_window = 24)


# Print the output of the optimization backtest
print(opt_rebal)

# Print the results
print(opt_rebal_base)

# Chart the weights
chart.Weights(opt_rebal_base)
chart.Weights(opt_rebal)

#### Third model. Model with robust estimators of cov.matrix, rebalancing ####
#### Fourth model. With many restrictions ####

acf2(monthly_returns$imoex.close)
acf2((monthly_returns$imoex.close)^2)
acf2(abs(monthly_returns$imoex.close))
acf(abs(monthly_returns$imoex.close))

library(lmtest)

dwtest(monthly_returns$imoex.close[,1] ~ monthly_returns$imoex.close[,2])
monthly_returns$imoex.close[,1]


best <- auto.arima(x = monthly_prices$imoex.close)
best

mult.norm(monthly_returns)$mult.test
mvnorm.etest(monthly_returns, R = 100)
