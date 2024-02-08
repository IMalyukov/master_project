##### packages #####
install.packages("gdata")
install.packages("doParallel")
install.packages("ROI.plugin.glpk")
install.packages("astsa")
install.packages("gt")
install.packages("vtable")
install.packages("strucchange")

###### libraries #####
library(PortfolioAnalytics)
library(ROI.plugin.optimx)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(data.table)
library(gdata)
library(MASS)
library(corrplot)
library(astsa)
library(gt)
library(magrittr)
library(vtable)
library(strucchange)

#registerDoParallel(cores=4)
#library(doParallel)

##### data ######
### russian instruments

### RGBI, IMOEX, MOEXBC, MCXSM, MOEXBMI, MOEX10, MOEXINN

### industry indices MOEXOG, MOEXEU,
                #MOEXTL, MOEXMM, MOEXFN,
                #MOEXCN, MOEXCH,
                #MOEXTN, MOEXIT, MOEXRE


##### Total return portfolio #####

# MCFTR, MEBCTR, MESMTR, RGBITR


##### download data #####

#### rgbi #####
rgbi <- fread("RGBI_000101_221209.csv")
colnames(rgbi) <- c("DATE", "TIME", "OPEN", "HIGH", "LOW", "CLOSE", "VOL")
str(rgbi)
rgbi$DATE <- as.Date(as.character(rgbi$DATE), "%Y%m%d")
rgbi_xts <- xts(rgbi[,-c(1,2)], order.by = rgbi$DATE)
chart.TimeSeries(rgbi_xts$CLOSE)

rgbi_returns <- Return.calculate(rgbi_xts$CLOSE)
chart.TimeSeries(rgbi_returns)

#### imoex #####
imoex <- fread("IMOEX_000101_221209.csv")
colnames(imoex) <- c("DATE", "TIME", "OPEN", "HIGH", "LOW", "CLOSE", "VOL")
str(imoex)
imoex$DATE <- as.Date(as.character(imoex$DATE), "%Y%m%d")
imoex_xts <- xts(imoex[,-c(1,2)], order.by = imoex$DATE)
chart.TimeSeries(imoex_xts)

imoex_returns <- Return.calculate(imoex_xts$CLOSE)
imoex_returns_20120305 <- imoex_returns["2012-03-05/"]
chart.TimeSeries(imoex_returns)
str(imoex_returns_20120305)
str(rgbi_returns)
rm(imoex)

##### rgbitr ####
rgbi_tr <- fread("RGBITR_000101_221209.csv")
colnames(rgbi_tr) <- c("DATE", "TIME", "OPEN", "HIGH", "LOW", "CLOSE", "VOL")
str(rgbi_tr)
rgbi_tr$DATE <- as.Date(as.character(rgbi_tr$DATE), "%Y%m%d")
rgbi_tr_xts <- xts(rgbi_tr[,-c(1,2)], order.by = rgbi_tr$DATE)
chart.TimeSeries(rgbi_tr_xts$CLOSE)


#### MOEXBC, MCXSM, MOEXBMI, MOEX10, MOEXINN #####
symbols <- c("MOEXBC", "MCXSM", "MOEXBMI", "MOEX10", "MOEXINN")
symbols <- tolower(symbols)

#### downloading cycle ######
for (i in 1:5) {
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

###### total xts ####
total_xts <- merge(rgbi_xts$CLOSE, imoex_xts$CLOSE, join = "inner")
colnames(total_xts) <- c("rgbi.close", "imoex.close")
total_xts <- merge(total_xts, MCXSM$CLOSE, join = "inner")
colnames(total_xts) <- c("rgbi.close", "imoex.close", "mcxsm.close")
total_xts <- merge(total_xts, MOEX10$CLOSE, join = "inner")
colnames(total_xts) <- c("rgbi.close", "imoex.close", "mcxsm.close", "moex10.close")
total_xts <- merge(total_xts, MOEXBC$CLOSE, join = "inner")
colnames(total_xts) <- c("rgbi.close", "imoex.close", "mcxsm.close", "moex10.close", "moexbc.close")
total_xts <- merge(total_xts, MOEXBMI$CLOSE, join = "inner")
colnames(total_xts) <- c("rgbi.close", "imoex.close", "mcxsm.close", "moex10.close", "moexbc.close", "moexbmi.close")
total_xts <- merge(total_xts, MOEXINN$CLOSE, join = "inner")
colnames(total_xts) <- c("rgbi.close", "imoex.close",
                         "mcxsm.close", "moex10.close",
                         "moexbc.close", "moexbmi.close", "moexinn.close")


total_xts <- merge(total_xts, rgbi_tr_xts$CLOSE, join = "inner")
colnames(total_xts) <- c("rgbi.close", "imoex.close",
                         "mcxsm.close", "moex10.close",
                         "moexbc.close", "moexbmi.close", "moexinn.close", "rgbitr.close")



write.csv(total_xts, "total_xts.csv")


######## transforming  ############
return.dataset <- Return.calculate(total_xts[-1,-1])
return.dataset <- return.dataset[-1,]

mcxsm_returns <- Return.calculate(MCXSM$CLOSE)
chart.TimeSeries(return.dataset)
chart.TimeSeries(total_xts)

##### optimization. base model #####
port_spec <- portfolio.spec(colnames(monthly_ret["/2019-12-31"]))
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
                           name = "StdDev")

opt <- optimize.portfolio(monthly_ret["/2019-12-31"],
                          portfolio = port_spec,
                          optimize_method = "random",
                          trace = T)
chart.RiskReward(opt,
                 risk.col = "StdDev",
                 return.col = "mean",
                 chart.assets = F)

extractWeights(opt)
chart.Weights(opt)

meansd.ef <- create.EfficientFrontier(R = monthly_ret["/2019-12-31"],
  portfolio = port_spec,
  type = "mean-sd",
  n.portfolios = 500,
)


chart.EfficientFrontier(meansd.ef,
                        type="l",
                        match.col = "StdDev", n.portfolios = 500, xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)

###### optimization with rebalance #####
# Run the optimization
opt_rebal_base <- optimize.portfolio.rebalancing(R = monthly_ret["/2019-12-31"], 
                                                 portfolio = port_spec, 
                                                 optimize_method = "ROI", 
                                                 rebalance_on = "quarters", 
                                                 training_period = 36,
                                                 rolling_window = 24)

# Print the results
print(opt_rebal_base)

# Chart the weights
chart.Weights(opt_rebal_base)

# Compute the portfolio returns
returns_base <- Return.portfolio(R = return.dataset["/2019-12-31"], weights = extractWeights(opt))
colnames(returns_base) <- "base"
returns_base
tail(returns_base)
cumsum(returns_base)
returns_base <- merge(returns_base, return.dataset)
returns_base <- merge(returns_base, returns_base_2)

returns_base_2 <- Return.portfolio(R = return.dataset["/2019-12-31"], weights = extractWeights(opt_rebal_base))
colnames(returns_base) <- "base_2"
returns_base_2

table.AnnualizedReturns(return.dataset, scale = 252)
charts.PerformanceSummary(R = returns_base["/2019-12-31"])


###### risk_budget #####

port_spec <- add.objective(portfolio = port_spec, 
                           type = "risk_budget", 
                           name = "StdDev", 
                           min_prisk = 0.05, 
                           max_prisk = 0.1)

# Run the optimization
opt_rebal_rb <- optimize.portfolio.rebalancing(R = return.dataset["/2021-12-31"], 
                                               portfolio = port_spec, 
                                               optimize_method = "random",
                                               trace = TRUE,
                                               rebalance_on = "quarters", 
                                               training_period = 252,
                                               rolling_window = 240)

# Chart the weights
chart.Weights(opt_rebal_rb)

# Chart the percentage contribution to risk
chart.RiskBudget(opt_rebal_rb, match.col = "StdDev", risk.type = "percentage")

# Compute the portfolio returns
returns_rb <- Return.portfolio(R = return.dataset["/2021-12-31"], weights = extractWeights(opt_rebal_rb))
colnames(returns_rb) <- "risk_budget"

returns_base <- merge(returns_base, returns_rb)
charts.PerformanceSummary(R = returns_base, plot.engine = "ggplot")
?charts.PerformanceSummary


##### moment_robust #####
moments_robust <- function(R, portfolio){
  out <- list()
  out$sigma <- cov.rob(R, method = "mcd")$cov
  return(out)
}

# Estimate the portfolio moments using the function you just defined 
moments <- moments_robust(R = return.dataset[-1,], portfolio = port_spec)

# Check the moment estimate
cov.rob(return.dataset[-1,], method = "mcd")$cov == moments$sigma

# Run the optimization with custom moment estimates
opt_custom <- optimize.portfolio(R = return.dataset[-1,],
                                 portfolio = port_spec,
                                 optimize_method = "random",
                                 momentFUN = "moments_robust")

# Print the results of the optimization with custom moment estimates
print(opt_custom)

# Run the optimization with sample moment estimates
opt_sample <- optimize.portfolio(R = return.dataset[-1,], portfolio = port_spec, optimize_method = "random", rp = rp)

# Print the results of the optimization with sample moment estimates
print(opt_sample)

##### removing #####
rm(opt_rebal_base)

##### exploratory analysis of data #####
cor(return.dataset["/2019-12-31"])
summary(return.dataset["/2019-12-31"])
summary(return.dataset)

monthly_ret <- to.monthly(return.dataset, OHLC = F)



corrplot(cor(monthly_ret["/2019-12"]), method = "number")
?to.monthly

par(mfrow = c(1,2))

chart.Histogram(monthly_ret$imoex.close, method = c("add.density", "add.normal"), main = "Monthly IMOEX")
chart.Histogram(return.dataset$imoex.close, method = c("add.density", "add.normal"), main = "Daily IMOEX")

chart.Histogram(monthly_ret$rgbitr.close, method = c("add.density", "add.normal"), main = "Monthly RGBITR")
chart.Histogram(return.dataset$rgbitr.close, method = c("add.density", "add.normal"), main = "Daily RGBITR")

chart.Histogram(monthly_ret$mcxsm.close, method = c("add.density", "add.normal"), main = "Monthly MCXSM")
chart.Histogram(return.dataset$mcxsm.close, method = c("add.density", "add.normal"), main = "Daily MCXSM")


skewness(monthly_ret$imoex.close)
kurtosis(monthly_ret$imoex.close)

skewness(return.dataset$imoex.close)
kurtosis(return.dataset$imoex.close)

table.Drawdowns(return.dataset$imoex.close)
chart.Drawdown(return.dataset$imoex.close)

table.Drawdowns(return.dataset$rgbitr.close)
chart.Drawdown(return.dataset$rgbitr.close)

chart.RollingPerformance(R = monthly_ret$imoex.close, width = 12, FUN = "Return.annualized")

charts.PerformanceSummary(R = monthly_ret$imoex.close, plot.engine = "default")
tail(monthly_ret, n = 12)

chart.Correlation(return.dataset["/2019-12-31"])
