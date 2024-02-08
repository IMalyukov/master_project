##### EQUALLY WEIGHTED PORTFOLIO ####

# Create a vector of equal weights
equal_weights <- rep(1 / ncol(monthly_returns), ncol(monthly_returns))
equal_weights

# Compute the benchmark returns
r_benchmark <- Return.portfolio(R = monthly_returns, weights = equal_weights, rebalance_on = "quarters")
colnames(r_benchmark) <- "benchmark"

# Plot the benchmark returns
plot(r_benchmark)

returns_train <- merge(r_benchmark, monthly_returns["/2022-12"])

table.AnnualizedReturns(returns_train["2015-01/2019-12"], scale = 12) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = returns_train["/2022-12"]) 





sd(monthly_returns$imoex.close)
sd(IMOEXmonthly$CLOSE)
daily_ret <- CalculateReturns(IMOEXmonthly$CLOSE)[-1]
periodicity(daily_ret["2013::2022"])

sd(daily_ret["2013::2022"]) * sqrt(252)

sqrt(12) * sd(monthly_returns$imoex.close)
##### #####
sd(monthly_returns$imoex.close["2013"]) * sqrt(12)
sd(monthly_returns$imoex.close["2014"]) * sqrt(12)
sd(monthly_returns$imoex.close["2015"]) * sqrt(12)
sd(monthly_returns$imoex.close["2016"]) * sqrt(12)
sd(monthly_returns$imoex.close["2017"]) * sqrt(12)
sd(monthly_returns$imoex.close["2018"]) * sqrt(12)
sd(monthly_returns$imoex.close["2019"]) * sqrt(12)
sd(monthly_returns$imoex.close["2020"]) * sqrt(12)
sd(monthly_returns$imoex.close["2021"]) * sqrt(12)
sd(monthly_returns$imoex.close["2022"]) * sqrt(12)
##### ####
sd(daily_ret["2013"]) * sqrt(252)
sd(daily_ret["2014"]) * sqrt(252)
sd(daily_ret["2015"]) * sqrt(252)
sd(daily_ret["2016"]) * sqrt(252)
sd(daily_ret["2017"]) * sqrt(252)
sd(daily_ret["2018"]) * sqrt(252)
sd(daily_ret["2019"]) * sqrt(252)
sd(daily_ret["2020"]) * sqrt(252)
sd(daily_ret["2021"]) * sqrt(252)
sd(daily_ret["2022"]) * sqrt(252)
# Load the package PerformanceAnalytics
library(PerformanceAnalytics)

# Showing two plots on the same figure (MONTHLY)
par(mfrow=c(2,1)) 

# Compute the rolling 1 month estimate of annualized volatility
chart.RollingPerformance(R = monthly_returns$imoex.close["1999::2022"], width = 2,
                         FUN = "sd.annualized", scale = 12, main = "One month rolling volatility")

# Compute the rolling 3 months estimate of annualized volatility
chart.RollingPerformance(R = monthly_returns$imoex.close["1999::2022"], width = 3,
                         FUN = "sd.annualized", scale = 12, main = "Three months rolling volatility")

m <- mean(monthly_returns$imoex.close) 

# Define the series of prediction errors
e <- monthly_returns$imoex.close - m

# Plot the absolute value of the prediction errors
par(mfrow = c(2,1),mar = c(3, 2, 2, 2))
plot(abs(e))

# Plot the acf of the absolute prediction errors
acf(abs(e))

# Showing two plots on the same figure (DAILY)
par(mfrow=c(2,1)) 

# Compute the rolling 1 month estimate of annualized volatility
chart.RollingPerformance(R = daily_ret["1999::2022"], width = 22,
                         FUN = "sd.annualized", scale = 252, main = "One month rolling volatility")

# Compute the rolling 3 months estimate of annualized volatility
chart.RollingPerformance(R = daily_ret["1999::2022"], width = 66,
                         FUN = "sd.annualized", scale = 252, main = "Three months rolling volatility")

m <- mean(daily_ret["1999::2022"]) 

# Define the series of prediction errors
e <- daily_ret["1999::2022"] - m

# Plot the absolute value of the prediction errors
par(mfrow = c(2,1),mar = c(3, 2, 2, 2))
plot(abs(e))

# Plot the acf of the absolute prediction errors
acf(abs(e))














# Compute the predicted variances
predvar[1] <- var(monthly_returns$imoex.close) 
for(t in 2:nobs){
  predvar[t] <- omega + alpha * e2[t-1] + beta * predvar[t-1]
}

# Create annualized predicted volatility
ann_predvol <- xts(sqrt(252) * sqrt(predvar), order.by = time(sp500ret))

# Plot the annual predicted volatility in 2008 and 2009
plot(ann_predvol["2008::2009"], main = "Ann. S&P 500 vol in 2008-2009")

install.packages("rugarch")
library(rugarch)

# Specify a standard GARCH model with constant mean
garchspec <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "sGARCH"), 
                        distribution.model = "norm")

# Estimate the model
garchfit <- ugarchfit(data = monthly_returns$imoex.close["2013::2021"], spec = garchspec)

# Use the method sigma to retrieve the estimated volatilities 
garchvol <- sigma(garchfit) 

# Plot the volatility for 2017
plot(garchvol)

# Compute unconditional volatility
sqrt(uncvariance(garchfit))

# Print last 10 ones in garchvol
tail(garchvol, n = 10)

# Forecast volatility 5 days ahead and add 
garchforecast <- ugarchforecast(fitORspec = garchfit, 
                                n.ahead = 5)

# Extract the predicted volatilities and print them
print(sigma(garchforecast))
















##### Compute the annualized volatility #####

annualvol <- sqrt(252) * sigma(garchfit)

# Compute the 5% vol target weights  
vt_weights <- 0.05 / annualvol

# Compare the annualized volatility to the portfolio weights in a plot
plot(merge(annualvol, vt_weights), multi.panel = TRUE)
plot(vt_weights)

# Compute the standardized returns
stdret <- residuals(garchfit, standardize = TRUE)

# Compute the standardized returns using fitted() and sigma()
stdret <- (daily_ret["1999::2022"] - fitted(garchfit)) / sigma(garchfit)

# Load the package PerformanceAnalytics and make the histogram
library(PerformanceAnalytics)
chart.Histogram(stdret, methods = c("add.normal","add.density" ), 
                colorset = c("gray","red","blue"))

#install.packages("nortsTest")
#install.packages("urca")
#install.packages("aTSA")
library(nortsTest)
arch.test(daily_ret["1999::2022"],arch = c("box","Lm"),alpha = 0.05,lag.max = 2)


periodicity(imoex.m$CLOSE)
arch.test(CalculateReturns(imoex.m$CLOSE["2008::2022"], method = "log")[-1],arch = c("box","Lm"),alpha = 0.05,lag.max = 2)
CalculateReturns()


par(mfrow = c(2,1),mar = c(3, 2, 2, 2))
plot(CalculateReturns(imoex.m$CLOSE["2008::2022"], method = "log"))
plot(monthly_returns)

arch.test(CalculateReturns(imoex.m$CLOSE["2000::2022"])[-1], arch = c("box","Lm"),alpha = 0.001,lag.max = 2)
arch.test(monthly_returns$imoex.close, arch = c("box","Lm"),alpha = 0.001,lag.max = 2)

periodicity(CalculateReturns(imoex.m$CLOSE["2013::2022"])[-1])
periodicity(monthly_returns$imoex.close)
plot()

#### 

var.test(imoex.m$CLOSE["2013::2018"], imoex.m$CLOSE["2019::2022"])
plot(CalculateReturns(imoex.m$CLOSE["1997::2022"])[-1])
plot(CalculateReturns(imoex.m$CLOSE["1997::2022"], method = "discrete")[-1])
plot(log(IMOEXmonthly$CLOSE))

##### structural change #####
#install.packages("strucchange")
library("strucchange")

periodicity(imoex.m$CLOSE["2013::2022"])
length(imoex.m$CLOSE["2013::2022"])
time <- c(1:120)

per_month <- breakpoints(imoex.m$CLOSE["2013::2022"] ~ time, h = 10)
per_month

imoex.m$CLOSE["2013::2022"][24]
imoex.m$CLOSE["2013::2022"][52]
imoex.m$CLOSE["2013::2022"][86]
imoex.m$CLOSE["2013::2022"][102]
imoex.m$CLOSE["2013::2022"][109]

coef(per_month)

?breakpoints


ur.ers()

mean(monthly_returns$imoex.close["2014::2015"]) * sqrt(12)
sd(monthly_returns$imoex.close) * sqrt(12)
mean(monthly_returns$rucbitr.close["2019::2022"]) * sqrt(12)


mean(CalculateReturns(imoex.m$CLOSE["2013::2015"])[-1])


#### Multivariate Analysis ####

#install.packages("MTS")
library(MTS)

MarchTest(monthly_returns)


zt <- matrix(rnorm(600),200,3)
MarchTest(zt)
plot(ts(zt))
plot(ts(monthly_returns))


rtn <- log(monthly_returns+1)
at <-  scale(rtn, center=T, scale=F) ## remove sample means
MarchTest(at)

arch.test(monthly_returns$imoex.close)

VAR(monthly_returns, 1)
m1a <- BEKK11(coredata(monthly_returns[,1:3])+1)
m1a



monthly_returns
write.csv(monthly_returns, "monthly_ret.csv", row.names = T)



