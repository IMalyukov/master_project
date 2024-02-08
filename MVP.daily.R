##### MINIMUM VARIANCE PORTFOLIO #####

train_data <- daily_ret["/2016-12"]
colMeans(train_data)#*sqrt(12)
sd(train_data$imoex)
sd(train_data$mcxsm)
sd(train_data$moexinn)
sd(train_data$rgbitr)
sd(train_data$rucbitr)
sd(train_data$rumbitr)


##### Conservative ######
# Create the portfolio specification
port_spec <- portfolio.spec(asset = colnames(daily_ret["/2016-12"]))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev", risk_aversion = 1)
#port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "var", risk_aversion = 10)

# Print the portfolio specification
print(port_spec)

opt <- optimize.portfolio(train_data,
                          portfolio = port_spec,
                          optimize_method = "ROI",
                          trace = T)
chart.RiskReward(opt,
                 risk.col = "StdDev",
                 return.col = "mean",
                 chart.assets = T)

extractWeights(opt)
chart.Weights(opt)

train <- Return.portfolio(R = daily_ret["/2016-12"], weights = extractWeights(opt))
colnames(train) <- "portfolio"
train <- merge(train, train_data)

table.AnnualizedReturns(train, scale = 252) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = train) 

test_1 <- Return.portfolio(R = daily_ret["2017-01/2019-12"], weights = extractWeights(opt))
colnames(test_1) <- "portfolio"
test_1 <- merge(test_1, daily_ret["2017-01/2019-12"])

table.AnnualizedReturns(test_1, scale = 252) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = test_1)

test_2 <- Return.portfolio(R = daily_ret["2020-01/2022-12"], weights = extractWeights(opt))
colnames(test_2) <- "portfolio"
test_2 <- merge(test_2, daily_ret["2020-01/2022-12"])

table.AnnualizedReturns(test_2, scale = 252) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = test_2)

whole <- Return.portfolio(R = daily_ret["/2022-12"], weights = extractWeights(opt))
colnames(whole) <- "portfolio"
whole <- merge(whole, daily_ret["/2022-12"])

table.AnnualizedReturns(whole, scale = 252) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = whole)




##### Medium #####
# Create the portfolio specification
port_spec <- portfolio.spec(asset = colnames(daily_ret["/2016-12"]))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev", risk_aversion = 1)
#port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "var", risk_aversion = 10)

#
port_spec <- add.constraint(portfolio = port_spec, type="return", return_target = 0.0004)

# Print the portfolio specification
print(port_spec)

opt <- optimize.portfolio(train_data,
                          portfolio = port_spec,
                          optimize_method = "ROI",
                          trace = T)
chart.RiskReward(opt,
                 risk.col = "StdDev",
                 return.col = "mean",
                 chart.assets = T)

extractWeights(opt)
chart.Weights(opt)

train <- Return.portfolio(R = daily_ret["/2016-12"], weights = extractWeights(opt))
colnames(train) <- "portfolio"
train <- merge(train, train_data)

table.AnnualizedReturns(train, scale = 12) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = train) 

test_1 <- Return.portfolio(R = daily_ret["2017-01/2019-12"], weights = extractWeights(opt))
colnames(test_1) <- "portfolio"
test_1 <- merge(test_1, daily_ret["2017-01/2019-12"])

table.AnnualizedReturns(test_1, scale = 12) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = test_1)

test_2 <- Return.portfolio(R = daily_ret["2020-01/2022-12"], weights = extractWeights(opt))
colnames(test_2) <- "portfolio"
test_2 <- merge(test_2, daily_ret["2020-01/2022-12"])

table.AnnualizedReturns(test_2, scale = 12) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = test_2)


##### Aggressive #####
# Create the portfolio specification
port_spec <- portfolio.spec(asset = colnames(daily_ret["/2016-12"]))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev", risk_aversion = 1)
#port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "var", risk_aversion = 10)

#
port_spec <- add.constraint(portfolio = port_spec, type="return", return_target = 0.000479)

# Print the portfolio specification
print(port_spec)

opt <- optimize.portfolio(train_data,
                          portfolio = port_spec,
                          optimize_method = "ROI",
                          trace = T)
chart.RiskReward(opt,
                 risk.col = "StdDev",
                 return.col = "mean",
                 chart.assets = T)

extractWeights(opt)
chart.Weights(opt)

train <- Return.portfolio(R = daily_ret["/2016-12"], weights = extractWeights(opt))
colnames(train) <- "portfolio"
train <- merge(train, train_data)

table.AnnualizedReturns(train, scale = 12) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = train) 

test_1 <- Return.portfolio(R = daily_ret["2017-01/2019-12"], weights = extractWeights(opt))
colnames(test_1) <- "portfolio"
test_1 <- merge(test_1, daily_ret["2017-01/2019-12"])

table.AnnualizedReturns(test_1, scale = 12) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = test_1)

test_2 <- Return.portfolio(R = daily_ret["2020-01/2022-12"], weights = extractWeights(opt))
colnames(test_2) <- "portfolio"
test_2 <- merge(test_2, daily_ret["2020-01/2022-12"])

table.AnnualizedReturns(test_2, scale = 12) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = test_2)
