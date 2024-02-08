###### robust optimization ####

train_data <- monthly_returns["/2019-12"]

port_spec <- portfolio.spec(colnames(train_data))
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
                           name = "StdDev"
)
#port_spec <- add.objective(portfolio = port_spec,
#                           type = "risk_budget",
#                           name = "StdDev", min_prisk = 0.01, max_prisk = 0.1)

opt <- optimize.portfolio(train_data,
                          portfolio = port_spec,
                          optimize_method = "random",
                          search_size = 4000,
                          trace = T)

opt_custom <- optimize.portfolio(R = train_data,
                                 portfolio = port_spec,
                                 optimize_method = "random",
                                 search_size = 4000,
                                 trace = T,
                                 momentFUN = "moments_robust")

##### #####
moments_robust <- function(R){
  require(MASS)
  out <- list()
  set.seed(1234)
  out$mu <- colMeans(R)
  out$sigma <- cov.rob(R, method = "mcd")$cov
  return(out)
}

# Estimate the portfolio moments using the function you just defined 
#moments <- moments_robust(R = return.dataset[-1,], portfolio = port_spec)

# Check the moment estimate
#cov.rob(return.dataset[-1,], method = "mcd")$cov == moments$sigma

# Run the optimization with custom moment estimates
opt_custom <- optimize.portfolio(R = train_data,
                                 portfolio = port_spec,
                                 optimize_method = "ROI",
                                 #search_size = 4000,
                                 momentFUN = "moments_robust")

# Print the results of the optimization with custom moment estimates
print(opt_custom)


weights <- extractWeights(opt_custom)
sigma <- moments_robust(train_data)$sigma
sqrt(t(weights) %*% sigma %*% weights)


##### #####
# Print the results of the optimization with custom moment estimates
print(opt_custom)

# Print the results of the optimization with sample moment estimates
print(opt)

train_opt <- Return.portfolio(R = train_data, weights = extractWeights(opt))
colnames(train_opt) <- "portfolio"
train <- merge(train_opt, train_data)

train_opt_custom <- Return.portfolio(R = train_data, weights = extractWeights(opt_custom))
colnames(train_opt_custom) <- "robust_portfolio"
train <- merge(train_data, train_opt_custom)


table.AnnualizedReturns(train, scale = 12) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = train) 

####
test_opt <- Return.portfolio(R = monthly_returns["2020-01/2022-12"],
                             weights = extractWeights(opt))
colnames(test_opt) <- "portfolio"
test <- merge(test_opt, monthly_returns["2020-01/2022-12"])

test_opt_custom <- Return.portfolio(R = monthly_returns["2020-01/2022-12"],
                                    weights = extractWeights(opt_custom))
colnames(test_opt_custom) <- "robust_portfolio"
test <- merge(test, test_opt_custom)


table.AnnualizedReturns(test, scale = 12) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = test) 



test <- Return.portfolio(R = monthly_returns["2020-01/2022-12"], weights = extractWeights(opt_custom))
colnames(test) <- "portfolio_custom"
test <- merge(test, monthly_returns["2020-01/2022-12"])





chart.RiskReward(opt_custom,
                 risk.col = "StdDev",
                 return.col = "mean",
                 chart.assets = T)

extractWeights(opt_custom)
chart.Weights(opt_custom)
