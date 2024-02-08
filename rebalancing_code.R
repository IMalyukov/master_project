options(scipen=999)

###### rebalancing #####
port_spec <- portfolio.spec(colnames(monthly_returns))
port_spec

port_spec <- add.constraint(portfolio = port_spec,
                            type = "full_investment")
# Add the weight sum constraint
#port_spec <- add.constraint(portfolio = port_spec, type = "weight_sum", min_sum = 0.25, max_sum = 1)

# Add the box constraint
port_spec <- add.constraint(portfolio = port_spec, type = "box", min = 0, max = 0.99)

# Add the group constraint
port_spec <- add.constraint(portfolio = port_spec, type = "group", groups = list(c(1,2,3), c(4,5,6)), group_min = 0.00, group_max = 0.99)

#port_spec <- add.constraint(portfolio = port_spec,
   #                         type = "long_only")
#port_spec <- add.objective(portfolio = port_spec,
                  #         type = "return",
                 #          name = "mean")
#port_spec <- add.objective(portfolio = port_spec,
                 #         type = "risk",
                #          name = "StdDev")

#port_spec <- add.constraint(portfolio = port_spec,
 #                          type="transaction_cost",
  #                          ptc = 0.05)
port_spec <- add.objective(portfolio = port_spec,
                           type="quadratic_utility", 
                        risk_aversion= 2)

port_spec



opt_rebal <- optimize.portfolio.rebalancing(R = monthly_returns,
                                            portfolio = port_spec,
                                            optimize_method = "ROI",
                                           # trace = TRUE,
                                           # search_size = 4000,
                                            rebalance_on = "quarters",
                                            training_period = 24, rolling_window = NULL)



# Print the output of the optimization backtest
print(opt_rebal)

# Chart the weights
chart.Weights(opt_rebal)
extractWeights(opt_rebal)

extractObjectiveMeasures(opt)
extractObjectiveMeasures(opt_rebal)

portfolio_reb <- Return.portfolio(R = monthly_returns,
                          weights = extractWeights(opt_rebal))
colnames(portfolio_reb) <- "portfolio_rebal"
portfolio_reb <- merge(portfolio_reb, monthly_returns)

table.AnnualizedReturns(portfolio_reb, scale = 12) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = portfolio_reb)

train <- Return.portfolio(R = train_data,
                          weights = extractWeights(opt_rebal))
colnames(train) <- "portfolio"
train <- merge(train, train_data)

table.AnnualizedReturns(train, scale = 12) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = train) 

test <- Return.portfolio(R = monthly_returns["2020-01/2022-12"],
                         weights = extractWeights(opt_rebal))
colnames(test) <- "portfolio"
test <- merge(test, monthly_returns["2020-01/2022-12"])

table.AnnualizedReturns(test, scale = 12) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = test)

