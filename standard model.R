options(scipen=999)

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
                           name = "StdDev", risk_aversion = 0.75
                           )
#port_spec <- add.objective(portfolio = port_spec,
 #                          type = "risk_budget",
 #                          name = "StdDev", max_prisk = 0.5)
#port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "var", risk_aversion = 1)

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

chart.RiskBudget(opt, match.col = "StdDev", risk.type = "percentage")

meansd.ef <- create.EfficientFrontier(R = train_data,
                                      portfolio = port_spec,
                                      type = "mean-sd",
                                      n.portfolios = 500
)


chart.EfficientFrontier(meansd.ef,
                        type="l",
                        match.col = "StdDev", n.portfolios = 500, xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "SR", rf = 0.04/12, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)

meanvar.efficient.frontier(port_spec, R = train_data, n.portfolios = 25,
                           risk_aversion = NULL)


train <- Return.portfolio(R = monthly_returns["/2019-12"], weights = extractWeights(opt))
colnames(train) <- "portfolio"
train <- merge(train, train_data)

table.AnnualizedReturns(train, scale = 12) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = train) 

test <- Return.portfolio(R = monthly_returns["2020-01/2022-12"], weights = extractWeights(opt))
colnames(test) <- "portfolio"
test <- merge(test, monthly_returns["2020-01/2022-12"])

table.AnnualizedReturns(test, scale = 12) # %>% gt(rownames_to_stub = T)
charts.PerformanceSummary(R = test)








