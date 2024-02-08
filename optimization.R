port_spec <- portfolio.spec(colnames(bluechips_returns))
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

opt <- optimize.portfolio(bluechips_returns,
                          portfolio = port_spec,
                          optimize_method = "ROI",
                          trace = T)
chart.RiskReward(opt,
                 risk.col = "StdDev",
                 return.col = "mean",
                 chart.assets = T)

extractWeights(opt)
chart.Weights(opt)

port_spec
options(scipen=999)
