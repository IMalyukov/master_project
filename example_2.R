data(edhec)
ret <- edhec[,1:6]

p <- portfolio.spec(colnames(ret))
p

p <- add.constraint(portfolio = p,
                            type = "full_investment")
p <- add.constraint(portfolio = p,
                            type = "long_only")
#p <- add.objective(portfolio = p,
                     #      type = "return",
                     #      name = "mean")
p <- add.objective(portfolio = p,
                           type = "risk",
                           name = "StdDev")
p

opt_single <- optimize.portfolio(R = ret,
                                 portfolio = p,
                                 optimize_method = "ROI")

extractWeights(opt_single)
chart.Weights(opt_single)



opt_rebal <- optimize.portfolio.rebalancing(R = ret,
                                            portfolio = p,
                                            optimize_method = "ROI",
                                            rebalance_on = "years",
                                            training_period = 60,
                                            rolling_window = 60)
extractWeights(opt_rebal)
chart.Weights(opt_rebal)

rr <- Return.portfolio(ret, weights = extractWeights(opt_rebal))
charts.PerformanceSummary(rr)

