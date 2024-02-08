port_spec <- portfolio.spec(colnames(train_data))
port_spec

port_spec <- add.constraint(portfolio = port_spec,
                            type = "full_investment")
port_spec <- add.constraint(portfolio = port_spec,
                            type = "long_only")
port_spec <- add.constraint(portfolio = port_spec,
                            type = "weight_sum",
                            min_sum = 1,
                            max_sum = 1)
port_spec <- add.constraint(portfolio = port_spec,
                            type = "box",
                            min = c(rep(0.05, 3),
                                    rep(0.05, 3)),
                            max = 0.6)
port_spec <- add.constraint(portfolio = port_spec,
                            type = "group",
                            groups = list(c(1,2,3),
                                          c(4,5,6)),
                            group_min = 0.4, group_max = 0.6)
port_spec <- add.objective(portfolio = port_spec,
                           type="quadratic_utility", 
                             risk_aversion= 0.75)

opt <- optimize.portfolio(train_data,
                          portfolio = port_spec,
                          optimize_method = "random",
                          search_size = 2000,
                          trace = T)

chart.RiskReward(opt,
                 risk.col = "StdDev",
                 return.col = "mean",
                 chart.assets = T)

extractWeights(opt)
chart.Weights(opt)

#chart.RiskBudget(opt, match.col = "StdDev", risk.type = "percentage")

meansd.ef <- create.EfficientFrontier(R = train_data,
                                      portfolio = port_spec,
                                      type = "mean-sd",
                                      n.portfolios = 500
)


chart.EfficientFrontier(meansd.ef,
                        type="l",
                        match.col = "StdDev", n.portfolios = 500, xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)
