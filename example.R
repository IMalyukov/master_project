install.packages("NMOF")
install.packages("datetimeutils")

library("NMOF")
library("PortfolioAnalytics")
library("xts")

R <- French(tempdir(), "17_Industry_Portfolios_CSV.zip")
R <- as.xts(R, as.Date(row.names(R)))
R <- window(R, start = as.Date("2000-01-01"))

retornos_categorias <- R
assets <- colnames(retornos_categorias)
portfolio.init <- portfolio.spec(assets)
portfolio.init <- add.constraint(portfolio.init,
                                 type = "full_investment")
portfolio.init <- add.constraint(portfolio.init,
                                 type = "long_only")
portfolio.minSD <- add.objective(portfolio = portfolio.init,
                                 type = "risk",
                                 name = "StdDev")

portfolio.minSD.opt <- optimize.portfolio(retornos_categorias,
                                          portfolio = portfolio.minSD,
                                          optimize_method = "ROI",
                                          trace = TRUE)
print(portfolio.minSD.opt)

# Extract the optimal weights
extractWeights(portfolio.minSD.opt)


# Chart the optimal weights
chart.Weights(portfolio.minSD.opt)

chart.RiskReward(portfolio.minSD.opt,
                 risk.col = "StdDev",
                 return.col = "mean",
                 chart.assets = T)
