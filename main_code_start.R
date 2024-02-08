

data(edhec)
data <- edhec[,1:8]
?edhec
port_spec <- portfolio.spec(colnames(data))
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

opt <- optimize.portfolio(data,
                          portfolio = port_spec,
                          optimize_method = "random",
                          trace = T)
chart.RiskReward(opt,
                 risk.col = "StdDev",
                 return.col = "mean",
                 chart.assets = T)

extractWeights(opt)
chart.Weights(opt)


#### index_optimization

# Load the data
data(indexes)

# Subset the data
index_returns <- indexes[,c(1:4)]

# Print the head of the data
head(index_returns)

# Create the portfolio specification
port_spec <- portfolio.spec(colnames(index_returns))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

# Solve the optimization problem
opt <- optimize.portfolio(index_returns,
                          portfolio = port_spec,
                          optimize_method = "ROI",
                          trace = T)

# Print the results of the optimization
print(opt)

# Extract the optimal weights
extractWeights(opt)

# Chart the optimal weights
chart.Weights(opt)

chart.RiskReward(opt,
                 risk.col = "StdDev",
                 return.col = "mean",
                 chart.assets = T)

######### Attempt ##############
# Create the portfolio specification
port_spec <- portfolio.spec(assets = colnames(index_returns))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to maximize portfolio mean return
port_spec <- add.objective(portfolio = port_spec, type = "return", name = "mean")

# Add an objective to minimize portfolio variance
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "var", risk_aversion = 10)

# Solve the optimization problem
opt <- optimize.portfolio(R = index_returns, portfolio = port_spec, optimize_method = "ROI", trace = T)

opt

# Print the results of the optimization
print(opt)

# Extract the optimal weights
extractWeights(opt)

# Chart the optimal weights
chart.Weights(opt)

chart.RiskReward(opt,
                 risk.col = "var",
                 return.col = "mean",
                 chart.assets = T)





