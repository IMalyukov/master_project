#library(data.table)
##### Pension funds' assets #####

# Pension funds assets are defined as assets bought with the contributions to
# a pension plan for the exclusive purpose of financing pension plan benefits.
# The pension fund is a pool of assets forming an independent legal entity.
# This indicator is measured in millions of USD or as a percentage of GDP. 
# OECD (2023), Pension funds' assets (indicator).
# doi: 10.1787/d66f4f9f-en (Accessed on 17 December 2023)

##### Import_data #####
pens_funds_assets <- fread("data/oecd_pension_funds_asset.csv") # here are two measures: MLN USD and %, GDP
# summary(pens_funds_assets)
colnames(pens_funds_assets) <- tolower(colnames(pens_funds_assets))

##### Divide initial data set for two ####
pens_asset_mln_usd <- pens_funds_assets[measure == "MLN_USD"]
pens_asset_prc_gdp <- pens_funds_assets[measure == "PC_GDP"]

pens_asset_mln_usd <- pens_asset_mln_usd[,c(1,4,6,7)]
pens_asset_prc_gdp <- pens_asset_prc_gdp[,c(1,4,6,7)]

##### Summary statistics ####
summary(pens_asset_mln_usd)
summary(pens_asset_prc_gdp)

##### wide table for MLN_USD ####
pens_asset_mln_usd_wide <- reshape(
  data = pens_asset_mln_usd,
  idvar = "location",
  v.names = c("value"),
  timevar = "time",
  direction = "wide"
)

colnames(pens_asset_mln_usd_wide) <- gsub("value\\.", "", colnames(pens_asset_mln_usd_wide))

# Extract the columns containing years
year_columns <- grep("^[0-9]{4}$", names(pens_asset_mln_usd_wide), value = TRUE)
year_columns <- sort(year_columns, decreasing = TRUE)

# Sort the columns in descending order
sorted_columns <- c("location", "measure", year_columns)

# Reorder the columns in the data table
pens_asset_mln_usd_wide <- pens_asset_mln_usd_wide[, ..sorted_columns]


##### wide table for MLN_USD ####

pens_asset_prc_gdp_wide <- reshape(
  data = pens_asset_prc_gdp,
  idvar = "location",
  v.names = c("value"),
  timevar = "time",
  direction = "wide"
)

colnames(pens_asset_prc_gdp_wide) <- gsub("value\\.", "", colnames(pens_asset_prc_gdp_wide))

# Extract the columns containing years
year_columns <- grep("^[0-9]{4}$", names(pens_asset_prc_gdp_wide), value = TRUE)
year_columns <- sort(year_columns, decreasing = TRUE)

# Sort the columns in descending order
sorted_columns <- c("location", "measure", year_columns)

# Reorder the columns in the data table
pens_asset_prc_gdp_wide <- pens_asset_prc_gdp_wide[, ..sorted_columns]

###### var removing ####
rm(sorted_columns)
rm(year_columns)
rm(pens_asset_mln_usd)
rm(pens_asset_prc_gdp)
rm(pens_funds_assets)


###### Data analysis #####
summary(pens_asset_mln_usd_wide)
summary(pens_asset_prc_gdp_wide)

##### for Russia #####
rf_pens_assets <- pens_asset_mln_usd_wide[location == "RUS", "2020"] * 71.94 * 1000000 # 6.293933 trln rubles
rf_gdp_2020 <- 106606.6 * 10^9
pens_asset_prc_gdp_wide[location == "RUS", "2020"]
(RF_pens_assets / RF_gdp_2020) * 100

###### Mean for Russia #####





































