
for (i in 0:10) {
  dataset_name <- paste0("NPF_", i)
  data_source <- paste0("data/npf_data/", dataset_name, ".csv")
  dataset <- fread(data_source)
  eval(call("<-", as.name(tolower(dataset_name)), dataset))
}
rm(dataset)
rm(data_source)
rm(i)
rm(dataset_name)

npf_cbonds <- fread('data/npf_data/npf_indicators_for_pension_rankings.csv')
View(npf_cbonds)
npf_indicators <- fread('data/npf_data/npf_indicators.csv')
View(npf_indicators)
gc()
npf_cbonds <- fread('data/npf_data/npf_indicators_for_pension_rankings.csv')
View(npf_cbonds)
help(fread)
npf_indicators <- fread('data/npf_data/npf_indicators.csv')
View(npf_indicators)
npf_indicators <- fread('data/npf_data/npf_indicators.csv', select = c(1:4, 13, 14))
View(npf_indicators)
summary(npf_indicators)
npf_indicators$pension_savings_yield <- as.numeric(npf_indicators$pension_savings_yield)
View(npf_indicators)
npf_indicators[!is.na(npf_indicators$pension_savings_yield)]
head(npf_indicators[!is.na(npf_indicators$pension_savings_yield)])
npf_indicators$pension_reserves_yield <- as.numeric(npf_indicators$pension_reserves_yield)
head(npf_indicators[!is.na(npf_indicators$pension_savings_yield)])
View(npf_indicators)
View(npf_indicators)
npf_cbonds <- fread('data/npf_data/npf_indicators_for_pension_rankings.csv')
View(npf_cbonds)
colnames(npf_cbonds)
View(npf_cbonds)
npf_income <- npf_cbonds[c(1:5)]
npf_income <- npf_cbonds[,c(1:5)]
View(npf_income)
write.csv(npf_income, file = "npf_income.csv")
?write.csv
write.csv(npf_income, file = "npf_income.csv", row.names = FALSE)
npf_income$pension_savings_yield <- as.numeric(npf_income$pension_savings_yield)
npf_income$pension_reserves_yield <- as.numeric(npf_income$pension_reserves_yield)
write.csv(npf_income, file = "npf_income.csv", row.names = FALSE)
for (i in 0:10) {
  dataset_name <- paste0("NPF_", i)
  data_source <- paste0("data/npf_data/", dataset_name, ".csv")
  dataset <- fread(data_source)
  eval(call("<-", as.name(tolower(dataset_name)), dataset))
}
View(npf_10)
View(npf_1)
View(npf_6)
View(npf_8)
View(npf_6)
npf_indicators <- fread(
  'data/npf_data/npf_indicators.csv',
  select = c(1:4, 13, 14) #import necessary columns
)
View(npf_indicators)
npf_indicators_write <- npf_indicators_write[,c(1,2,5,6)]
npf_indicators_write <- npf_indicators[,c(1,2,5,6)]
View(npf_indicators_write)
write.csv(npf_indicators_write, file = "npf.csv")
View(npf_indicators_write)

npf_main <- fread("data/npf_data/npf_main_1.csv")

colnames(npf_main)
npf_main_need <- npf_main[,c('id', 'name', 'license', 'new_npf_id')]

npf_main_need <- npf_main[,c('name', 'license', 'new_npf_id')]

write.csv(npf_main_need, file = "npf_main_need.csv", row.names = FALSE)

head(npf_6)

npf_6$date <- as.Date(paste0(npf_6$year, "-12-31"))

npf_8$date <- as.Date(paste0(npf_8$year, "-12-31"))
npf_6_sql <- npf_6[,c(1,6,3:5)]

npf_8_sql <- npf_8[,c(1,5,3,4)]

#####
npf_6_sql$savings_gross_return <- as.numeric(npf_6_sql$savings_gross_return)
npf_6_sql$savings_gross_return_anpf_pac <- as.numeric(npf_6_sql$savings_gross_return_anpf_pac)
npf_6_sql$savings_net_return <- as.numeric(npf_6_sql$savings_net_return)
npf_6_sql



npf_cbonds <- fread('data/npf_data/npf_indicators_for_pension_rankings.csv')
npf_income <- npf_cbonds[,c(1:5)]

npf_income$pension_savings_yield <- as.numeric(npf_income$pension_savings_yield)
npf_income$pension_reserves_yield <- as.numeric(npf_income$pension_reserves_yield)



##### add month and day to year #####
npf_6$date <- as.Date(paste0(npf_6$year, "-12-31"))
npf_8$date <- as.Date(paste0(npf_8$year, "-12-31"))
#####
npf_6_sql <- npf_6[,c(1,6,3:5)]
npf_8_sql <- npf_8[,c(1,5,3,4)]

#####
npf_6_sql$savings_gross_return <- as.numeric(sub(",", ".", npf_6_sql$savings_gross_return))
View(npf_6)
View(npf_6_sql)
npf_6_sql$savings_gross_return_anpf_pac <- as.numeric(sub(",", ".", npf_6_sql$savings_gross_return_anpf_pac))
View(npf_6_sql)
npf_6_sql$savings_net_return <- as.numeric(sub(",", ".", npf_6_sql$savings_net_return))
npf_6_sql
View(npf_6_sql)

#####
npf_8_sql$reserves_gross_return <- as.numeric(sub(",", ".", npf_8_sql$reserves_gross_return))
npf_8_sql$reserves_net_return <- as.numeric(sub(",", ".", npf_8_sql$reserves_net_return))
write.csv(npf_8_sql, file = "npf_8_sql.csv")

write.csv(npf_6_sql, file = "npf_6_sql.csv")

colnames(npf_6_sql)
colnames(npf_8_sql)
##### join_incomes #####
join_incomes <- fread('data/npf_data/join_incomes.csv')
##### join_incomes #####
join_incomes <- fread('data/npf_data/join_incomes.csv')
View(join_incomes)
summary(join_incomes)

##### join_incomes #####
join_incomes <- read.csv('data/npf_data/join_incomes.csv')
summary(join_incomes)
#join_incomes$date <- as.Date(join_incomes$date)
join_incomes$date_cb <- as.Date(join_incomes$date_cb)
join_incomes$savings_gross_return <- as.numeric(join_incomes$savings_gross_return)
join_incomes$savings_gross_return_anpf_pac <- as.numeric(join_incomes$savings_gross_return_anpf_pac)
join_incomes$savings_yield_cb <- as.numeric(join_incomes$savings_yield_cb)
join_incomes$reserves_yield_cb <- as.numeric(join_incomes$reserves_yield_cb)
summary(join_incomes)




##### without full nulls in income ####
income_without_full_nulls <- read.csv('data/npf_data/income_without_full_nulls.csv')

matches <- income_without_full_nulls[
  income_without_full_nulls$savings_gross_return == income_without_full_nulls$savings_yield_cb,
]

non_matches <- income_without_full_nulls[
  income_without_full_nulls$savings_gross_return != income_without_full_nulls$savings_yield_cb,
]

##### 
join_incomes <- as.data.table(join_incomes)
table_2011 <- join_incomes[date_cb >= as.Date('2011-01-01') & date_cb <= as.Date('2011-12-31')]



join_incomes$license_cb
















