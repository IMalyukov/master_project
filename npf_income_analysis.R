###### npf income returns #######
npf_income_dataset <- fread("data/npf_income_data.csv")
colnames(npf_income_dataset) <- c("id",
                                  "npf_id",
                                  "date",
                                  "income",
                                  "name",
                                  "income_per")
npf_income_dataset$income <- npf_income_dataset$income / 100


##### to_wide ####
wide_npf_income_dataset <- reshape(
                                  npf_income_dataset[,c(2,3,4)],
                                  idvar = "date",
                                  v.names = c("income"),
                                  timevar = "npf_id",
                                  direction = "wide"
                                   )
# par(mar = c(1, 1, 1, 1))
plot(wide_npf_income_dataset)
head(wide_npf_income_dataset)
typeof(npf_income_dataset$date)


##### 
xts_npf_income <- xts(wide_npf_income_dataset, order.by=as.Date(wide_npf_income_dataset$date))[,-1]
head(xts_npf_income)
plot(as.xts(xts_npf_income[c(8:44),c(4:5)]))

