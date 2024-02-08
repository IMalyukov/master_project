##### download data_description #####
data_description <- read.xlsx('data/benchmark_data/data_description.xlsx')

##### symbols ####
symbols <- data_description$r_symbols
#symbols <- tolower(symbols)
yahoo <- data_description$r_symbols[data_description$quotes_source == "yahoo finance"]
moex <- data_description$r_symbols[data_description$quotes_source == "moex" & data_description$r_symbols != "MCFTR"]
cbonds <- data_description$r_symbols[data_description$quotes_source == "cbonds"]
investing <- data_description$r_symbols[data_description$quotes_source == "investing"]
cbrf <- data_description$r_symbols[data_description$quotes_source == "cbrf"]

#symbols <- c("MOEXBC", "MCXSM", "MOEXBMI", "MOEX10", "MOEXINN")

#### MOEX. downloading cycle for moex symbols ######
for (i in 1:length(moex)) {
  x <- moex[i]
  x <- tolower(x)
  value <- fread(paste0("data/benchmark_data/",moex[i],".csv"))
  value <- value[,c(3,6,7,8,9,10)]
  value$CLOSE <- as.numeric(gsub(",", ".", value$CLOSE))
  value$OPEN <- as.numeric(gsub(",", ".", value$OPEN))
  value$HIGH <- as.numeric(gsub(",", ".", value$HIGH))
  value$LOW <- as.numeric(gsub(",", ".", value$LOW))
  value$VALUE <- as.numeric(gsub(",", ".", value$VALUE))
  #colnames(value) <- c("DATE", "OPEN", "HIGH", "LOW", "CLOSE", "VOL")
  value$TRADEDATE <- as.Date(as.character(value$TRADEDATE), "%d.%m.%Y")
  value_xts <- xts(value[,-1], order.by = value$TRADEDATE)
  eval(call("<-", as.name(x), value_xts))
}
rm(value)
rm(value_xts)
rm(i)
rm(moex)
rm(x)

#### and separately download  MCFTR (another format of data) ####
mcftr <- fread("data/benchmark_data/MCFTR.csv")
mcftr <- mcftr[,c(3:8)]
mcftr$CLOSE <- as.numeric(gsub(",", ".", mcftr$CLOSE))
mcftr$OPEN <- as.numeric(gsub(",", ".", mcftr$OPEN))
mcftr$HIGH <- as.numeric(gsub(",", ".", mcftr$HIGH))
mcftr$LOW <- as.numeric(gsub(",", ".", mcftr$LOW))
mcftr$VALUE <- as.numeric(gsub(",", ".", mcftr$VALUE))
mcftr$TRADEDATE <- as.Date(as.character(mcftr$TRADEDATE), "%Y-%m-%d")
mcftr <- xts(mcftr[,-1], order.by = mcftr$TRADEDATE)

#### YAHOO #####
for (i in 1:length(yahoo)) {
  x <- yahoo[i]
  x <- tolower(x)
  value <- fread(paste0("data/benchmark_data/",yahoo[i],".csv"))
  value$Date <- as.Date(as.character(value$Date), "%Y-%m-%d")
  value_xts <- xts(value[,-1], order.by = value$Date)
  eval(call("<-", as.name(x), value_xts))
}
rm(value)
rm(value_xts)
rm(yahoo)
rm(i)
rm(x)

#### new downloading cycle (i don't know structure of data) #####
#for (i in 1:length(symbols)) {
#  x <- symbols[i]
#  value <- fread(paste0("data/benchmark_data/",symbols[i],".csv"))
#  x <- tolower(x)
#  eval(call("<-", as.name(x), value))
#}
#rm(value)


###### total xts ####
total_xts <- merge(efa$Close, imoex$CLOSE, join = "inner")
colnames(total_xts) <- c("efa.close", "imoex.close")
total_xts <- merge(total_xts, ewg$Close, join = "inner")
colnames(total_xts) <- c("efa.close", "imoex.close", "ewg.close")
total_xts <- merge(total_xts, ewj$Close, join = "inner")
colnames(total_xts) <- c("efa.close", "imoex.close", "ewg.close", "ewj.close")
total_xts <- merge(total_xts, ewu$Close, join = "inner")
colnames(total_xts) <- c("efa.close", "imoex.close", "ewg.close", "ewj.close",
                         "ewu.close")
total_xts <- merge(total_xts, ezu$Close, join = "inner")
colnames(total_xts) <- c("efa.close", "imoex.close", "ewg.close", "ewj.close",
                         "ewu.close", "ezu.close")
total_xts <- merge(total_xts, gld$Close, join = "inner")
colnames(total_xts) <- c("efa.close", "imoex.close", "ewg.close", "ewj.close",
                         "ewu.close", "ezu.close", "gld.close")
total_xts <- merge(total_xts, ivv$Close, join = "inner")
colnames(total_xts) <- c("efa.close", "imoex.close", "ewg.close", "ewj.close",
                         "ewu.close", "ezu.close", "gld.close", "ivv.close")
total_xts <- merge(total_xts, qqq$Close, join = "inner")
colnames(total_xts) <- c("efa.close", "imoex.close", "ewg.close", "ewj.close",
                         "ewu.close", "ezu.close", "gld.close", "ivv.close",
                         "qqq.close")
total_xts <- merge(total_xts, mcftr$CLOSE, join = "inner")
colnames(total_xts) <- c("efa.close", "imoex.close", "ewg.close", "ewj.close",
                         "ewu.close", "ezu.close", "gld.close", "ivv.close",
                         "qqq.close", "mcftr.close")
total_xts <- merge(total_xts, rucbicp$CLOSE, join = "inner")
colnames(total_xts) <- c("efa.close", "imoex.close", "ewg.close", "ewj.close",
                         "ewu.close", "ezu.close", "gld.close", "ivv.close",
                         "qqq.close", "mcftr.close", "rucbicp.close")
total_xts <- merge(total_xts, rucbitr$CLOSE, join = "inner")
colnames(total_xts) <- c("efa.close", "imoex.close", "ewg.close", "ewj.close",
                         "ewu.close", "ezu.close", "gld.close", "ivv.close",
                         "qqq.close", "mcftr.close", "rucbicp.close", "rucbitr.close")
total_xts <- merge(total_xts, rupai$CLOSE, join = "inner")
colnames(total_xts) <- c("efa.close", "imoex.close", "ewg.close", "ewj.close",
                         "ewu.close", "ezu.close", "gld.close", "ivv.close",
                         "qqq.close", "mcftr.close", "rucbicp.close", "rucbitr.close",
                         "rupai.close")
total_xts <- merge(total_xts, rupmi$CLOSE, join = "inner")
colnames(total_xts) <- c("efa.close", "imoex.close", "ewg.close", "ewj.close",
                         "ewu.close", "ezu.close", "gld.close", "ivv.close",
                         "qqq.close", "mcftr.close", "rucbicp.close", "rucbitr.close",
                         "rupai.close", "rupmi.close")
total_xts <- merge(total_xts, rupci$CLOSE, join = "inner")
colnames(total_xts) <- c("efa.close", "imoex.close", "ewg.close", "ewj.close",
                         "ewu.close", "ezu.close", "gld.close", "ivv.close",
                         "qqq.close", "mcftr.close", "rucbicp.close", "rucbitr.close",
                         "rupai.close", "rupmi.close", "rupci.close")
total_xts <- merge(total_xts, vgk$Close, join = "inner")
colnames(total_xts) <- c("efa.close", "imoex.close", "ewg.close", "ewj.close",
                         "ewu.close", "ezu.close", "gld.close", "ivv.close",
                         "qqq.close", "mcftr.close", "rucbicp.close", "rucbitr.close",
                         "rupai.close", "rupmi.close", "rupci.close",
                         "vgk.close")
total_xts <- merge(total_xts, vea$Close, join = "inner")
colnames(total_xts) <- c("efa.close", "imoex.close", "ewg.close", "ewj.close",
                         "ewu.close", "ezu.close", "gld.close", "ivv.close",
                         "qqq.close", "mcftr.close", "rucbicp.close", "rucbitr.close",
                         "rupai.close", "rupmi.close", "rupci.close",
                         "vgk.close", "vea.close")
total_xts <- merge(total_xts, spy$Close, join = "inner")
colnames(total_xts) <- c("efa.close", "imoex.close", "ewg.close", "ewj.close",
                         "ewu.close", "ezu.close", "gld.close", "ivv.close",
                         "qqq.close", "mcftr.close", "rucbicp.close", "rucbitr.close",
                         "rupai.close", "rupmi.close", "rupci.close",
                         "vgk.close", "vea.close", "spy.close")
total_xts <- merge(total_xts, slv$Close, join = "inner")
colnames(total_xts) <- c("efa.close", "imoex.close", "ewg.close", "ewj.close",
                         "ewu.close", "ezu.close", "gld.close", "ivv.close",
                         "qqq.close", "mcftr.close", "rucbicp.close", "rucbitr.close",
                         "rupai.close", "rupmi.close", "rupci.close",
                         "vgk.close", "vea.close", "spy.close", "slv.close")

















chart.TimeSeries(total_xts)
return.dataset <- Return.calculate(total_xts[-1,])
return.dataset <- return.dataset[-1,]

chart.TimeSeries(return.dataset)

corrplot(cor(return.dataset), method = "number")
chart.Correlation(return.dataset)

