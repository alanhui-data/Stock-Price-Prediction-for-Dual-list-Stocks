library(zoo)

#read data
setwd("/Users/edgarleung/git/financial_time_series/data")
HK = read.table('HK_HSBC.csv', header = TRUE, sep = ",", dec = ".")
NY = read.table('NY_HSBC.csv', header = TRUE, sep = ",", dec = ".")

HK$Adj.Close = as.numeric(as.character(HK$Adj.Close))
HK$Date <- as.Date(HK$Date)
NY$Date <- as.Date(NY$Date)

#one day lag for US
NY = transform(NY, Date = as.Date(c(NA,Date[2:length(Date)])))

#generate data frame with all Monday to Friday
all_days = data.frame("Date"=seq(as.Date("2000-01-03"), as.Date("2019-11-15"), "days"))
all_days$day <- weekdays(as.Date(all_days$Date))
all_days = all_days[all_days$day!='Sunday' & all_days$day!='Saturday',]

# merge data frames
df = merge(all_days, HK, by="Date", all.x = TRUE)
df = merge(df, NY, by="Date", all.x = TRUE)

names(df) <- c("Date","day","Open.hk","High.hk","Low.hk","Close.hk","Adj.Close.hk","Volume.hk","Open.ny.lag","High.ny.lag","Low.ny.lag","Close.ny.lag","Adj.Close.ny.lag","Volume.ny.lag")

#fill NA by previous value
df = na.locf(df)

write.csv(df,'cleaned_df.csv',row.names=FALSE)