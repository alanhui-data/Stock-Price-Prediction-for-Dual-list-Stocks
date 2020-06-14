#install.packages("tidyverse")
library(tidyverse)
library(fUnitRoots)
library(fGarch)
library(rugarch)


#read data
setwd("/Users/edgarleung/git/financial_time_series/data")
df = read.table('cleaned_df.csv', header = TRUE, sep = ",", dec = ".")

df$Date <- as.Date(df$Date)

######################################
#Start Modelling
######################################
ts.plot(cbind(df$Adj.Close.hk, df$Adj.Close.ny.lag), gpars = list(col = c("black", "red")))
ts.plot(cbind(diff(df$Adj.Close.hk), diff(df$Adj.Close.ny.lag)), gpars = list(col = c("black", "red")))

ts.plot(diff(log(df$Adj.Close.hk)))
#ts.plot(diff(df$Adj.Close.hk))
df$Adj.Close.hk.log = log(df$Adj.Close.hk)

# Augmented Dickey-Fuller Test
unitrootTest(df$Adj.Close.hk,lags=1,type=c("c"))
# p-value > 0.05
# non-stationary -> take diff
unitrootTest(diff(df$Adj.Close.hk.log),lags=1,type=c("c"))
# p-value < 0.05
# stationary

#### first model: ARIMA(0,1,0)
arima010 = arima(df$Adj.Close.hk.log,order=c(0,1,0))
acf(arima010$residuals,65,ylim=c(-0.1,0.1))
pacf(arima010$residuals,90)

Box.test(arima010$residuals,lag=5,type="Ljung")
Box.test(arima010$residuals,lag=10,type="Ljung")
Box.test(arima010$residuals,lag=15,type="Ljung")
Box.test(arima010$residuals,lag=20,type="Ljung")
Box.test(arima010$residuals,lag=25,type="Ljung")
Box.test(arima010$residuals,lag=30,type="Ljung")
Box.test(arima010$residuals,lag=35,type="Ljung")
# still significant


########### try Seasonal ARIMA #######
### FAILED ####
arima010_200_30 = arima(df$Adj.Close.hk.log, c(0, 1, 0), seasonal = list(order = c(2,0,0), period = 30))
acf(arima010_200_30$residuals,90,ylim=c(-0.1,0.1))
pacf(arima010_200_30$residuals,90)

Box.test(arima010_200_30$residuals,lag=10,type="Ljung")
Box.test(arima010_200_30$residuals,lag=20,type="Ljung")
# still significant
#######################################


# test GARCH effect
acf(arima010$residuals^2,35)
pacf(arima010$residuals^2,35)
Box.test(arima010$residuals^2,lag=31,type="Ljung")


#fit GARCH(1,1) model
garch11=garchFit(~garch(1,1),data=diff(df$Adj.Close.hk.log)*100,trace=F)
summary(garch11)
# mu is not significant

#fit GARCH(1,1) model with no mu
garch11_nomean=garchFit(~garch(1,1),data=diff(df$Adj.Close.hk.log)*100,trace=F,include.mean = FALSE)
summary(garch11_nomean)
#alpha + beta close to 1


#fit iGARCH(1,1) model without mean
igarch11=ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1))
                    ,mean.model=list(armaOrder=c(0,0),include.mean = FALSE) 
)
igarch11_dex=ugarchfit(spec=igarch11,data=diff(df$Adj.Close.hk.log)*100)
igarch11_dex  ### see output


res=residuals(igarch11_dex,standardize=T)

Box.test(res,10,type="Ljung")
Box.test(res,20,type="Ljung")
Box.test(res,30,type="Ljung")
Box.test(res,40,type="Ljung")
Box.test(res,60,type="Ljung")
Box.test(res,90,type="Ljung")

Box.test(res^2,10,type="Ljung")
Box.test(res^2,20,type="Ljung")
Box.test(res^2,30,type="Ljung")
Box.test(res^2,40,type="Ljung")
Box.test(res^2,60,type="Ljung")
Box.test(res^2,90,type="Ljung")

forcast_value = c()
forecast_period = 34

for (val_date in tail(df$Date,forecast_period)) {
  train_df = df[df$Date < val_date,]
  
  igarch11=ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1))
                      ,mean.model=list(armaOrder=c(0,0),include.mean = FALSE) 
  )
  igarch11_dex=ugarchfit(spec=igarch11,data=diff(train_df$Adj.Close.hk.log)*100)
  
  fore = ugarchforecast(igarch11_dex, n.ahead = 1)
  
  forcast_value = c(forcast_value, exp(
    tail(df[df$Date < val_date,],1)$Adj.Close.hk.log
    + fitted(fore)[1]/100
  )
  )
}

df$forcast_value = NA
df[df$Date >= tail(df$Date,forecast_period)[1],]$forcast_value = forcast_value

#RMSE
(mean(((df[df$Date >= tail(df$Date,forecast_period)[1],]$forcast_value - df[df$Date >= tail(df$Date,forecast_period)[1],]$Adj.Close.hk)^2)))**(1/2)
#0.6120394


#########################################
# use US time series to predict HK
#########################################
# linear model
linearMod <- lm(df$Adj.Close.hk.log ~ log(df$Adj.Close.ny.lag))
ts.plot(linearMod$residuals)

# differencing
linearMod <- lm(diff(df$Adj.Close.hk.log) ~ diff(log(df$Adj.Close.ny.lag)))
ts.plot(linearMod$residuals)
acf(linearMod$residuals)
pacf(linearMod$residuals)


# ARIMAX(0,1,1)
arima011x = arima(tail(df$Adj.Close.hk.log, 5100),order=c(0,1,1),xreg=tail(diff(log(df$Adj.Close.ny.lag)), 5100))
ts.plot(arima011x$residuals)
acf(arima011x$residuals)
pacf(arima011x$residuals,90)

acf(arima011x$residuals^2)
pacf(arima011x$residuals^2)


# ARIMAX(0,1,1)+GARCH(1,1)
arimax011_garch11=ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1))
                    ,mean.model=list(armaOrder=c(0,1),include.mean = FALSE, external.regressors = matrix(diff(log(df$Adj.Close.ny.lag))*100)) 
)
arimax011_garch11_dex=ugarchfit(spec=arimax011_garch11,data=diff(df$Adj.Close.hk.log)*100)
arimax011_garch11_dex  ### see output

# ARIMAX(0,1,1)+iGARCH(1,1)
arimax011_igarch11=ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1))
                             ,mean.model=list(armaOrder=c(0,1),include.mean = FALSE, external.regressors = matrix(diff(log(df$Adj.Close.ny.lag))*100)) 
)
arimax011_igarch11_dex=ugarchfit(spec=arimax011_igarch11,data=diff(df$Adj.Close.hk.log)*100)
arimax011_igarch11_dex  ### see output

res=residuals(arimax011_igarch11_dex,standardize=T)
ts.plot(res)
acf(res)
pacf(res,90)

acf(res^2)
pacf(res^2,90)


Box.test(res,10,type="Ljung")
Box.test(res,20,type="Ljung")
Box.test(res,30,type="Ljung")
Box.test(res,40,type="Ljung")
Box.test(res,60,type="Ljung")
Box.test(res,90,type="Ljung")

Box.test(res^2,10,type="Ljung")
Box.test(res^2,20,type="Ljung")
Box.test(res^2,30,type="Ljung")
Box.test(res^2,40,type="Ljung")
Box.test(res^2,60,type="Ljung")
Box.test(res^2,90,type="Ljung")

#overfit
# ARIMAX(1,1,1)+iGARCH(1,1)
arimax111_igarch11=ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1))
                              ,mean.model=list(armaOrder=c(1,1),include.mean = FALSE, external.regressors = matrix(diff(log(df$Adj.Close.ny.lag))*100)) 
)
arimax111_igarch11_dex=ugarchfit(spec=arimax111_igarch11,data=diff(df$Adj.Close.hk.log)*100)
arimax111_igarch11_dex  ### see output

validation_rmse <- function(model_df, forecast_period=34, training_period=120) {
  forcast_value = c()
  for (val_date in tail(model_df$Date,forecast_period)) {
    train_df = tail(model_df[model_df$Date < val_date,],training_period)
    xreg = data.matrix(train_df[-4:-1])
    
    arimax011_igarch11=ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1))
                                  ,mean.model=list(armaOrder=c(0,1),include.mean = FALSE, external.regressors = xreg) 
    )
    arimax011_igarch11_dex=ugarchfit(spec=arimax011_igarch11,data=train_df$y_hk)
    
    fore = ugarchforecast(arimax011_igarch11_dex, n.ahead = 1)
    
    forcast_value = c(forcast_value, exp(
      tail(model_df[model_df$Date < val_date,],1)$Adj.Close.hk.log
      + fitted(fore)[1]/100
    )
    )
    
  }
  
  model_df$forcast_value = NA
  model_df[model_df$Date >= tail(model_df$Date,forecast_period)[1],]$forcast_value = forcast_value
  #RMSE
  return (list(model_df, (mean(((model_df[model_df$Date >= tail(model_df$Date,forecast_period)[1],]$forcast_value - model_df[model_df$Date >= tail(model_df$Date,forecast_period)[1],]$Adj.Close.hk)^2)))**(1/2) ))
}

train_period = 5100
y_df = tail(data.frame(df['Date'],df['Adj.Close.hk'],df['Adj.Close.hk.log']),train_period)
y_hk = tail(diff(df$Adj.Close.hk.log)*100,train_period)
x_ny = tail(diff(log(df$Adj.Close.ny.lag)),train_period)

model_df = cbind.data.frame(y_df, y_hk, x_ny)


# find optimal training period
rmse = c()
for (each_training_period in seq(600, 1000, by=20)){
  result = validation_rmse(model_df=model_df, training_period=each_training_period)
  rmse = c(rmse, result[[2]])
}
plot(rmse ~ seq(600, 1000, by=20),type="l", xlab="training period",ylab="RMSE")



result = validation_rmse(model_df=model_df, training_period=820)
#RMSE
result[[2]]
#0.4933729
result_df = result[[1]]

#RMSE for naive model
(mean(tail(diff(result_df$Adj.Close.hk)^2,34)))**(1/2)
#0.5755835

ggplot() + 
  geom_line(data = result_df[result_df$Date >= tail(result_df$Date,forecast_period)[1],], aes(x = Date, y = Adj.Close.hk, color = "Actual"), size=1) +
  geom_line(data = result_df[result_df$Date >= tail(result_df$Date,forecast_period)[1],], aes(x = Date, y = forcast_value, color = "Predict"), size=1) +
  scale_color_manual(values = c(
    'Actual' = 'darkblue',
    'Predict' = 'red')) +
  labs(color = '') +
  xlab('Date') +
  ylab('closing')


comparision_df = result_df[result_df$Date >= tail(result_df$Date,forecast_period)[1],][-3:-5]

#try more variables
len = length(diff(df$Adj.Close.hk.log))

x_hk_vol_lag = tail(diff(df$Volume.hk)[1:(len-1)],train_period)
x_hk_gap_lag = tail(diff(df$High.hk - df$Low.hk)[1:(len-1)],train_period)

x_ny = tail(diff(log(df$Adj.Close.ny.lag)),train_period)
x_ny_vol = tail(diff(log(df$Volume.ny.lag)),train_period)
x_ny_gap = tail(diff(log(df$High.ny.lag - df$Low.ny.lag)),train_period)

x_ny_lag = tail(diff(log(df$Adj.Close.ny.lag))[1:(len-1)],train_period)
x_ny_vol_lag = tail(diff(log(df$Volume.ny.lag))[1:(len-1)],train_period)
x_ny_gap_lag = tail(diff(log(df$High.ny.lag - df$Low.ny.lag))[1:(len-1)],train_period)



#model_df = cbind.data.frame(y_df, y_hk, x_ny, x_ny_vol, x_ny_gap)
model_df = cbind.data.frame(y_df, y_hk, x_ny, x_ny_vol, x_ny_gap, x_ny_lag, x_ny_vol_lag, x_ny_gap_lag)
result = validation_rmse(model_df=model_df, training_period=820)
#RMSE
result[[2]]
# 0.5278344 not helping
result_df = result[[1]]


comparision_df = merge(comparision_df, result_df[result_df$Date >= tail(result_df$Date,forecast_period)[1],][-2:-10], by='Date')
colnames(comparision_df) = c('Date','Adj.Close.hk','forecast_with_1_xreg','forecast_with_6_xreg')

write.csv(comparision_df,'arima_result.csv')

