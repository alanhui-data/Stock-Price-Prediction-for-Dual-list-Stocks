# Stock Price Prediction for Dual-list Stocks in Hong Kong and New York
Detailed paper refers to Dual List Stock Prediction Project Paper.pdf

Presentation refers to https://youtu.be/BnaqN3CDniQ

## ABSTRACT
We selected a popular stock in Hong Kong stock market, which is HSBC stock (0005.HK). This stock is cross-listed on exchanges in Hong Kong and New York. We studied the patterns of information flow for HSBC stocks, trying to find out the relationship between these dual-listed stocks. Results showing that the offshore (New York) markets has significant impact on the domestic (Hong Kong) markets. We can use offshore closing price to predict the trend of domestic markets of the next day. We used 2 different approaches to fit a prediction model, ARIMA and LSTM models. Results showing that ARIMA model is more reliable than LSTM on this stock price prediction.


## DATA DESCRIPTION
To start with this study, we extracted 2 datasets, HSBC HK history stock price and HSBC US history stock price from yahoo finance [2], data time range start from Jan-2000 to Nov-2019.

 
In this data, we have 6 data fields:
-          Open: this is the stock price of HSBC when the stock market open for trading
-          High: this is the highest bid cost of the day
-          Low: this is the lowest ask cost of the day
-          Close: this is the stock price of HSBC when the stock market close for trading (with adjustment for splits)
-          Adj. Close: this is the adjusted close price, adjusted for dividends and splits
-          Volume: this is the transaction volume

We split our dataset into training data and test data. We set a common cutoff date for both models, which is 1-Oct-2019. All data before this cutoff date is used as training data. 

##3	ARIMA-GARCH MODEL ON THE TIME SERIES ITSELF
Refer to code.R

## LSTM MODEL
Refer to LSTM Stock Prediction Train and Predict.ipynb


## CONCLUSION
There were studies on LSTM achieved better results than  ARIMA.  However, in our case, ARIMA is clearly the better model.

●	Simple data series and relationship
Neural networks works great in identifying the unobservable or stochastic data dependencies.  In our case, the data series is rather simple.  A direct information transmission from the close price in US stock market to that of HK stock market is also observed.  We are able to tailor made configuration on ARIMA model for this specific HSBC stock which yield a more accurate result.

●	Short prediction period
Since it is a daily stock price prediction problem, it has to adopt walk-forward prediction approach to re-train the model and predict the next day price.  Since we only predict next day price, ARIMA works better.  Some research did find a conclusion that LSTM can have a better prediction accuracy when the prediction period is long.

●	Data volume and Training time
LSTM model requires a large dataset and long training time to train the model up to standard.  In our case, we spent around 4~5 hours to train the LSTM model.  It is a big advantage of ARIMA model over LSTM model.

We cannot draw a simple conclusion here that ARIMA model is better than LSTM model in all senarios.  Experiments needed to be carried out carefully each time in order to choose the best model for your particular data series and use cases.
