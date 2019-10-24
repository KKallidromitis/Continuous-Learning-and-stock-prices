# Continuous-Learning-and-stock-prices
The LSTM model is used to predict the stock prices of Apple Inc. The data are collected from Yahoo finance, and include the stock prices since the current CEO Tim Cook took over on 24th August 2011. The model is constantly trained on updated data from the [Alphavantage finance API](https://www.alphavantage.co). Website is hosted on a Django webapp in AWS.


Currently, there are major innacuracies since the stock prices depends on a plethora of factors such as product launches and scandals which are not being considered. A future improvement is to combine the pricing model with an improvement of the NLP one I made in the past to overcome those weaknesses.
