data(EuStockMarkets)
mode(EuStockMarkets)
class(EuStockMarkets)
plot(EuStockMarkets)
colSums(is.na(EuStockMarkets))
#Problem 1 Write a brief description of the time series plots of the four indices.
#Do the series look stationary? Do the fluctuations in the series seem to be of constant size? 
#If not, describe how the volatility fluctuates.
adf.test(EuStockMarkets[,'DAX'])
adf.test(EuStockMarkets[,'SMI'])
adf.test(EuStockMarkets[,'CAC'])
adf.test(EuStockMarkets[,'FTSE'])
ESM<-data.frame(EuStockMarkets)


#Ttherefore the timeseries is statioanry, all of them, All of the four indices have a common plot, there movements are correlated and synchronized.
returns<-(EuStockMarkets-lag(EuStockMarkets))*100/lag(EuStockMarkets)
colnames(returns)<-c('DAX','SMI','CAC','FTSE')
plot(returns)
#Problem2: Write a brief description of the time series plots of the four series of log returns. Do the series look stationary? Do the fluctuations in the series
#seem to be of constant size? If not, describe how the volatility fluctuates.
#logreturns
log_ret<-diff(log(EuStockMarkets))
plot(log_ret)
adf.test(log_ret[,'DAX'])
adf.test(log_ret[,'SMI'])
adf.test(log_ret[,'CAC'])
adf.test(log_ret[,'FTSE'])
par(mfrow=c(2, 2))
for(i in colnames(log_ret))
{
  qqnorm(log_ret[ ,i], datax = T, main = i)
  qqline(log_ret[ ,i], datax = T)
  print(shapiro.test(log_ret[ ,i]))
}
#Shapiro-Wilk Test/QQPlots