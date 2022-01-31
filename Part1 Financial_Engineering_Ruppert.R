library(zoo)
library(quantmod)
library(ggplot2)
library(tidyverse)
library(xts)
library(dplyr)
getSymbols("GOOG",from='2010-01-01',to='2022-01-01',auto.assign = TRUE)
getSymbols("FB",from='2010-01-01',to='2022-01-01',auto.assign = TRUE)

close_yesterday_g <- lag.xts(GOOG$GOOG.Adjusted, 1, na.pad=TRUE)
GOOG$DailyReturn <- 100.0 * (GOOG$GOOG.Close - close_yesterday_g) / close_yesterday_g
close_yesterday_f<-lag.xts(FB$FB.Adjusted,1,na.pad=TRUE)
FB$DailyReturn<-100*(FB$FB.Close-close_yesterday_f)/close_yesterday_f
#plot prices
plot(GOOG$GOOG.Close, type="l",col="red")
lines(FB$FB.Close,type="l",col="green")

ret_goog=na.omit(GOOG$DailyReturn)
ret_fb=na.omit(FB$DailyReturn)
google_cum=100*cumprod(1+ret_goog/100)

fb_cum=100*cumprod(1+ret_fb/100)
plot(google_cum,type='l',col='red')
lines(fb_cum,type='l',col='green')
cor(GOOG2$DailyReturn,FB$DailyReturn)
cov(GOOG2$DailyReturn,FB$DailyReturn)
GOOG2=window(GOOG,start='2012-05-18')
return=cbind(GOOG2$DailyReturn,FB$DailyReturn)
return=na.omit(return)
cor(return$DailyReturn,return$DailyReturn.1)

#log returns
logret_goog<-log(GOOG2$GOOG.Adjusted/lag.xts(GOOG2$GOOG.Adjusted))
GOOG2$Logret<-logret_goog
logret_fb<-log(FB$FB.Adjusted/lag.xts(FB$FB.Adjusted))
FB$Logret<-logret_fb
logrets<-cbind(logret_goog,logret_fb)
ggplot(GOOG2,aes(DailyReturn,Logret))+geom_line()
GOOG2<-na.omit(GOOG2)
cor(GOOG2$Logret,GOOG2$DailyReturn)

#Repeat with Microsoft and Merck
getSymbols("MSFT",from='2010-01-01',to='2022-01-01',auto.assign = TRUE)
getSymbols("MRK",from='2010-01-01',to='2022-01-01',auto.assign = TRUE)
close_yesterday_MS <- lag.xts(MSFT$MSFT.Adjusted, 1, na.pad=TRUE)
MSFT$DailyReturn <- 100.0 * (MSFT$MSFT.Adjusted - close_yesterday_MS) / close_yesterday_MS
close_yesterday_MK<-lag.xts(MRK$MRK.Adjusted,1,na.pad=TRUE)
MRK$DailyReturn<-100*(MRK$MRK.Adjusted-close_yesterday_MK)/close_yesterday_MK
#plot prices
plot(MSFT$MSFT.Adjusted, type="l",col="red")
lines(MRK$MRK.Adjusted,type="l",col="green")
ret_msft=na.omit(MSFT$DailyReturn)
ret_mk=na.omit(MRK$DailyReturn)
msft_cum=100*cumprod(1+ret_msft/100)
mk_cum=100*cumprod(1+ret_mk/100)
plot(msft_cum,type='l',col='red')
lines(mk_cum,type='l',col='green')
return2=cbind(MSFT$DailyReturn,MRK$DailyReturn)
colnames(return2)<-c("Microsoft_Returns","Merck_Returns")
return2=na.omit(return2)
cor(return2$Microsoft_Returns,return2$Merck_Returns)
return2<-cbind(return2,msft_cum,mk_cum)
colnames(return2)<-c("Microsoft_Returns","Merck_Returns","Microsoft_Cumulative","Merck_Cumulative")
autoplot(return2$Microsoft_Cumulative)+geom_line(aes(y=return2$Merck_Cumulative))

