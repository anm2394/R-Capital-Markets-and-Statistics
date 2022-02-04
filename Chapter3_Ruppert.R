#function to estimate the bond value, where the r is semi annual rate of interest and so is the coupon paid, the Time is taken twice 
bondval<-function(c,t, r, par){
  bv=c/r+(par-c/r)*(1+r)^(-2*t)
  return(bv)
}
par=100 #par value
C=5  #semi annual coupon rate
t=30 #total to maturity
price=110 #price traded
r=seq(0.02,0.05,length=300) #half yearly rates
value=bondval(C,t,r,par)
yield2mat=spline(value,r,xout=price)
#Chapter 3: Problem #1Solve the YTM graphically 
plot(r,value,xlab='Yield to Maturity',ylab='Price of Bond')
abline(h=110,col='red')
abline(v=yield2mat,col='green')
#Problem 2: what does uniroot(function(r) r^2 - 0.5, c(0.7, 0.8)) do?
soln<-uniroot(function(r) r^2 - 0.5, c(0.7, 0.8))
y2m=2*soln$root
sprintf('The YTM annual is %f',y2m)
#it finds out the square root of 0.5

#Problem 3: Use uniroot() to find the yield to maturity of the 30-year par
#$1,000 bond with coupon payments of $40 that is selling at $1,200.

#Solution
#Running on original data
soln<-uniroot(function(r) price-C/r+(par-C/r)*(1+r)^-(2*t), c(0.02, 0.05))
y2m=2*soln$root
sprintf('The YTM annual is %f',y2m)
#defining the values as per the question
price=1200
C=40
par=1000
t=30
soln<-uniroot(function(r) price-C/r+(par-C/r)*(1+r)^-(2*t), c(0.02, 0.05))
soln$root
#Problem 4: Find the yield to maturity of a par $10,000 bond selling at $9,800 with semiannual coupon payments equal to $280 and maturing in 8 years.
price=9800
C=280
par=10000
t=8
soln<-uniroot(function(r) price-C/r+(par-C/r)*(1+r)^-(2*t), c(0.02, 0.05))
y2m<-2*soln$root
sprintf('The yield is %f',y2m)

#Problem #5: Use uniroot() to find the yield to maturity of the 20-year par $1,000 bond with semiannual coupon payments of $35 that is selling at $1,050.
price=1050
C=35
par=1000
t=20
soln<-uniroot(function(r) price-C/r+(par-C/r)*(1+r)^-(2*t), c(0.02, 0.05))
y2m<-2*soln$root
sprintf('The yield to maturity is %f',y2m)
#Problem #6: The yield to maturity is 0.035 on a par $1,000 bond selling at $950.10 and maturing in 5 years. What is the coupon payment?
price=950.1
r=0.035
par=1000
t=5
annual_soln<-uniroot(function(C) price-C/r+(par-C/r)*(1+r)^-(t), c(0,100))
annual_soln$root
semiannual_soln<-uniroot(function(C) price-C/r+(par-C/r)*(1+r)^-(2*t),c(0,100))
semiannual_soln$root


#Zero coupon yields
library(YieldCurve)
data("FedYieldCurve")
head(FedYieldCurve)
dates<-c("2000-01-01","2010-01-01")
rates<-FedYieldCurve["2000"]
autoplot(rates)
autoplot(rates, facets=FALSE)
last(rates)
plot_grid(rates$R_3M,rates$R_6M,rates$R_1Y,rates$R_2Y,rates$R_3Y)
#random stuff
cat('Price ',price)
sprintf('any value %d',price)
res<-price-C/r+(par-C/r)*(1+r)^-(2*t)
plot(res)
plot(value,)