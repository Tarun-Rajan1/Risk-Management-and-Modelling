rm(list = ls()) # clear all

#install.packages("knitr")
#install.packages("rmarkdown")
#install.packages("markdown")
#install.packages("tidyverse")
#install.packages("PerformanceAnalytics")
#install.packages("timeSeries")
#install.packages("tseries")
#install.packages("roll")
#install.packages("car")
#install.packages("MASS")
#install.packages("extraDistr")
#install.packages("rugarch")
#install.packages("QRM")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("lubridate")

library(moments)
library(knitr)
library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(timeSeries)
library(tseries)
library(roll)
library(car)
library(MASS)
library(extraDistr)
library(rugarch)
library(QRM)
library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)


setwd("/Users/tarunrajan/Library/Mobile Documents/com~apple~CloudDocs/FM321")

load('Returns.RData')
load('Prices.RData')

date.ts <- ymd(Returns$date)

plot(date.ts, Returns$JPM,
     type = "l",
     main = "Compound returns for JP Morgan",
     ylab = "Returns",
     xlab = "Date",
     col = "red",
     las = 1
)

plot(date.ts,Prices$JPM,type='l',
     main = "Compound returns for JP Morgan",
     ylab = "Price",
     xlab = "Date",
     col = "green",
     las = 1
)

plot(date.ts,Prices$JPM,type='l',log='y',
     main = "Compound returns for JP Morgan - logarithmic scale",
     ylab = "Price",
     xlab = "Date",
     col = "green",
     las = 1
)

plot(date.ts,log(Prices$JPM),type='l',
     main = "Compound returns for JP Morgan - logarithmic scale",
     ylab = "Price",
     xlab = "Date",
     col = "green",
     las = 1)

n=names(Returns)
n=n[2:length(n)]
n
for(i in n){  # i takes the values/names of the columns 
  x=Returns[[i]] # it extracts the data of column i from the Returns dataframe
  cat(i,mean(x),sd(x),min(x),max(x),"\n")  # "\n" newline character
}

df=matrix(ncol=4,nrow=length(n)) # creates a n x 4 matrix
n=names(Returns)
n=n[2:length(n)]
for(i in 1:length(n)){
  x=Returns[[n[i]]]
  df[i,]=c(mean(x),sd(x),min(x),max(x)) # concatenates the mean, sd, min and max into a  1 x 4 row vector
}
df=as.data.frame(df)  #  Converts the matrix df into a dataframe.
df=cbind(n,df*100)    # concatenates horizontally the vector with names (n) and df
names(df)=c("Asset","mean","sd","min","max")
kable(df,digits=3,caption="Sample stats (in %)") # uses the kable function to create a nicely formatted table from df, rounding values to 3 digits and adding the caption "Sample stats (in %)".



# we now play with the returns ....

y=Returns$GE

mean(y)
sd(y)
skewness(y)
kurtosis(y)
jarque.bera.test(y)
acf(y, main = "Autocorrelation of returns")
Box.test(y, type = "Ljung-Box")
acf(y^2, main = "Autocorrelation of returns")
Box.test(y^2, type = "Ljung-Box")

dev.new() # new figure
x=qqPlot(y, distribution = "norm", envelope = FALSE,xlab="normal",main="QQ plot")

dev.new() # new figure
x=qqPlot(y, distribution = "t", df = 4, envelope = FALSE,xlab="t(4)")



y.ts=zoo(y,order.by=date.ts)
AvgRet <- mean(y.ts)
y.ts <- y.ts - AvgRet

view(y.ts)

###########################################################################
# Running the MA model for two lengths of estimation window for GE -------
###########################################################################

we <- c(20, 60) # Estimation windows we = 20 and we = 60

sigmaMA <- zoo(matrix(nrow = length(y.ts), ncol = length(we)), order.by=date.ts) # Pre-allocation of estimated volatility matrix
for (i in 1:length(we)) {
  sigmaMA[we[i]:length(y.ts), i] <- rollapply(data = y.ts, width = we[i], 
                                              FUN = function(x) sd(x) * sqrt(we[i] - 1) / sqrt(we[i])) # This is essentially a more efficient for-loop
}

# Plot
par(mfrow=c(2,1)) 
for (i in 1:2) {
  plot(x = index(sigmaMA), y = y.ts, type = 'l', main = paste('MA volatility forecast JPM - WE', we[i]), xlab = 'Trading Days', ylab = 'Returns')
  lines(x = index(sigmaMA), y = 2 * sigmaMA[, i], col = 'red')
  lines(x = index(sigmaMA), y = -2 * sigmaMA[, i], col = 'red')
}


###########################################################################
# EWMA --------------------------------------------------------------------
###########################################################################

# Comparing EWMA models with lambda = 0.94 and lambda = 0.99
lambda <- c(0.94, 0.99)
sigmaEWMA <- zoo(matrix(nrow = length(y.ts), ncol = length(lambda)), order.by = date.ts) # Pre-allocation of estimated conditional volatility matrix
hEWMA <- zoo(matrix(nrow = length(y.ts), ncol = length(lambda)), order.by = date.ts)     # Pre-allocation of estimated conditional variance matrix

hEWMA[1, ] <- var(x = y.ts[1:30]) # Initialize conditional variance by taking sample variance of the first 30 days

par(mfrow=c(2,1)) 
for (i in 1:2) {
  for (t in 2:length(y.ts)) {
    hEWMA[t, i] <- lambda[i] * hEWMA[t-1, i] + (1-lambda[i]) * y.ts[t-1]^2
    sigmaEWMA[t, i] <- sqrt(hEWMA[t, i])
  }
  plot(x = index(sigmaEWMA), y = y.ts, type = 'l', main = paste('EWMA volatility forecast GE - lambda =', lambda[i]), xlab = 'Trading Days', ylab = 'Returns')
  lines(x = index(sigmaEWMA), y = 2 * sigmaEWMA[, i], col = 'red')
  lines(x = index(sigmaEWMA), y = -2 * sigmaEWMA[, i], col = 'red')
}

