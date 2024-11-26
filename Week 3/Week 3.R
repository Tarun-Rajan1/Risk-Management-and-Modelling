rm(list = ls()) # clear all

# Install Packages:

# set the paths
.libPaths(c(.libPaths(), "C:/Users/tpd19wzu/Documents/R/MyLibrary"))
.libPaths(c(.libPaths(), "C:/Program Files/R/R-4.1.3/library"))



# if (!require(tseries, quietly = TRUE)) {
#   # If the package is not installed, install it
#   install.packages("rmgarch", lib="C:/Users/tpd19wzu/Documents/R/MyLibrary", dependencies = TRUE)
# }

install.packages("quantmod")
install.packages("lubridate")
install.packages("reshape2") # is designed to make it easier to reshape and aggregate data. I
install.packages("tidyverse")
install.packages("PerformanceAnalytics")
install.packages("timeSeries")
install.packages("tseries")
install.packages("roll")
install.packages("car")
install.packages("MASS")
install.packages("extraDistr")
install.packages("rugarch")
install.packages("QRM")
install.packages("dplyr")
install.packages("reshape2")
install.packages("moments")
install.packages("zoo")


library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(timeSeries)
library(tseries)
library(roll)
library(car) # qqplot is part of this package
library(MASS)
library(extraDistr)
library(rugarch)
library(QRM)
library(dplyr)
library(reshape2)
library(moments)
library(zoo)

# setting the work directory
#setwd("C:/Users/tpd19wzu/Dropbox/Gus_stuff/teaching/__LSE/FM321/2024-25/Seminars/data_wrds")
setwd("C:/Users/FRUETDIA/Dropbox/Gus_stuff/teaching/__LSE/FM321/2024-25/Seminars/data_wrds")


load('Returns.RData')
load('Prices.RData')


# playing with pdf, cdf and inverse of cfd

x=seq(-3,3,length=1000)   # it creates a sequence of 1000 equally spaced numbers from -3 to 3
z=seq(0,1,length=1000)
par(mfrow=c(2,2))
plot(x,dnorm(x), main="Normal Density")       # it creates a plot of the probability density function (pdf) evaluated on each value of x
plot(x,pnorm(x), main="Cumulative Density")   # it creates a plot of the cumulative distribution function (CDF) evaluated on each value of x
plot(z,qnorm(z), main="Normal Quantile")      # it creates a plot of the quantile function (inverse CDF) of the normal distribution



# Comparing the normal distribution with the Student-t (The Student-t distribution has fatter tails than the normal.)

x <- seq(-3, 3, length=1000) 
normal <- dnorm(x)     # it computes the density of the standard normal distribution (mean = 0, standard deviation = 1) for each value in x
st2 <- dt(x, df = 2)   # it computes the density of the t-distribution with 2 degrees of freedom for each value in x
st3 <- dt(x, df = 3)   # it computes the density of the t-distribution with 3 degrees of freedom for each value in x
st10 <- dt(x, df = 10) # it computes the density of the t-distribution with 10 degrees of freedom for each value in x  

# we now plot
dev.new() # new figure
plot(x, normal, type = "l", main = "Comparing distributions", col = 1, xlab = "x", ylab = "Density") # states that the plot type is "line" and the horizontal and vertical axis are named x and density, respectively.
lines(x, st2, col = 2)   # This adds a line to the existing plot (same vertical and horizontal axis). col = 2: This sets the color of the line to color number 2 (usually red).
lines(x, st3, col = 3)   # col 3 is usually green
lines(x, st10, col = 4)  # col 4 is usually blue

#  This adds a legend to the plot at the top right position.
legend("topright",
       legend = c("Normal", "T - 2 df", "T - 3 df", "T - 10 df"), # it specifies the text for the legend entries.
       col = c(1:4), # it sets the colors for the legend entries to match the colors of the lines in the plot.
       lty = 1,      # it sets the line type for the legend entries to solid lines.
       bty = 'n'     # it removes the box around the legend. Try to comment it out ...
)
a

head(Prices)
dev.new() # new figure
plot(Prices$GE, type = "l", main = "Price of GE")

# handling date
date.ts=ymd(Prices$date)
dev.new() # new figure
plot(date.ts,Prices$GE, type = "l", main = "Price of GE")

max(Prices$GE)
date.ts[Prices$GE == max(Prices$GE)]
Prices[which.max(Prices$GE),] # selects the row (date) associated with the date of the max price of GE and all columns (stocks)

dev.new() # new figure
plot(date.ts,Prices$GE, type = "l", main = "Price of GE")
abline(v = ymd(20010907), lwd = 2, col = "blue") # it sets the line width to 2, making the line thicker; colour blue
# The abline() function in R is used to add straight lines to a plot. 
# It can add horizontal, vertical, or regression lines. Here are the main inputs 
# Example: abline(a = 0, b = 1) adds a line with intercept 0 and slope 1.
# Example: abline(v = 10) adds a vertical line at x = 10.
# Example:  abline(coef = c(0, 1)) is equivalent to abline(a = 0, b = 1), where a is the intercept and b is the slope
# or alternatively,  abline(lm(y ~ x)) adds the regression line from a linear model of y on x.

# it uses zoo to create a time series vector (data - columns - that brings the date in the right format)
Prices.ts=zoo(Prices[,2:dim(Prices)[2]],order.by=date.ts)
head(Prices.ts)
class(Prices.ts)  # zoo (time series) data type

dev.new() # new figure
plot(Prices.ts)
plot(Prices.ts,screen=FALSE)  # The screen argument is used when plotting multiple time series. 
# By default, screen = TRUE means that each series will be plotted on a separate screen (or panel). 
# Setting screen = FALSE means that all series will be plotted on the same screen (or panel).

# we now focus on the crises period:
Prices.crisis=window(Prices.ts,
                     start= ymd(20080101),
                     end=ymd(20100101)  # the start and ed commands select rows based on the dates
)
view(Prices.crisis)

dev.new() # new figure
plot(Prices.crisis,screen=FALSE,col=1:6)
legend("topright",
       legend=names(Prices.crisis),
       lty=1,
       col=1:6,
       bty='n'
)

# we now normalize prices: all series start from one:
head(Prices.crisis)
class(Prices.crisis)  # it remains a time series object (zoo)
first=unclass(Prices.crisis[1,]) # the unclass() function removes the data properties and transform the data into a matrix(vector)
first
dim(first)

for(i in 1:dim(Prices.crisis)[2]) Prices.crisis[,i]=Prices.crisis[,i]/first[i]
plot(Prices.crisis,screen=FALSE,col=1:6)  # it overlaps the graph (update) on every iteration.
legend("bottomleft",
       legend=names(Prices.crisis),
       lty=1,
       col=1:6,
       bty='n'
)


y=Returns$JPM
AvgRet<-mean(y)
AvgRet
StdDevRet<-sd(y)
StdDevRet
MaxRet<-max(y)
MaxRet
MinRet<-min(y)
MinRet
SkewRet<-skewness(y)
SkewRet
KurtRet<-kurtosis(y)
KurtRet
# Statistical test to check for normality
# JB = (n/6)[S^{2}+(1/4)(K-3)^{2}]~\Xi^{2}(2)
jarque.bera.test(y)
CV = qchisq(p = 0.95, df = 2)
CV
# test statistic is larger than the critical value:null hypothesis of normality is rejected.

DailyStats <- as.table(rbind(AvgRet, StdDevRet, MaxRet, MinRet, SkewRet, KurtRet))
DailyStats



# Statistical test to detect volatility clusters
# Compute the Box–Pierce or Ljung–Box test statistic for examining the null hypothesis of independence in a given time series.
# Q=T(T+2)\sum_{k=1}^{L}\rho^{2}/(T-k) \sim \chi^{2}(L)
# where T is the sample size, L is the number of lags being tested
# Ljung-Box test should be preferred to  Box-Pierce test

Box.test(y, type = "Ljung-Box")
Box.test(y^2, type = "Ljung-Box")

Box.test(y^2, lag = 20, type = "Ljung-Box")
print(paste('Critical Value is:', qchisq(p = 0.95, df = 20, lower.tail=TRUE)))

dev.new() # new figure
acf(y, main = "Autocorrelation of returns")

dev.new() # new figure
acf(y^2, main = "Autocorrelation of returns squared")


# if we want to plot the qqplot against other distributions:
# qqPlot(your_data, distribution = ged_quantiles, df = 2, scale = 1, 
#        xlab = 'GED Quantiles', ylab = 'Quantiles of Input Sample', 
#        main = 'Q-Q Plot Against GED')

#qqPlot(your_data, distribution = qt, df = 5, 
#        xlab = 't-Distribution Quantiles', ylab = 'Quantiles of Input Sample', 
#        main = 'Q-Q Plot Against t-Distribution')

# Check for normality using qqplot's
# envelope=FALSE does not include confidence intervals
dev.new() # new figure  
x=qqPlot(y, distribution = "norm", envelope = FALSE,xlab="normal")

dev.new() # new figure
x=qqPlot(y, distribution = "t", df = 4, envelope = FALSE,xlab="t(4)")

dev.new() # new figure
x=qqPlot(y, distribution = "t", df = 3.5, envelope = FALSE,xlab="t(3.5)")

dev.new() # new figure
x=qqPlot(y, distribution = "t", df = 3, envelope = FALSE,xlab="t(3)") 

