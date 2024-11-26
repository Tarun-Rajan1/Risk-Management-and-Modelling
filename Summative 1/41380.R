# Clearing the environment
rm(list =ls())

# #Installing the packages 
# install.packages("lubridate")
# install.packages("reshape2") 
# install.packages("moments")
# install.packages("zoo")
# install.packages("tseries")
# install.packages("car")

#Libraries
library(lubridate)
library(reshape2)
library(moments)
library(zoo)
library(tseries)
library(car)

#Reading in the data 
stocks <- read.csv("Stocks.csv")

#Adjusting the Returns
stocks$Unadjusted_Price <- stocks$PRC
stocks$Adjusted_Price <- stocks$PRC / stocks$CFACPR 

#Subset for prices 
Tickers <- dcast(stocks, date ~ PERMNO, value.var = "TICKER")
Tickers <- head(Tickers,1)
Prices <- dcast(stocks, date ~ PERMNO, value.var = "Adjusted_Price")
names(Prices) <- Tickers
names(Prices)[1] <- "date"


#Plotting prices
date.ts <- ymd(Prices$date)
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2), oma = c(4, 0, 2, 0)) 
Prices.ts.Tesla <- zoo(Prices$TSLA,order.by=date.ts)
plot(Prices.ts.Tesla, type = "l",main = "Prices", ylab = "Tesla",col = "red")
abline(v = ymd(20200315), lwd = 1, col = "purple") # Start of Covid Lockdown (15-03-2020)
abline(v = ymd(20220224), lwd = 1, col = "black") #Start of Russia - Ukraine Conflict (24-02-2022)
abline(v = ymd(20221027), lwd = 1, col = "blue") #Elon Musk bought Twitter (27-10-2022)
legend("topleft", 
       legend = c("Start of Covid Lockdown", "Russia-Ukraine Conflict","CEO Twitter Acquisition"),
       col = c("purple", "black", "blue"), 
       lwd = 2, 
       cex = 0.6,
       bty = "n") 

Prices.ts.Unilever <- zoo(Prices$UL,order.by=date.ts)
plot(Prices.ts.Unilever, type = "l", ylab = "Unilever", col = "red")
abline(v = ymd(20160623), lwd = 1, col = "green") #Brexit Referendum (23-06-2016)
abline(v = ymd(20170217), lwd = 1, col = "darkred") #Merger speculation (17-02-2017)
abline(v = ymd(20190101), lwd = 1, col = "blue") #CEO change (01-01-2019)
abline(v = ymd(20200315), lwd = 1, col = "purple") # Start of Covid Lockdown (15-03-2020)
abline(v = ymd(20220224), lwd = 1, col = "black") #Start of Russia - Ukraine Conflict (24-02-2022)
legend("topleft", 
       legend = c("Brexit Referendum","Merger speculation","CEO change","Start of Covid Lockdown", "Russia-Ukraine Conflict"),
       col = c("green","darkred","blue","purple", "black"), 
       lwd = 2, 
       cex = 0.6,
       bty = "n")  # 'bty = "n"' removes the box around the legend


#Subset for simple returns
Simple_Returns <- dcast(stocks, date ~ PERMNO, value.var = "RET")
names(Simple_Returns) <- Tickers
names(Simple_Returns)[1] <- "date"

#Converting to compound returns 
Returns <- log(1 + Simple_Returns[,2:dim(Simple_Returns)[2]])
Returns$date <- Simple_Returns$date
Returns <- Returns[,c(dim(Simple_Returns)[2],1:dim(Simple_Returns)[2] - 1)]

#Plotting returns
date.ts <- ymd(Returns$date)
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2), oma = c(4, 0, 2, 0))  
Returns.ts <- zoo(Returns$TSLA,order.by=date.ts)
plot(Returns.ts, type = "l",main = "Returns", ylab = "Tesla",col = "blue")
Returns.ts <- zoo(Returns$UL,order.by=date.ts)
plot(Returns.ts, type = "l", ylab = "Unilever", col = "blue")


#Analysing Tesla's returns
t <- Returns$TSLA
dist_test <- qqPlot(t, distribution = "norm", envelope = FALSE, ylab ="Tesla Returns", main ="Normal")
dist_test <- qqPlot(t, distribution = "t", df = 4, envelope = FALSE,  ylab ="Tesla Returns",main ="t(4)")
dist_test <- qqPlot(t, distribution = "t", df = 3, envelope = FALSE, ylab ="Tesla Returns",main ="t(3)")
dist_test <- qqPlot(t, distribution = "t", df = 3.5, envelope = FALSE, ylab ="Tesla Returns",main ="t(3.5)")

#Sample statistics for Tesla 
mean(t)
sd(t)
min(t)
max(t)
skewness(t)
kurtosis(t)
jarque.bera.test(t)

#Autocorrelation plots and Ljung box test 
while (!is.null(dev.list())) dev.off()
acf(t, main = "Autocorrelation of Tesla returns")
Box.test(t, type = "Ljung-Box")
acf(t^2, main = "Autocorrelation of Tesla returns squared")
Box.test(t^2, type = "Ljung-Box")


#Analysing Unilever's returns
u <- Returns$UL
dist_test <- qqPlot(u, distribution = "norm", envelope = FALSE, ylab ="Unilever Returns", main ="Normal")
dist_test <- qqPlot(u, distribution = "t", df = 4, envelope = FALSE,  ylab ="Unilever Returns",main ="t(4)")
dist_test <- qqPlot(u, distribution = "t", df = 3, envelope = FALSE, ylab ="Unilever Returns",main ="t(3)")
dist_test <- qqPlot(u, distribution = "t", df = 3.5, envelope = FALSE, ylab ="Unilever Returns",main ="t(3.5)")

#Sample statistics for Unilever
mean(u)
sd(u)
min(u)
max(u)
skewness(u)
kurtosis(u)
jarque.bera.test(u)

#Autocorrelation plots and Ljung box test 
while (!is.null(dev.list())) dev.off()
acf(u, main = "Autocorrelation of Unilever returns")
Box.test(u, type = "Ljung-Box")
acf(u^2, main = "Autocorrelation of Unilever returns squared")
Box.test(u^2, type = "Ljung-Box")


#//////////////////////////////#
#Tables to be pasted into document (Run in RMarkdown/Quarto)


#library(kableExtra)
# df <- matrix(ncol=4,nrow=2)
# n <- names(Returns)
# n <- n[2:length(n)]
# for(i in 1:length(n)){
# x=Returns[[n[i]]]
# df[i,]=c(mean(x),sd(x),min(x),max(x))
# }
# df <-  as.data.frame(df)
# df <- cbind(n,df*100)
# names(df)=c("Asset","mean","sd","min","max")
# kable(df,digits=3,caption="Sample stats (in %)") # uses the kable function to create a nicely formatted table from df, rounding values to 3 digits and adding the caption "Sample stats (in %)".


# skew_tesla <- skewness(t)
# kurtosis_tesla <- kurtosis(t)
# jb_test_tesla <- jarque.bera.test(t)
# 
# tesla_results <- data.frame(
#   Metric = c("Skewness", "Kurtosis", "Jarque-Bera Statistic", "Jarque-Bera p-value"),
#   Tesla = c(skew_tesla, kurtosis_tesla, jb_test_tesla$statistic, jb_test_tesla$p.value)
# )
# 
# kable(tesla_results, caption = "Summary Statistics for Tesla Returns")

# lb_test_tesla_returns <- Box.test(t, type = "Ljung-Box")
# lb_test_tesla_returns_squared <- Box.test(t^2, type = "Ljung-Box")
# 
# tesla_ljung_box_results <- data.frame(
#   Metric = c("Ljung-Box Statistic (Returns)", "Ljung-Box p-value (Returns)",
#              "Ljung-Box Statistic (Returns Squared)", "Ljung-Box p-value (Returns Squared)"),
#   Tesla = c(lb_test_tesla_returns$statistic, lb_test_tesla_returns$p.value,
#             lb_test_tesla_returns_squared$statistic, lb_test_tesla_returns_squared$p.value)
# )
# 
# kable(tesla_ljung_box_results, caption = "Ljung-Box Test Results for Tesla Returns")


# skew_unilever <- skewness(u)
# kurtosis_unilever <- kurtosis(u)
# jb_test_unilever <- jarque.bera.test(u)
# 
#unilever_results <- data.frame(
#   Metric = c("Skewness", "Kurtosis", "Jarque-Bera Statistic", "Jarque-Bera p-value"),
#   unilever = c(skew_unilever, kurtosis_unilever, jb_test_unilever$statistic, jb_test_unilever$p.value)
# )
# 
# kable(unilever_results, caption = "Summary Statistics for Unilever Returns")


# lb_test_unilever_returns <- Box.test(u, type = "Ljung-Box")
# lb_test_unilever_returns_squared <- Box.test(u^2, type = "Ljung-Box")
# unilever_ljung_box_results <- data.frame(
#   Metric = c("Ljung-Box Statistic (Returns)", "Ljung-Box p-value (Returns)",
#              "Ljung-Box Statistic (Returns Squared)", "Ljung-Box p-value (Returns Squared)"),
#   Unilever = c(lb_test_unilever_returns$statistic, lb_test_unilever_returns$p.value,
#                lb_test_unilever_returns_squared$statistic, lb_test_unilever_returns_squared$p.value)
# )
# 
# 
# kable(unilever_ljung_box_results, caption = "Ljung-Box Test Results for Unilever Returns")







