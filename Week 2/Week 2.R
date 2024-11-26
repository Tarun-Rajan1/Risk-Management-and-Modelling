rm(list = ls()) # clear all

# Install Packages:

# set the paths
.libPaths(c(.libPaths(), "C:/Users/tpd19wzu/Documents/R/MyLibrary"))
.libPaths(c(.libPaths(), "C:/Program Files/R/R-4.1.3/library"))
setwd("/Users/tarunrajan/Library/Mobile Documents/com~apple~CloudDocs/FM321")



# if (!require(tseries, quietly = TRUE)) {
#   # If the package is not installed, install it
#   install.packages("rmgarch", lib="C:/Users/tpd19wzu/Documents/R/MyLibrary", dependencies = TRUE)
# }

# install.packages('quantmod')
# install.packages("tidyverse")
# install.packages('PerformanceAnalytics')
# install.packages('timeSeries')
# install.packages('tseries')
# install.packages('dplyr')
# install.packages("lubridate")
# install.packages("reshape2") # is designed to make it easier to reshape and aggregate data. I

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



data <- read.csv('crsp.csv') # reads a CSV file named 'crsp.csv' into a data frame called 'data'. The 'read.csv' function is used to import data from a CSV file.
view(data)            # opens a new tab for data visualization
head(data)
tail(data)
class(data) # what type of variable? data.frame


max(data$CFACPR)
min(data$CFACPR)

dev.new() # new figure
plot(data$CFACPR, type = "l", main = "Cumulative Factor to Adjust Prices: CFACPR")

data$Unadjusted_Prices <- data$PRC  # creates a new column in the data data frame called 'Unadjusted_Prices' and assigns the values from the 'PRC' column to it.

data$Adjusted_Prices <- data$PRC / data$CFACPR
View(data)

# --- we now create firm-specific data frames
unique(data$PERMNO) # checking the available PERMNO codes

MSFT <- data[data$PERMNO == 10107, c("date", "Adjusted_Prices")]  # [filtering all rows with the code 10107 in column PERMNO, selecting only the columns "date" and "adjusted_oprice"]
names(MSFT)[2] <- "MSFT"   # changing the name of the second column

XOM <- data[data$PERMNO==11850, c("date", "Adjusted_Prices")]
names(XOM)[2] <- "XOM"

GE <- data[data$PERMNO==12060, c("date", "Adjusted_Prices")]
names(GE)[2] <- "GE"

JPM <- data[data$PERMNO==47896, c("date", "Adjusted_Prices")]
names(JPM)[2] <- "JPM"

MCD <- data[data$PERMNO==43449, c("date", "Adjusted_Prices")]
names(MCD)[2] <- "MCD"

C <- data[data$PERMNO==70519, c("date", "Adjusted_Prices")]
names(C)[2] <- "C"

PRC <- merge(MSFT, XOM) # combines two data frames into a single data frame(same dates) 
PRC <- merge(PRC, GE)
PRC <- merge(PRC, JPM)
PRC <- merge(PRC, MCD)
PRC <- merge(PRC, AAPL)
PRC <- merge(PRC, C) # the default is merge(PRC, C, by = "date", all = FALSE), meaning only merges the rows which dates are presented in both datasets
# if merge(PRC, C, by = "date", all = TRUE), merges all dates (NA if does no exist)
head(PRC)


#  writing the above functions as a loop

# we firstly create a list
stocks <- list(
  MSFT = 10107,
  XOM = 11850,
  GE = 12060,
  JPM = 47896,
  MCD = 43449,
  C = 70519
)

# Loop through the stocks list, rename columns and merge into a single dataset
for (s in 1:length(stocks)) {
  stock_data <- data[data$PERMNO == stocks[s], c("date", "Adjusted_Prices")]
  names(stock_data)[2] <- names(stocks[s]) # names() an be used to get or set the names of the list elements. 
  if (s==1) {
    PRC_loop <-stock_data
  }
  else {
    PRC_loop<-merge(PRC_loop, stock_data)
  }
}


# Another way of doing it using the reshape2 package
# The dcast function is a powerful tool for reshaping data

# Key Components of dcast
#Data: The data frame you want to reshape.
#Formula: Specifies how you want to reshape the data. It typically follows the format row_variable ~ column_variable.
#fun.aggregate: An optional function to aggregate data if there are multiple values for a combination of row and column variables.

Tickers= dcast(data, date ~ PERMNO, value.var = "TICKER") # reshapes the data data frame from long to wide format.
# date ~ PERMNO specifies that date should be the row variable and PERMNO should be the column variable.
# value.var = "TICKER" indicates that the values to be spread across the columns are from the TICKER column.
Tickers=tail(Tickers,1) # This function returns the last row of the Tickers data frame
Tickers

Prices = dcast(data, date ~ PERMNO, value.var = "Adjusted_Prices") #value.var = "Adjusted_Prices" indicates that the values to be spread across the columns are from the Adjusted_Prices column.
names(Prices) <- Tickers # adds names to the columns. The first one is still not correct ...
head(Prices)
names(Prices)[1]="date"
head(Prices)


UnAdjustedPrices = dcast(data, date ~ PERMNO, value.var = "PRC")
names(UnAdjustedPrices) <- Tickers # renames the names of the columns as those in the "tickers"
names(UnAdjustedPrices)[1]="date"  # renames only the first column as"date"
head(UnAdjustedPrices)             # prints the first 6 rows 
dim(UnAdjustedPrices)             # returns the dimension: rows x cols


# creating a data frame for returns
simpleReturns <- dcast(data, date ~ PERMNO, value.var = "RET")
names(simpleReturns) <- Tickers
names(simpleReturns)[1]="date"
head(simpleReturns)

# transforming simple returns to log-returns
# R_t = (P_t/P_{t-1})-1
# R_t + 1 = = (P_t/P_{t-1})
# ln(R_t + 1) = ln(P_t/P_{t-1}) = r_t
Returns <- log(1 + simpleReturns[,2:dim(simpleReturns)[2]]) # dim(simpleReturns)[2] is the num cols; dim(simpleReturns)[1] is the num rows
Returns$date <- simpleReturns$date # add to a newly created column "date" the column "date" from the "simpleReturns" data frame
head(Returns)

# we now bring date to be the first column by creating an indexing vector c(dim(Returns)[2],1:(dim(Returns)[2]-1)) with the right ordering
Returns =Returns[,c(dim(Returns)[2],1:(dim(Returns)[2]-1))] # just reordering the columns
head(Returns)

# date format is an "integer". to see that
class(Returns$date)

# transforming interger in "yyymmss" to date format
date.ts <- ymd(Returns$date)
class(date.ts)

# Alternatively ...
ReturnsDate<-Returns
ReturnsDate$date<-ymd(ReturnsDate$date)
head(ReturnsDate)
class(ReturnsDate$date)
view(ReturnsDate)

# saving files in R format (.RData object)
save(simpleReturns, file = "simpleReturns.RData")
save(Returns, file = "Returns.RData")
save(Prices, file = "Prices.RData")
save(UnAdjustedPrices, file = "UnAdjustedPrices.RData")
save(ReturnsDate, file = "ReturnsDate.RData")

#saving as a .csv file
write.csv(Prices, file = "Prices.csv")

# ------ making plots ---------------
dev.new() # new figure
plot(Returns$JPM)

dev.new() # new figure
plot(Returns$JPM, type = "l")

# trying to make a plot with dates, where date is in the wrong format does not work! See....
dev.new() # new figure
plot(Returns$date, Returns$JPM,
     type = "l",
     main = "Compound returns for JP Morgan",
     ylab = "Returns",
     xlab = "Date",
     col = "red",
     las = 1 #1 if x-axis tick labels are horizontal; 2 if vertical
)
#-------------------------------------------

# we now do it with the correct date format
dev.new() # new figure
plot(date.ts, Returns$JPM,
     type = "l",
     main = "Compound returns for JP Morgan",
     ylab = "Returns",
     xlab = "Date",
     col = "red",
     las = 1
)

head(ReturnsDate)
dev.new() # new figure
plot(ReturnsDate$date, ReturnsDate$JPM,,
     type = "l",
     main = "Compound returns for JP Morgan",
     ylab = "Returns",
     xlab = "Date",
     col = "red",
     las = 1
)

# plotting 3 stocks: does not work that well as we cannot control the y axis ....
dev.new() # new figure
plot(date.ts, Prices$JPM, type = "l", main = "Prices for JP Morgan and Citi",
     ylab = "Price", xlab = "Date", col = "red")# adding the first line: date, variable and specifications
lines(date.ts, Prices$C, col = "blue")          # adding one extra line: date and variable
lines(date.ts, Prices$MSFT, col = "green")      # adding one extra line: date and variable
legend("bottomright",legend = c("JPM", "C","MSFT"), col = c("red", "blue","green"), lty=1,bty='n')

# now controlling the y axis with the ylim() option
dev.new() # new figure
plot(date.ts, Prices$JPM, type = "l", main = "Prices for JP Morgan and Citi",
     ylab = "Price", xlab = "Date", col = "red", ylim=c(0,600))  # we now control the y axis with ylim()
lines(date.ts, Prices$C, col = "blue")
lines(date.ts, Prices$MSFT, col = "green")
legend("bottomright",legend = c("JPM", "C","MSFT"), col = c("red", "blue","green"), lty=1,bty='n') # c("red", "blue","green") makes sure the legend colour is the same as the lines.


#to save file in pdf, comment out dev.new() # new figure

dev.off() # closes pdf environment


dev.new() # new figure
matplot(date.ts,
        cbind(Prices$MSFT,UnAdjustedPrices$MSFT),
        type = "l",
        lty=1,
        col = 1:2,
        main = "Adjusted and unadjusted prices for MSFT",
        ylab = "USD"
)

dev.new() # new figure
par(mfrow = c(1,2)) # subplot (sequential order)
matplot(date.ts,
        Prices$MSFT,
        type = "l",
        lty=1,
        col = 2,
        main = "Adjustedprices for MSFT",
        ylab = "USD"
)
matplot(date.ts,
        UnAdjustedPrices$MSFT,
        type = "l",
        lty=1,
        col = 3,
        main = "unadjustedprices for MSFT",
        ylab = "USD"
)

dev.new() # new figure
par(mfrow = c(3,2))
for (i in 2:dim(Returns)[2]) {
  plot(date.ts, Returns[,i],
       type = "l",
       ylab = "Returns",
       xlab = "Date",
       main = paste("Returns for", names(Prices)[i]) # gets the name of the column i (name of the stock i)
  )
}



# normalized prices

normalizedPrices<-Prices
head(normalizedPrices)
normalizedPrices$MSFT<-normalizedPrices$MSFT/normalizedPrices[1,2]
normalizedPrices$XOM<-normalizedPrices$XOM/normalizedPrices[1,3]
normalizedPrices$GE<-normalizedPrices$GE/normalizedPrices[1,4]
normalizedPrices$MCD<-normalizedPrices$MCD/normalizedPrices[1,5]
normalizedPrices$JPM<-normalizedPrices$JPM/normalizedPrices[1,6]
normalizedPrices$C<-normalizedPrices$C/normalizedPrices[1,7]
head(normalizedPrices)

dev.new() # new figure
matplot(date.ts,
        cbind(normalizedPrices$MSFT,normalizedPrices$XMO,normalizedPrices$GE,normalizedPrices$MCD,normalizedPrices$JPM,normalizedPrices$C),
        type = "l",
        lty=1,
        ylab = "Price",
        xlab = "Date",
        col = 1:6, #colors for the lines
        main = "normalized prices",
)



dev.new() # new figure
plot(date.ts, Prices$MSFT, log="y")


