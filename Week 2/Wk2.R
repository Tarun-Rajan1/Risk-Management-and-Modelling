install.packages("zoo")
install.packages("lubridate")
install.packages("tseries")
install.packages("rugarch")
install.packages("rmgarch")
install.packages("reshape2")
install.packages("car")

setwd("/Users/tarunrajan/Library/Mobile Documents/com~apple~CloudDocs/FM321")

library(reshape2)
library(lubridate)

data <- read.csv("crsp.csv")
head(data)
class(data)

data$Unadjusted_Prices <- data$PRC
data$Adjusted_Prices <- data$PRC/data$CFACPR
head(data)


Tickers= dcast(data, date ~ PERMNO, value.var = "TICKER")
Tickers=tail(Tickers,1)
Tickers

Prices = dcast(data, date ~ PERMNO, value.var = "Adjusted_Prices")
names(Prices) <- Tickers
names(Prices)[1]="date"
head(Prices)
dim(Prices)

UnAdjustedPrices = dcast(data, date ~ PERMNO, value.var = "PRC")
names(UnAdjustedPrices) <- Tickers
names(UnAdjustedPrices)[1] <- "date"
head(UnAdjustedPrices)
dim(UnAdjustedPrices)

simpleReturns <- dcast(data, date ~ PERMNO, value.var = "RET")
names(simpleReturns) <- Tickers
names(simpleReturns)[1]="date"
head(simpleReturns)


Returns <- log(1 + simpleReturns[,2:dim(simpleReturns)[2]])
Returns$date <- simpleReturns$date


Returns = Returns[, c(dim(Returns)[2], 1: dim(Returns)[2] - 1)]
head(Returns)

date.ts <- ymd(Returns$date)
class(date.ts)

save(simpleReturns, file = "simpleReturns.RData")
save(Returns, file = "Returns.RData")

plot(Returns$JPM)
plot(Returns$JPM, type = "l")


plot(Returns$JPM,
     type = "l",
     main = "Compound returns for JP Morgan",
     ylab = "Returns",
     xlab = "Observation",
     col = "red",
     las=1
)


plot(date.ts, Returns$JPM,
     type = "l",
     main = "Compound returns for JP Morgan",
     ylab = "Returns",
     xlab = "Date",
     col = "red",
     las = 1
)

plot(date.ts, Prices$JPM, type = "l", main = "Prices for JP Morgan and Citi",
     ylab = "Price", xlab = "Date", col = "red")
lines(date.ts, Prices$C, col = "blue")
legend("bottomright",legend = c("JPM", "C"), col = c("red", "blue"), lty=1,bty = 'n')


plot(date.ts, Prices$JPM,
     type = "l",
     main = "Prices for JP Morgan and Citi",
     ylab = "Price",
     xlab = "Date",
     col = "red",
     ylim = c(0, 600)
)

lines(date.ts, Prices$C, col = "blue")
legend("topright", legend = c("JPM", "C"), col = c("red", "blue"),lty=1)


matplot(date.ts, Prices[,2:dim(Prices)[2]],
        type = "l",
        lty=1,
        main = "Prices ",
        ylab = "Price",
        xlab = "Date",
        las=1,
        col = 1:9
)

matplot(date.ts,
        cbind(Prices$MSFT,UnAdjustedPrices$MSFT),
        type = "l",
        lty=1,col = 2:3,
        main = "Adjusted and unadjusted prices for MSFT",
        ylab = "USD"
)


matplot(date.ts,Returns[,2:dim(Returns)[2]],
        type = "l",
        main = "Returns for our stocks",
        ylab = "Returns",
        lty = 1,
        las=1
)
legend("bottomright",
       legend = names(Returns[,2:dim(Returns)[2]]),
       col = c(1:6),
       lty=1,
       bty='n'
)


dev.new()
par(mfrow = c(4,2))

for (i in 2:dim(Returns)[2]) {
  plot(date.ts, Returns[,i],
       type = "l",
       ylab = "Returns",
       xlab = "Date",
       main = paste("Returns for", names(Prices)[i])
  )
}



normalizedPrices<-Prices
head(normalizedPrices)
normalizedPrices$MSFT<-normalizedPrices$MSFT/normalizedPrices[1,2]
normalizedPrices$XOM<-normalizedPrices$XOM/normalizedPrices[1,3]
normalizedPrices$GE<-normalizedPrices$GE/normalizedPrices[1,4]
normalizedPrices$AAPL<-normalizedPrices$AAPL/normalizedPrices[1,5]
normalizedPrices$MCD<-normalizedPrices$MCD/normalizedPrices[1,6]
normalizedPrices$JPM<-normalizedPrices$JPM/normalizedPrices[1,7]
normalizedPrices$C<-normalizedPrices$C/normalizedPrices[1,8]
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
matplot(date.ts,
        cbind(normalizedPrices$MSFT,normalizedPrices$XMO,normalizedPrices$GE,normalizedPrices$MCD,normalizedPrices$JPM,normalizedPrices$C),
        type = "l",
        lty=1,
        ylab = "Price",
        xlab = "Date",
        col = 1:6, #colors for the lines
        main = "normalized prices",
        log = "y"
)
