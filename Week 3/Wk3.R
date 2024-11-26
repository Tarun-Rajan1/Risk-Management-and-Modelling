library(tseries)
library(car)
library(lubridate)
library(zoo)
library(moments)

rm(list = ls())  


load('Returns.RData')
load('Prices.RData')

x=seq(-3,3,length=1000)
z=seq(0,1,length=1000)
par(mfrow=c(2,2))
plot(x,dnorm(x), main="Normal Density")
plot(x,pnorm(x), main="Cumulative Density")
plot(z,qnorm(z), main="Normal Quantile")

dev.new()
x <- seq(-3, 3, length=1000)
normal <- dnorm(x)
st2 <- dt(x, df = 2)
st3 <- dt(x, df = 3)
st10 <- dt(x, df = 10)
plot(x, normal, type = "l", main = "Comparing distributions", col = 1, xlab = "x", ylab = "y")
     lines(x, st2, col = 2)
     lines(x, st3, col = 3)
     lines(x, st10, col = 4)
     legend("topright",
            legend = c("Normal", "T - 2 df", "T - 3 df", "T - 10 df"),
            col = c(1:4),
            lty=1,
            bty='n'
     )
     

head(Prices)
plot(Prices$GE, type = "l", main = "Price of GE", ylab = "Prices")

date.ts <- ymd(Prices$date)
plot(date.ts,Prices$GE, type = "l", main = "Price of GE", ylab = "Prices")

max(Prices$GE)
date.ts[Prices$GE == max(Prices$GE)]

plot(date.ts,Prices$GE, type = "l", main = "Price of GE")
abline(v = ymd(20010907), lwd = 2, col = "blue")

Prices.ts=zoo(Prices[,2:dim(Prices)[2]],order.by=date.ts)
head(Prices.ts)

plot(Prices.ts)
plot(Prices.ts,screen=FALSE)


Prices.crisis=window(Prices.ts,
                     start= ymd(20080101),
                     end=ymd(20100101)
)

plot(Prices.crisis,screen=FALSE,col=1:8)
legend("topright",
       legend=names(Prices.crisis),
       lty=1,
       col=1:8,
       bty='n'
)

first=unclass(Prices.crisis[1,])
for(i in 1:dim(Prices.crisis)[2]) {
  Prices.crisis[,i]=Prices.crisis[,i]/first[i]
}

plot(Prices.crisis,screen=FALSE,col=1:8)
legend("bottomleft",
       legend=names(Prices.crisis),
       lty=1,
       col=1:8,
       bty='n'
)


y=Returns$JPM

mean(y)
sd(y)
skewness(y)
kurtosis(y)
jarque.bera.test(y)
Box.test(y, type = "Ljung-Box")
Box.test(y^2, type = "Ljung-Box")

acf(y, main = "Autocorrelation of returns")
acf(y^2, main = "Autocorrelation of returns squared")


x=qqPlot(y, distribution = "norm", envelope = FALSE,xlab="normal")
x=qqPlot(y, distribution = "t", df = 4, envelope = FALSE,xlab="t(4)")
x=qqPlot(y, distribution = "t", df = 3.5, envelope = FALSE,xlab="t(3.5)")
x=qqPlot(y, distribution = "t", df = 3, envelope = FALSE,xlab="t(3)")



