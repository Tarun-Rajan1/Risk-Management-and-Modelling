setwd("/Users/tarunrajan/Library/Mobile Documents/com~apple~CloudDocs/FM321")


data <- read.csv('crsp.csv')
dim(data)
head(data)
head(data$RET)
head(data[,6])
names(data)
unique(data$PERMNO)
unique(data$TICKER)
unique(data$COMNAM)


citi <- data[data$PERMNO == 70519,]
dim(citi)
head(citi)
tail(citi)
unique(citi$TICKER)
highest_citi <- max(citi$RET) * 100
paste0("Highest return for Citi: ", highest_citi, "%")
lowest_citi <- min(citi$RET) * 100
paste0("Lowest return for Citi: ", lowest_citi , "%")

plot(citi$PRC, type = "l", main = "Price of Citi")






