# ====================================================
# Table of Contents
# ====================================================
# 1. Environment Setup
# 2. Data Visualization
# 3. Data Manipulation
# 4. Statistical Functions
# 5. File Saving and Exporting
# ====================================================

# ---- 1. Environment Setup ----
# Clears the environment to remove all objects
rm(list = ls())  

# Adds additional library paths without removing defaults
.libPaths(c(.libPaths(), "C:/Users/tpd19wzu/Documents/R/MyLibrary"))  

# Sets the working directory
setwd("C:/Users/tpd19wzu/Dropbox/Gus_stuff/teaching/__LSE/FM321/2024-25/Seminars/data_wrds")  


# ---- 2. Data Visualization ----
# Opens a new tab to view the data frame
View(data)

# Displays basic information about the data frame
dim(data)            # Returns data frame dimensions (rows and columns)
head(data)           # Shows first six rows for quick inspection
head(data$RET)       # Shows first six values of the "RET" column
head(data[,6])       # Shows first six values of the sixth column
names(data)          # Lists the column names in the data frame
unique(data$PERMNO)  # Finds unique values in the "PERMNO" column

# Concatenates and displays a string with variable content
paste0("Highest return for Citi: ", highest_citi, "%") 

# Plotting: Visualize returns for JP Morgan
plot(date.ts, Returns$JPM, type = "l", main = "Compound Returns for JP Morgan", 
     ylab = "Returns", xlab = "Date", col = "red", las = 1)

# Plotting: Compare prices for multiple stocks with controlled y-axis
plot(date.ts, Prices$JPM, type = "l", main = "Prices for JP Morgan and Citi",
     ylab = "Price", xlab = "Date", col = "red", ylim = c(0, 600))
lines(date.ts, Prices$C, col = "blue")
lines(date.ts, Prices$MSFT, col = "green")
legend("bottomright", legend = c("JPM", "C", "MSFT"), col = c("red", "blue", "green"), lty = 1, bty = 'n') #lty sets the line type, bty controls the box around the legend


#Plots columns of a matrix or data frame against a common x-axis
matplot(date.ts, Prices[, 2:dim(Prices)[2]], type = "l", lty = 1, 
        main = "Prices", ylab = "Price", xlab = "Date", col = 1:6, las = 1)

#Provides plotting area in an with a rows and b columns
par(mfrow = c(4,2))

#Adds straight lines to the plot 
abline(v = ymd(20010907), lwd = 2, col = "blue")


# The screen argument is used when plotting multiple time series on the same panel 
plot(Prices.ts,screen=FALSE)  

#To create a nicely formatted table from df, rounding values to 3 digits and adding the caption "Sample stats (in %)".
kable(df,digits=3,caption="Sample stats (in %)") 



# ---- 3. Data Manipulation ----

# Renaming columns
names(MSFT)[2] <- "MSFT"  # Renames the second column in MSFT data to "MSFT"

# Filtering: Extract rows where PERMNO matches 10107
MSFT <- data[data$PERMNO == 10107, c("date", "Adjusted_Prices")]

# Merging: Combine two data frames (by common dates)
PRC <- merge(MSFT, XOM) 

# Reshaping: Convert data from long to wide format
Tickers = dcast(data, date ~ PERMNO, value.var = "TICKER")  

# Date conversion: Transform integer in "yyymmss" format to date
date.ts <- ymd(Returns$date)

#Converting into time series vector using zoo package 
Prices.ts=zoo(Prices[,2:dim(Prices)[2]],order.by=date.ts)

#Extracts the subset of the object between the start and end times 
Prices.crisis=window(Prices.ts, start= ymd(20080101), end=ymd(20100101))

#Removes the data properties and transform the data into a matrix(vector)
first <- unclass(Prices.crisis[1,]) 

#  Converts the matrix df into a dataframe
df=as.data.frame(df)


# ---- 4. Statistical Functions ----
# Matrix operations
C_t <- t(C)            # Computes the transpose of matrix C
solve(B)               # Finds the inverse of matrix B

#Generates a sequance 
x <- seq(-3, 3, length=1000)

plot(x,dnorm(x), main="Normal Density")       # it creates a plot of the probability density function (pdf) evaluated on each value of x
plot(x,pnorm(x), main="Cumulative Density")   # it creates a plot of the cumulative distribution function (CDF) evaluated on each value of x
plot(z,qnorm(z), main="Normal Quantile")      # it creates a plot of the quantile function (inverse CDF) of the normal distribution

#Jarque-Bera Test 
jarque.bera.test(y)

#Ljung box test
Box.test(y, type = "Ljung-Box")
Box.test(y^2, type = "Ljung-Box")

#Auto correlation funciton 
acf(y, main = "Autocorrelation of returns")
acf(y^2, main = "Autocorrelation of returns squared")

#QQ Plot 
x=qqPlot(y, distribution = "norm", envelope = FALSE,xlab="normal")


# ---- 5. File Saving and Exporting ----
# Save an R object to a .RData file
save(simpleReturns, file = "simpleReturns.RData")

# Export a data frame to a .csv file
write.csv(Prices, file = "Prices.csv")

# PDF Plotting: Save plot to a PDF file
pdf(file = "Seminar2_plot.pdf", width = 8, height = 6, paper = "a4", bg = "white", pointsize = 12)
