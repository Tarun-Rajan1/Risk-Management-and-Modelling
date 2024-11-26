rm(list = ls()) # clear all

# Install Packages:

# set the paths
.libPaths(c(.libPaths(), "C:/Users/tpd19wzu/Documents/R/MyLibrary"))
.libPaths(c(.libPaths(), "C:/Program Files/R/R-4.1.3/library"))



# if (!require(tseries, quietly = TRUE)) {
#   # If the package is not installed, install it
#   install.packages("rmgarch", lib="C:/Users/tpd19wzu/Documents/R/MyLibrary", dependencies = TRUE)
# }

# install.packages('quantmod')
# install.packages("tidyverse")
# install.packages('PerformanceAnalytics')
# install.packages('timeSeries')
# install.packages('tseries')
# install.packages("roll", lib="C:/Users/tpd19wzu/Documents/R/MyLibrary", dependencies = TRUE)
# install.packages("car", lib="C:/Users/tpd19wzu/Documents/R/MyLibrary", dependencies = TRUE)
# install.packages("MASS", lib="C:/Users/tpd19wzu/Documents/R/MyLibrary", dependencies = TRUE)
# install.packages("extraDistr", lib="C:/Users/tpd19wzu/Documents/R/MyLibrary", dependencies = TRUE)
# install.packages("rugarch", lib="C:/Users/tpd19wzu/Documents/R/MyLibrary", dependencies = TRUE)
# install.packages("rmgarch", lib="C:/Users/tpd19wzu/Documents/R/MyLibrary", dependencies = TRUE)
# install.packages("BEKKs", lib="C:/Users/tpd19wzu/Documents/R/MyLibrary", dependencies = TRUE)
# install.packages("QRM", lib="C:/Users/tpd19wzu/Documents/R/MyLibrary", dependencies = TRUE)
# install.packages('dplyr')

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


# setting the work directory
setwd("/Users/tarunrajan/Library/Mobile Documents/com~apple~CloudDocs/FM321/Week 1 ")


## Basic R commands
2^5
a=2
b=100
exp(a)+b/10
result=exp(a)+b
cat("the answer is:",log(a),"or",result,"\n")

# --------------- creating and manipulating matrices --------------
A <- matrix(nrow = 3, ncol = 2) # empty (NA) matrix
A
B <- matrix(c(3, 4, 5, 8), nrow = 2, byrow = TRUE)  # The byrow argument specifies how the matrix should be filled with the provided data.
#byrow = TRUE: The matrix is filled row-wise. This means the data is entered into the matrix one row at a time.
#byrow = FALSE (default): The matrix is filled column-wise. This means the data is entered into the matrix one column at a time.
B
C <- matrix(c(4, 4, 5, 7, 34, 2), nrow = 2, byrow = TRUE)
C
c_1c<- C[1,]  # displays the first row 
c_1c
c_r1<- C[,1]  # displays the first column
c_r1
c_21<- C[2,1] # displays the 2,1 element
c_21
C_t <- t(C)  # transpose of C
C_t


# important matrixes:
H <- matrix(0, nrow = 3, ncol = 4)# 3 x 4 matrix of zeros
H    
I <- diag(5) # Creates a 5x5 identity matrix
I 
W <- matrix(1, nrow = 3, ncol = 1) # creates a 3 x 1 vector of ones
W


B_inv <- solve(B) # inverse of matrix B
B_inv
D<-B%*%C
D
V = B%*%B_inv
V
#-------------------------------------



## ---- we will now import the data (from a csv file) ----

data <- read.csv('crsp_seminar1.csv') # reads a CSV file named 'crsp.csv' into a data frame called 'data'. The 'read.csv' function is used to import data from a CSV file.

View(data)            # opens a new tab for data visualization
dim(data)             # returns the dimensions of the data frame data, i.e., the number of rows and columns. It gives you an idea of the size of your dataset.
head(data)            # displays the first six rows of the data frame data. It’s useful for quickly inspecting the beginning of your dataset.
head(data$RET)        # shows the first six values of the column named RET in the data frame data. The $ operator is used to access a specific column in a data frame.
head(data[,6])        # displays the first six values of the sixth column in the data frame data. The [,6] notation is used to access the sixth column
names(data)           # returns the names of the columns in the data frame data. It helps you understand the structure of your dataset.

unique(data$PERMNO)   # returns the unique values in the PERMNO column of the data frame data. The unique function is used to find distinct values in a column.
unique(data$TICKER)
unique(data$COMNAM)


citi <- data[data$PERMNO == 70519,]
# data$PERMNO == 70519: creates a logical vector (a series of TRUE or FALSE values) by checking each value in the PERMNO column of the data data frame. It returns TRUE for rows where the PERMNO value is 70519 and FALSE otherwise.
# for example 
# p_citi <- data$PERMNO == 70519
# view(p_citi)

# or alternatively ....
citi_df <- as.data.frame(data[data$PERMNO == 70519,])

dim(citi)
head(citi)
tail(citi)
unique(citi$TICKER)
highest_citi <- max(citi$RET) * 100
paste0("Highest return for Citi: ", highest_citi, "%")  # paste0() function concatenates (or join) strings together without any separator.
lowest_citi <- min(citi$RET) * 100
paste0("Lowest return for Citi: ", lowest_citi , "%")

# --- we now plot the prices ------

dev.new() # new figure
plot(citi$PRC, type = "l", main = "Price of Citi")
dev.new() # new figure
plot(citi$PRC, type = "p", main = "Price of Citi")
dev.new() # new figure
plot(citi$PRC, type = "b", main = "Price of Citi")


# plot(citi$PRC, ...):
# main function to create a plot, where citi$PRC is the data to be plotted. 

# type = "l":
# This argument specifies the type of plot: "l" stands for “line”; "p" for points; "b" for both points and lines.

#main = "Price of Citi":
# includes the main title of the plot. 

# -------------------------------------------------------------

## ---------- Example if and loop ----------------
## Generate prices: random walk model
T<-2000
p <- matrix(nrow = T, ncol = 1)
r <- matrix(nrow = T-1, ncol = 1)
eps <- rnorm(n = T)
for (t in 1:T) {  
  if (t==1 ) {
    p[t]<- 3+eps[t]
  } else {
    p[t]<- p[t-1]+eps[t]
    r[t]<- p[t]-p[t-1]
  }
} # ends loop T

# plotting prices
dev.new()
plot(p, type = 'l')


# plotting returns
dev.new()
plot(r, type = 'l')