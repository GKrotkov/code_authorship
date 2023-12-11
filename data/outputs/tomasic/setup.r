#

# set up for three_boe.r, six_boe.r, and twelve_boe.r
# run source("setup.r") before running those models

library(FNN) # knn
library(MASS) # rlm
library(e1071) # kurtosis
library(randomForest)
library(neuralnet) # mlp

print(paste("read data", Sys.time()))

data <- read.csv("well_completion_clean.csv", header=T)

data$completion_date <- as.Date(data$completion_date, format="%Y-%m-%d")

rmse <- function(error)
{
     sqrt(mean(error^2))
}

mae <- function(error)
{
	mean(abs(error))
}

