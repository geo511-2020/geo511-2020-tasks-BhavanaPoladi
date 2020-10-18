rm(list=ls())
setwd("/Users/bhavanapoladi/Desktop/GEO511")
library(datasets)
data(iris)
summary(iris)
iris$Petal.Length
petal_length_mean <- mean(iris$Petal.Length)
petal_length_mean
hist(iris$Petal.Length)
