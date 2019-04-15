# Functions in R

## Sample data
x <- c(1, 2, 3, 4) # Vector x
y <- c(5, 6, 7, 8) # Vector x

## cbind function is used to combine two vectors by columns
cbind(x,y)

## rbind function is used to combine two vectors by rows
rbind(x,y)

## head() - shows first several rows of a dataset
head(x)

## tail() - shows last several rows of a dataset
tail(x)

## View() - opens all of the dataset in a new window
View(x)

## summary() - gives stats like min, 1st quart., median, mean, 3rd quart. and max for all cols in the dataset
summary(x)

## str() - gives information on the datatypes of columns in a dataset
str(x)

## Slicing with [, ]
x[1:10,5] # Returns first 10 rows and column 5.

