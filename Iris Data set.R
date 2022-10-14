data(iris)
dim(iris)
# Another way to get the dim is to use ncol or nrow:
ncol(iris)
nrow(iris)
# Variable names or column names
names(iris)
colnames(iris)

# Structure of the dataframe, note that the difference between num and Factor
str(iris)

#Subsetting the dataset I: Numerical Index and Variable Names
#Get the first 5 rows/obs
iris[1:5, ]

#Get Sepal.Length of the first 10 rows
iris[1:10, "Sepal.Length"]

# or alternatively
iris$Sepal.Length[1:10]

# or we can use the column index instead of variable names. Since Sepal.Length is the first
#column:
iris[1:10, 1]

################# Subsetting the dataset II: Logical Conditions ###############

## There are 3 ways to get subset of data that satisfy certain logical conditions. These kind of
##operations are called filtering in Excel.

## Suppose we want to get the subset of data that has Sepal.Length > 5 and Sepal.Width >
#4.
# Use subset function
subset(x = iris, subset = Sepal.Length > 5 & Sepal.Width > 4)

## Alternative way -- You can omit the x = and sebset = part
subset(iris, Sepal.Length > 5 & Sepal.Width > 4)


## Use logical vectors
iris[which(iris$Sepal.Length > 5 & iris$Sepal.Width > 4), ]


##  Use SQL statement
install.packages("sqldf")
library(sqldf)
sqldf('select * from iris where "Sepal.Length">5 and "Sepal.Width">4')




