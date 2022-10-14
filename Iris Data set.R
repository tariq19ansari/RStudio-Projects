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
