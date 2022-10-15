####################### Subsetting the data set III: Random Sample  ###########################
## The following code random sample (without replacement) 90% of the original data set and
## assgin them to a new variable iris_sample.
iris_sample_dataset = iris[sample(nrow(iris), nrow(iris) * 0.9), ]

## Summary statistics of every variable
summary(iris_sample_dataset)

## Frequency of species
table(iris_sample_dataset$Species)

## Pie Chart
pie(table(iris_sample_dataset$Species))

############### "Sepal.Length" #########

##Plot histogram and density plot of Sepal.length
hist(iris_sample_dataset$Sepal.Length)

plot(density(iris_sample_dataset$Sepal.Length))



## Parallel Coordinates
library(MASS)
parcoord(iris_sample_dataset[, 1:4], col = iris_sample_dataset$Species)

############### "Sepal.Width" #########
##Plot histogram and density plot of Sepal.Width
hist(iris_sample_dataset$Sepal.Width)

plot(density(iris_sample_dataset$Sepal.Width))

names(iris_sample_dataset)

############## "Petal.Length" ##########

hist(iris_sample_dataset$Petal.Length)

plot(density(iris_sample_dataset$Petal.Length))

############## "Petal.Width" ##########

hist(iris_sample_dataset$Petal.Width)

plot(density(iris_sample_dataset$Petal.Width))


######### Scatter plot of length and width of SEPALS ##########
plot(iris_sample_dataset$Sepal.Length, iris_sample_dataset$Sepal.Width)


######### Scatter plot of length and width of PETALS #########
plot(iris_sample_dataset$Petal.Length, iris_sample_dataset$Petal.Width)
