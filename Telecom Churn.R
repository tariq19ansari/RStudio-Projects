# Get and print current working directory.
print(getwd())
# Set current working directory.
churn <- read.csv("C:/Users/Checkout/Desktop/BUS 235C/Proposal_Telco/Customer-Churn.csv")
# Get the dimension of the file
dim(churn)

#Structure of the data set
str(churn)

######## Check for missing values and which column has missing values
sum(is.na(churn))
sapply(churn, function(x) sum(is.na(x)))
#check missing values in column
churn[is.na(churn$TotalCharges), 1:21]


############### Appendix 4: Remove 11 missing values form totalcharges column ##########
churn <- churn[complete.cases(churn),]
dim(churn)



###############Appendix 5: Cleaning the data  ##############
#change no internet service to no

library(plyr)    ## This library is inbuilt package in R version.
churn_modified <- c(10:15)
for(i in 1:ncol(churn[, churn_modified])){
  churn[,churn_modified][,i] <- as.factor(mapvalues(churn[,churn_modified][,i], from =c("No internet service"), to =c("No")))
}

#######   change no phone service in multiple lines to no
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, from =c("No phone service"), to =c("No")))

#######   change senior citizen values from 0,1 to No,Yes
churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen, from =c("0", "1"), to =c("No", "Yes")))


###########  To check changes in the column above the summary of churn
summary(churn)

#### Remove column id since unique, total charges as it is highly correlated with monthly charges
#for customerid
churn<- churn[, -c(1)]
names(churn)

#####  for monthly charges(check correlation)
cor(churn$TotalCharges, churn$MonthlyCharges)
churn <- churn[, -c(19)]


###########  Appendix 6: Churn Count  ###############
#To check total churn count
churncount <-  table(churn$Churn)/nrow(churn)
churncount

#Splitting data
#Training dataset
set.seed(1)
total.rows <- nrow(churn)
train.size <- floor(0.7 * total.rows)
train.rows <- sample(1:total.rows, train.size)
train <- churn[train.rows, ]
dim(train)


####  Validation dataset
churn.remaining <- churn[-train.rows, ]
dim(churn.remaining)

remaining.rows <- nrow(churn.remaining)
 validate.size <- floor(0.67 * remaining.rows)
validate.rows <- sample(1:remaining.rows, validate.size)
validate <- churn.remaining[validate.rows, ]
dim(validate)


#######   Testing dataset
test <- churn.remaining[-validate.rows, ]
dim(test)

############### Appendix 7: Logistic Model #############
#######         1. Fit Logistic Model              ###################
logisticmodel <- glm(Churn ~ ., data = train, family = "binomial")
glm(churn)
summary(logisticmodel)

#Finding an ROC plot for sensitivity and FPR
#ROC curve
train$Churn <- as.character(train$Churn)
train$Churn[train$Churn == "No"] <- "0"
train$Churn[train$Churn == "Yes"] <- "1"
actual <- train$Churn
predicted.probability <- predict(logisticmodel, type = "response")
roc.table <- data.frame(Threshold = seq(from = 0, to = 1, by = 0.01), FPR = NA, Sensitivity = NA)
#For loop to calculate FPR ans Sensitivity
for (i in 1:nrow(roc.table)){
  #Calculate FPR
  roc.table$FPR[i] <- sum(predicted.probability > roc.table$Threshold[i] & actual == 0)/sum(actual == 0)
  #calculate Sensitivity
  roc.table$Sensitivity[i] <- sum(predicted.probability > roc.table$Threshold[i] & actual == 1)/sum(actual == 1)
}
#plot ROC 
plot(Sensitivity ~ FPR, data = roc.table, type = "s")
abline(a = 0, b = 1, lty = 2)
roc.table

#Accuracy, Sensitivity, Specificity on training dataset
threshold <- 0.5
train$Churn <- as.character(train$Churn)
train$Churn[train$Churn == "No"] <- "0"
train$Churn[train$Churn == "Yes"] <- "1"
predicted.probability <- predict(logisticmodel, type = "response")
predicted <- ifelse(predicted.probability > threshold, 1, 0)
actual <- train$Churn
#Confusion matrix
table(actual, predicted)

#Accuracy
accuracy <- sum(actual == predicted)/nrow(train)
accuracy

#Sensitivity
sensitivity <- sum(actual == 1 & predicted == 1)/sum(actual == 1)
sensitivity

#Specificity
specificity <- sum(actual == 0 & predicted == 0)/sum(actual == 0)
specificity


#For Validation dataset
threshold <- 0.5
validate$Churn <- as.character(validate$Churn)
validate$Churn[validate$Churn == "No"] <- "0"
validate$Churn[validate$Churn == "Yes"] <- "1"
predicted.probability <- predict(logisticmodel, newdata = validate, type = "response")
predicted <- ifelse(predicted.probability > threshold, 1, 0)
actual <- validate$Churn
#Confusion matrix
table(actual, predicted)


#Accuracy
accuracy <- sum(actual == predicted)/nrow(validate)
accuracy

#Sensitivity
sensitivity <- sum(actual == 1 & predicted == 1)/sum(actual == 1)
sensitivity

#Specificity
specificity <- sum(actual == 0 & predicted == 0)/sum(actual == 0)
specificity

###Fit new model with most significant values
#Setting model for significant values
#Changing Churn to numeric from character
train$Churn <- as.numeric(as.character(train$Churn))
logisticmodelnew <- glm(Churn ~ tenure + MultipleLines + Contract + PaperlessBilling + PaymentMethod, data = train, family = "binomial")
summary(logisticmodelnew)

#Accuracy, Sensitivity, Specificity on training data for new model logisticmodelnew
threshold <- 0.5
train$Churn <- as.character(train$Churn)
train$Churn[train$Churn == "No"] <- "0"
train$Churn[train$Churn == "Yes"] <- "1"
predicted.probability <- predict(logisticmodelnew, type = "response")
predicted <- ifelse(predicted.probability > threshold, 1, 0)
actual <- train$Churn

#Confusion Matrix
table(actual, predicted)


#Accuracy
accuracy <- sum(actual == predicted)/nrow(train)


#Sensitivity
sensitivity <- sum(actual == 1 & predicted == 1)/sum(actual == 1)


#Specificity
specificity <- sum(actual == 0 & predicted == 0)/sum(actual == 0)


####For Validation dataset
#Accuracy, Sensitivity, Specificity on validation data for new model logisticmodelnew
threshold <- 0.5
validate$Churn <- as.character(validate$Churn)
validate$Churn[validate$Churn == "No"] <- "0"
validate$Churn[validate$Churn == "Yes"] <- "1"
predicted.probability <- predict(logisticmodelnew, newdata = validate, type = "response")
predicted <- ifelse(predicted.probability > threshold, 1, 0)
actual <- validate$Churn

#Confusion matrix
table(actual, predicted)

#Accuracy
accuracy <- sum(actual == predicted)/nrow(validate)


#Sensitivity
sensitivity <- sum(actual == 1 & predicted == 1)/sum(actual == 1)


#Specificity
specificity <- sum(actual == 0 & predicted == 0)/sum(actual == 0)

####Model for all significant values
#Setting model for all significant values
#Changing Churn to numeric from character
 train$Churn <- as.numeric(as.character(train$Churn))
 logisticmodelnew1 <- glm(Churn ~ SeniorCitizen + tenure + MultipleLines + InternetService + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod, data = train, family = "binomial")
 summary(logisticmodelnew1)

#ROC curve
train$Churn <- as.character(train$Churn)
train$Churn[train$Churn == "No"] <- "0"
train$Churn[train$Churn == "Yes"] <- "1"
actual <- train$Churn
predicted.probability <- predict(logisticmodelnew1, type = "response")
roc.table <- data.frame(Threshold = seq(from = 0, to = 1, by = 0.01), FPR = NA, Sensitivity = NA)
#For loop to calculate FPR ans Sensitivity
 for (i in 1:nrow(roc.table)){

       roc.table$FPR[i] <- sum(predicted.probability > roc.table$Threshold[i] & actual == 0)/sum(actual == 0)
      
       roc.table$Sensitivity[i] <- sum(predicted.probability > roc.table$Threshold[i] & actual == 1)/sum(actual == 1)
        }

plot(Sensitivity ~ FPR, data = roc.table, type = "s")
abline(a = 0, b = 1, lty = 2)

#Accuracy, Sensitivity, Specificity on training data for new model logisticmodelnew1
threshold <- 0.5
train$Churn <- as.character(train$Churn)
train$Churn[train$Churn == "No"] <- "0"
train$Churn[train$Churn == "Yes"] <- "1"
predicted.probability <- predict(logisticmodelnew1, type = "response")
predicted <- ifelse(predicted.probability > threshold, 1, 0)

actual <- train$Churn
 #Confusion Matrix
 table(actual, predicted)

#Accuracy
accuracy <- sum(actual == predicted)/nrow(train)


#Sensitivity
sensitivity <- sum(actual == 1 & predicted == 1)/sum(actual == 1)

#Specificity
specificity <- sum(actual == 0 & predicted == 0)/sum(actual == 0)



#Accuracy, Sensitivity, Specificty on validation data for new model logisticmodelnew1
threshold <- 0.5
validate$Churn <- as.character(validate$Churn)
validate$Churn[validate$Churn == "No"] <- "0"
validate$Churn[validate$Churn == "Yes"] <- "1"
predicted.probability <- predict(logisticmodelnew1, newdata = validate, type = "response")
predicted <- ifelse(predicted.probability > threshold, 1, 0)
actual <- validate$Churn

#Confusion matrix
  
table(actual, predicted)

#Accuracy
accuracy <- sum(actual == predicted)/nrow(validate)

 #Sensitivity
sensitivity <- sum(actual == 1 & predicted == 1)/sum(actual == 1)

#Specificity
specificity <- sum(actual == 0 & predicted == 0)/sum(actual == 0)

#Accuracy, Sensitivity, Specificity on testing data for model logisticmodelnew
threshold <- 0.5
test$Churn <- as.character(test$Churn)
test$Churn[test$Churn == "No"] <- "0"
test$Churn[test$Churn == "Yes"] <- "1"
predicted.probability <- predict(logisticmodelnew, newdata = test, type = "response")
predicted <- ifelse(predicted.probability > threshold, 1, 0)
actual <- test$Churn

#Accuracy
 accuracy <- sum(actual == predicted)/nrow(test)

#Sensitivity
sensitivity <- sum(actual == 1 & predicted == 1)/sum(actual == 1)

#Specificity
specificity <- sum(actual == 0 & predicted == 0)/sum(actual == 0)

#Training accuracy, sensitivity with threshold level 0.39
threshold <- 0.39
train$Churn <- as.character(train$Churn)
train$Churn[train$Churn == "No"] <- "0"
train$Churn[train$Churn == "Yes"] <- "1"
predicted.probability <- predict(logisticmodelnew, type = "response")
predicted <- ifelse(predicted.probability > threshold, 1, 0)
actual <- train$Churn
accuracy <- sum(actual == predicted)/nrow(train)


sensitivity <- sum(actual == 1 & predicted == 1)/sum(actual == 1)

specificity <- sum(actual == 0 & predicted == 0)/sum(actual == 0)

validate$Churn <- as.character(validate$Churn)
validate$Churn[validate$Churn == "No"] <- "0"
validate$Churn[validate$Churn == "Yes"] <- "1"
predicted.probability <- predict(logisticmodelnew, newdata = validate, type = "response")
predicted <- ifelse(predicted.probability > threshold, 1, 0)
actual <- validate$Churn
accuracy <- sum(actual == predicted)/nrow(validate)

sensitivity <- sum(actual == 1 & predicted == 1)/sum(actual == 1)
specificity <- sum(actual == 0 & predicted == 0)/sum(actual == 0)

test$Churn <- as.character(test$Churn)
test$Churn[test$Churn == "No"] <- "0"
test$Churn[test$Churn == "Yes"] <- "1"
predicted.probability <- predict(logisticmodelnew, newdata = test, type = "response")
predicted <- ifelse(predicted.probability > threshold, 1, 0)
actual <- test$Churn

#Accuracy
 accuracy <- sum(actual == predicted)/nrow(test)
sensitivity <- sum(actual == 1 & predicted == 1)/sum(actual == 1)
specificity <- sum(actual == 0 & predicted == 0)/sum(actual == 0)


################# Decision Tree ######################

###Creating a tree with all the columns
churn_fulltree <- rpart(Churn ~ ., data = train, method = "class", + control = rpart.control(minsplit = 0, cp=0))
library(rpart.plot)
rpart.plot(churn_fulltree)
##Creating a full tree with fancyRpart plot
library(rattle)
fancyRpartPlot(churn_fulltree)

##Classification tree:
  rpart(formula = Churn ~ ., data = train, method = "class", control = rpart.control(minsplit = 0, cp = 0))
printcp(churn_fulltree)

##Classification tree:
  rpart(formula = Churn ~ ., data = train, method = "class", control = rpart.control(minsplit = 0, cp = 0))


######  Tree that we will choose using 1-SE rule will be having 3 splits.
  pruned_tree <- prune(churn_fulltree, cp = 0.03)
  rpart.plot(pruned_tree)
###  Accuracy for training dataset
actual_train <- train$Churn
predicted_train <- predict(pruned_tree , type ="class")
sum(actual_train == predicted_train)/nrow(train)

##sensitivity for training dataset
sum(predicted_train == "Yes" & actual_train == "Yes")/sum(actual_train=="Yes")

##specificity for training dataset
sum(predicted_train == "No" & actual_train == "No")/sum(actual_train=="No")

table(actual_train, predicted_train)

####Accuracy for validation set
  actual_validate <- validate$Churn
  predicted_validate <- predict(pruned_tree, type ="class", newdata = validate)
  table(actual_validate, predicted_validate)
  
  sum(actual_validate == predicted_validate)/nrow(validate)
  
  ##Sensitivity on validation set
  sum(predicted_validate == "Yes" & actual_validate == "Yes")/sum(actual_validate=="Yes")
  
  ##Specificity on validation set
  sum(predicted_validate == "No" & actual_validate == "No")/sum(actual_validate=="No")
  
######Accuracy for Testing dataset
actual_test <- test$Churn
predicted_test <- predict(pruned_tree, type ="class", newdata = test)
table(actual_test, predicted_test)
sum(actual_test == predicted_test)/nrow(test)
##Sensitivity on testing set
sum(predicted_test == "Yes" & actual_test == "Yes")/sum(actual_test=="Yes")
##Specificity on testing set
sum(predicted_test == "No" & actual_test == "No")/sum(actual_test=="No")

######Creating a new tree model with next lower validation error
  pruned_tree_new <- prune(churn_fulltree, cp = 0.004)
rpart.plot(pruned_tree)
# For training dataset
actual_train <- train$Churn
predicted_train <- predict(pruned_tree_new , type ="class")
table(actual_train, predicted_train)
#####Accuracy for training dataset
sum(actual_train == predicted_train)/nrow(train)

########Sensitivity for training dataset
  sum(predicted_train == "Yes" & actual_train == "Yes")/sum(actual_train=="Yes")

#########Specificity for training dataset
  um(predicted_train == "No" & actual_train == "No")/sum(actual_train=="No")

####### For Validation dataset
actual_validate <- validate$Churn 
predicted_validate <- predict(pruned_tree_new, type ="class", newdata = validate)
table(actual_validate, predicted_validate)
####Accuracy for validation dataset
 sum(actual_validate == predicted_validate)/nrow(validate)

####Sensitivity for validation dataset
  sum(predicted_validate == "Yes" & actual_validate == "Yes")/sum(actual_validate=="Yes")

###Specificity for validation dataset
  sum(predicted_validate == "No" & actual_validate == "No")/sum(actual_validate=="No")

###FPR for validation dataset = 1 - Specificity = 0.0964
# ##For Testing dataset
actual_test <- test$Churn 
predicted_test <- predict(pruned_tree_new, type ="class", newdata = test)
table(actual_test, predicted_test)

###Accuracy for testing dataset
  sum(actual_test == predicted_test)/nrow(test)

###Sensitivity for testing dataset
  sum(predicted_test == "Yes" & actual_test == "Yes")/sum(actual_test=="Yes")

####Specificity for testing dataset
  sum(predicted_test == "No" & actual_test == "No")/sum(actual_test=="No")

######################  Ensemble Method Random Forest #############
 ####### With Default trees, ntree = 500
  ##Accuracy on training set 
library(randomForest)
forest_model <- randomForest(Churn ~ ., data = train)
actual_train <- train$Churn
 predicted_train <- predict(forest_model , newdata = train)
 sum(actual_train==predicted_train)/nrow(train)

##  ##Sensitivity on training set
  table(actual_train, predicted_train)
  

sum(predicted_train == "Yes" & actual_train == "Yes")/sum(actual_train=="Yes")
 
  ##Specificity on training set 
  sum(predicted_train == "No" & actual_train == "No")/sum(actual_train=="No")
  
 
  ##Accuracy on Validation Set
actual_validate <- validate$Churn
predicted_validate <- predict(forest_model , newdata = validate)
sum(actual_validate==predicted_validate)/nrow(validate)

  ##Sensitivity on validation set
table(actual_validate, predicted_validate)
sum(predicted_validate == "Yes" & actual_validate ==   "Yes")/sum(actual_validate=="Yes")

####  ##Specificity on Validation set
sum(predicted_validate == "No" & actual_validate == "No")/sum(actual_validate=="No")

  ##Accuracy on Testing Set
 actual_test <- test$Churn
predicted_test <- predict(forest_model , newdata = test)
sum(actual_test==predicted_test)/nrow(test)

###Sensitivity on Testing set
table(actual_test, predicted_test)  
         
sum(predicted_test == "Yes" & actual_test == "Yes")/sum(actual_test=="Yes")

####Specificity on Testing set
sum(predicted_test == "No" & actual_test == "No")/sum(actual_test=="No")


##########With ntree = 5
##Accuracy on training set
forestmodel_5 <- randomForest(Churn ~ ., data = train , ntree = 5 )
actual_train5 <- train$Churn
predicted_train5 <- predict(forestmodel_5 , newdata = train)
sum(actual_train5==predicted_train5)/nrow(train)
 ##Sensitivity on training set
table(actual_train5, predicted_train5)
sum(predicted_train5 == "Yes" & actual_train5 == "Yes")/sum(actual_train5=="Yes")


##Specificity on training set
 sum(predicted_train5 == "No" & actual_train5 == "No")/sum(actual_train5=="No")


##FPR on training Set

##Accuracy on validation set
actual_validate5 <- validate$Churn
predicted_validate5 <- predict(forestmodel_5 , newdata = validate)
sum(actual_validate5==predicted_validate5)/nrow(validate)

##Sensitivity on validation set
table(actual_validate5, predicted_validate5)

sum(predicted_validate5 == "Yes" & actual_validate5 == "Yes")/sum(actual_validate5=="Yes")

##Specificity on validation set
sum(predicted_validate5 == "No" & actual_validate5 == "No")/sum(actual_validate5=="No")


##Accuracy on testing set
actual_test5 <- test$Churn
predicted_test5 <- predict(forestmodel_5 , newdata = test)
sum(actual_test5==predicted_test5)/nrow(test)

##Sensitivity on testing set
table(actual_test5, predicted_test5)
sum(predicted_test5 == "Yes" & actual_test5 == "Yes")/sum(actual_test5=="Yes")

##Specificity on testing set
sum(predicted_test5 == "No" & actual_test5 == "No")/sum(actual_test5=="No")


######With ntree = 25 ##########

##Accuracy on training set
 forestmodel_25 <- randomForest(Churn ~ ., data = train , ntree = 25 )
 actual_train25 <- train$Churn
 predicted_train25 <- predict(forestmodel_25 , newdata = train)
 sum(actual_train25==predicted_train25)/nrow(train)

##Sensitivity on training set
table(actual_train25, predicted_train25)

sum(predicted_train25 == "Yes" & actual_train25 == "Yes")/sum(actual_train25=="Yes")

##Specificity on training set
sum(predicted_train25 == "No" & actual_train25 == "No")/sum(actual_train25=="No")

##Accuracy on validation set
actual_validate25 <- validate$Churn
predicted_validate25 <- predict(forestmodel_25 , newdata = validate)
sum(actual_validate25==predicted_validate25)/nrow(validate)


##Sensitivity on validation set
table(actual_validate25, predicted_validate25)

sum(predicted_validate25 == "Yes" & actual_validate25 == "Yes")/sum(actual_validate25=="Yes")

##Specificity on validation set
sum(predicted_validate25 == "No" & actual_validate25 == "No")/sum(actual_validate25=="No")

##Accuracy on testing set
actual_test25 <- test$Churn
predicted_test25 <- predict(forestmodel_25 , newdata = test)
sum(actual_test25==predicted_test25)/nrow(test)

##Sensitivity on testing set
table(actual_test25, predicted_test25)
sum(predicted_test25 == "Yes" & actual_test25 == "Yes")/sum(actual_test25=="Yes")

##Specificity on testing set
sum(predicted_test25 == "No" & actual_test25 == "No")/sum(actual_test25=="No")


######With ntree = 101
##Accuracy on training set
forestmodel_101 <- randomForest(Churn ~ ., data = train , ntree = 101 )
actual_train101 <- train$Churn
predicted_train101 <- predict(forestmodel_101 , newdata = train)
sum(actual_train101==predicted_train101)/nrow(train)

##Sensitivity on training set
table(actual_train101, predicted_train101)
sum(predicted_train101 == "Yes" & actual_train101 == "Yes")/sum(actual_train101=="Yes")

##Specificity on training set
sum(predicted_train101 == "No" & actual_train101 == "No")/sum(actual_train101=="No")

##Accuracy on validation set
actual_validate101 <- validate$Churn
predicted_validate101 <- predict(forestmodel_101 , newdata = validate)
sum(actual_validate101==predicted_validate101)/nrow(validate)


##Sensitivity on validation set
table(actual_validate101, predicted_validate101)
sum(predicted_validate101 == "Yes" & actual_validate101 == "Yes")/sum(actual_validate101=="Yes")


##Specificity on validation set
sum(predicted_validate101 == "No" & actual_validate101 == "No")/sum(actual_validate101=="No")


##Accuracy on testing set
actual_test101 <- test$Churn
predicted_test101 <- predict(forestmodel_101 , newdata = test)
sum(actual_test101==predicted_test101)/nrow(test)


##Sensitivity on testing set
table(actual_test101, predicted_test101)
sum(predicted_test101 == "Yes" & actual_test101 == "Yes")/sum(actual_test101=="Yes")


##Specificity on testing set
sum(predicted_test101 == "No" & actual_test101 == "No")/sum(actual_test101=="No")

          

######## With ntree = 1101
##Accuracy on training set
forestmodel_1101 <- randomForest(Churn ~ ., data = train , ntree = 1101 )
actual_train1101 <- train$Churn
predicted_train1101 <- predict(forestmodel_1101 , newdata = train)
sum(actual_train1101==predicted_train1101)/nrow(train)


##Sensitivity on training set
table(actual_train1101, predicted_train1101)
sum(predicted_train1101 == "Yes" & actual_train1101 == "Yes")/sum(actual_train1101=="Yes")


##Specificity on training set
sum(predicted_train1101 == "No" & actual_train1101 == "No")/sum(actual_train1101=="No")



##Accuracy on validation set
actual_validate1101 <- validate$Churn
predicted_validate1101 <- predict(forestmodel_1101 , newdata = validate)
sum(actual_validate1101==predicted_validate1101)/nrow(validate)


##Sensitivity on validation set
table(actual_validate1101, predicted_validate1101)
sum(predicted_validate1101 == "Yes" & actual_validate1101 == "Yes")/sum(actual_validate1101=="Yes")


##Specificity on validation set
sum(predicted_validate1101 == "No" & actual_validate1101 == "No")/sum(actual_validate1101=="No")

##Accuracy on testing set
actual_test1101 <- test$Churn
predicted_test1101 <- predict(forestmodel_1101 , newdata = test)
sum(actual_test1101==predicted_test1101)/nrow(test)



##Sensitivity on testing set
table(actual_test1101, predicted_test1101)
sum(predicted_test1101 == "Yes" & actual_test1101 == "Yes")/sum(actual_test1101=="Yes")


##Specificity on testing set
sum(predicted_test1101 == "No" & actual_test1101 == "No")/sum(actual_test1101=="No")


############## With ntree = 1601 ############
##Accuracy on training set
forestmodel_1601 <- randomForest(Churn ~ ., data = train , ntree = 1601 )
actual_train1601 <- train$Churn
predicted_train1601 <- predict(forestmodel_1601 , newdata = train)
sum(actual_train1601==predicted_train1601)/nrow(train)


##Sensitivity on training set
table(actual_train1601, predicted_train1601)
sum(predicted_train1601 == "Yes" & actual_train1601 == "Yes")/sum(actual_train1601=="Yes")


##Specificity on training set
sum(predicted_train1601 == "No" & actual_train1601 == "No")/sum(actual_train1601=="No")


##Accuracy on validation set
actual_validate1601 <- validate$Churn
predicted_validate1601 <- predict(forestmodel_1601 , newdata = validate)
sum(actual_validate1601==predicted_validate1601)/nrow(validate)


##Sensitivity on validation set
table(actual_validate1601, predicted_validate1601)
sum(predicted_validate1601 == "Yes" & actual_validate1601 == "Yes")/sum(actual_validate1601=="Yes")


##Specificity on validation set
sum(predicted_validate1601 == "No" & actual_validate1601 == "No")/sum(actual_validate1601=="No")


##Accuracy on testing set
actual_test1601 <- test$Churn
predicted_test1601 <- predict(forestmodel_1601 , newdata = test)
sum(actual_test1601==predicted_test1601)/nrow(test)


 ##Sensitivity on testing set
table(actual_test1601, predicted_test1601)
sum(predicted_test1601 == "Yes" & actual_test1601 == "Yes")/sum(actual_test1601=="Yes")

##Specificity on testing set
sum(predicted_test1601 == "No" & actual_test1601 == "No")/sum(actual_test1601=="No")

#################  KNN Method  #############
##Separating the training data into x and y variables
## x will have all the columns from 1 to 18
## y will have the 19th column "Churn"
library(class)
x <- sapply(train[ , 1:18] , as.numeric)
y <- train[ , 19]
##normalizing
normalize <- function(numbers) {(numbers - mean(numbers))/sd(numbers) }
x.normalized <- apply(x, 2, normalize)
##checking if it is normalized
apply(x.normalized, 2, mean)
apply(x.normalized, 2, sd)
## Let's apply knn to the normalized data
## Let's start by finding the optimal value of k
  for (k in 1:20) {
    predicted <- knn.cv(x.normalized, y, k)
    print(paste("With", k, "neighbors the accuracy is", sum(y == predicted)/nrow(x.normalized)))
  }









