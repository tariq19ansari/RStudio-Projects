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



