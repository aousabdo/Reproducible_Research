
library(kernlab)

## Split the data set into two ~ equal in length sub data sets, one will be our 
## training data set and one will be our test data set

data(spam)
Obs <- nrow(spam)

set.seed(3435)
trainIndicator <- rbinom(n=Obs, size=1, prob=0.5) 
table(trainIndicator)

trainSpam <- spam[ trainIndicator==1,]
testSpam  <- spam[ trainIndicator==0,]

table(trainSpam$type)
table(testSpam$type)

hist(log10(spam$capitalAve))
plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve+1) ~ trainSpam$type)

plot(log10(spam[, 1:4]+1))

hCluster = hclust(dist(t(trainSpam[, 1:57])))
plot(hCluster)

hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:57] + 1))))
plot(hClusterUpdated)

## add new column which we will use as our logistic predictory
trainSpam$numType <- as.numeric(trainSpam$type) -1

costFunction <- function(x, y) sum(x != (y>0.5))
cvError <- rep(NA, 55)

library(boot)
for (i in 1:55) {
  ## Create formula for the logistic fit. The response in each case is the 
  ## newly created numType binomial response
  lmFormula = reformulate(names(trainSpam)[i], response = "numType")
  ## fit a logistic model to each variable 
  glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
  ## Get the cross validated error for each case
  cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}
names(trainSpam)[which.min(cvError)]


## Use the best model from the group
predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam)

## Get predictions on the test set
predictionTest = predict(predictionModel, testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])

## Classify as `spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"

## Classification table
table(predictedSpam, testSpam$type)

## Error rate 
(61+458)/(61+458+1346+449)
