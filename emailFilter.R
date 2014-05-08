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

