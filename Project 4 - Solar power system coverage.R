
# Jamie Kennedy - 17372983

deepsolar = read.csv("projectdata.csv", header = TRUE)

is.na(deepsolar)
which(is.na(deepsolar))

#make sure reponse variabe is binary with high coded as 1 and low coded as 0
deepsolar$solar_system_count <- ifelse(deepsolar$solar_system_count == "high", 1, 0) 
str(deepsolar)

# make sure categorical variables are factors
deepsolar$solar_system_count = as.factor(deepsolar$solar_system_count)
deepsolar$voting_2016_dem_win = as.factor(deepsolar$voting_2016_dem_win)
deepsolar$voting_2012_dem_win = as.factor(deepsolar$voting_2012_dem_win)
str(deepsolar)

# rename response variable
colnames(deepsolar)[colnames(deepsolar) == "solar_system_count"] <- "count"






#### EDA  ################

library(DataExplorer)

#get general idea
introduce(deepsolar)
plot_intro(deepsolar)


# to visualize continuous features
plot_histogram(deepsolar)

# to visualize discrete features
plot_bar(deepsolar) 


# remove correlated variables
library(caret)
correlationmatrix <- cor(Filter(is.numeric,deepsolar))
highlycorrelated <- findCorrelation(correlationmatrix, names=TRUE)
print(highlycorrelated)
deepsolar2 <- deepsolar[, !colnames(deepsolar) %in% highlycorrelated]
str(deepsolar2)


# scale continuous variables
for (colName in names(deepsolar2)) {
  
  if(class(deepsolar2[,colName]) == 'integer' | class(deepsolar2[,colName]) == 'numeric') {
    deepsolar2[,colName] <- scale(deepsolar2[,colName])
  }
}

str(deepsolar2)





### split data into training and test sets
set.seed(999)
N <- nrow(deepsolar2)  
keep <- sample(1:N, size=0.7*N) 
test <- setdiff(1:N, keep)  
dat <- deepsolar2[keep,]   
dat_test <- deepsolar2[test,]



### Logistic regression  

library(bestglm)
fitLog1 <- glm(count ~ ., data = dat, family="binomial")
summary(fitLog1)
library(ROCR) 
predObj <- prediction(fitted(fitLog1), dat$count) 
perf <- performance(predObj, "tpr", "fpr")
plot(perf) 
abline(0,1, col = "darkorange2", lty = 2)
# compute the area under the ROC curve 
auc <- performance(predObj, "auc")
auc
auc@y.values
#
sens <- performance(predObj, "sens") 
spec <- performance(predObj, "spec")
tau <- sens@x.values[[1]]
sensSpec <- sens@y.values[[1]] + spec@y.values[[1]]
best <- which.max(sensSpec) 
pred <- ifelse(fitted(fitLog1) > tau[best], 1, 0)
tab = table(dat$count, pred)
acc <- sum(diag(tab))/sum(tab)
acc
plot(tau, sensSpec, type = "l")
points(tau[best], sensSpec[best], pch = 19, col = adjustcolor("darkorange2", 0.5))
tau[best]


### classification tree

library(rpart) 
library(partykit)
ct <- rpart(count ~ ., data = dat) 
plot( as.party(ct), cex = 0.5 )
summary(ct)
# find class probabilities
phat <- predict(ct) 
head(phat)   # just gives sample of the values
# find predicted classes
class <- predict(ct, type = "class")   
head(class)   # just gives sample of the values
table(dat$count, class)
# look at Roc curve
library(ROCR)
predObj <- prediction(phat[,2], dat$count) 
roc <- performance(predObj, "tpr", "fpr")
plot(roc)
abline(0,1, col = "darkorange2", lty = 2)
# compute the area under the ROC curve
auc <- performance(predObj, "auc")
auc@y.values



### Support vector machine
library(kernlab)
Svm <- ksvm(count ~ ., data = dat) 
predSvm <- predict(Svm)
tabSvm <- table(dat$count, predSvm)
tabSvm
accSvm <- sum(diag(tabSvm))/sum(tabSvm)
accSvm



### Random Forest
library(randomForest)
rf <- randomForest(count ~ ., data = dat, importance = TRUE)
rf
pred <- predict(rf, type = "class")
tab <- table(dat$count, pred)
tab



######## Find accuracy of models

# replicate the process a number of times
R <- 100
out <- matrix(NA, R, 5)
colnames(out) <- c("val_class_tree", "val_logistic", "val_RF", "val_svm", "best")
out <- as.data.frame(out)
M = nrow(dat)


for ( r in 1:R ) {
  
  # split the data
  train <- sample(1:M, size = 0.75*M)                      
  val <- setdiff(1:M, train)
  
  # fit classifiers to only the training data
  fitCt <- rpart(count ~ ., data = dat, subset = train)        
  fitLog <- glm(count ~ ., data = dat, subset = train, family="binomial")
  fitrf <- randomForest(count ~ ., data = dat, subset=train)
  fitSvm <- ksvm(count ~ ., data = dat[train,]) # svm
  
  # classify the validation data observations
  predValCt <- predict(fitCt, type = "class", newdata = dat[val,]) 
  tabValCt <- table(dat$count[val], predValCt)
  accCt <- sum(diag(tabValCt))/sum(tabValCt)
  #
  predValLog <- predict(fitLog, type = "response", newdata = dat[val,])  
  predValLog <- ifelse(predValLog > 0.56, 1, 0)  # using optimal tau
  tabValLog <- table(dat$count[val], predValLog)
  accLog <- sum(diag(tabValLog))/sum(tabValLog)
  #
  predValrf <- predict(fitrf, type = "class", newdata = dat[val,])  
  tabValrf <- table(dat$count[val], predValrf)
  accrf <- sum(diag(tabValrf))/sum(tabValrf)
  #
  predValSvm <- predict(fitSvm, newdata = dat[val,])
  tabValSvm <- table(dat$count[val], predValSvm)
  accSvm <- sum(diag(tabValSvm))/sum(tabValSvm)
  
  
  # accuracy
  acc <- c(class_tree = accCt, logistic = accLog, Random_forest = accrf, Svm = accSvm)
  out[r,1] <- accCt
  out[r,2] <- accLog
  out[r,3] <- accrf
  out[r,4] <- accSvm
  
  
  # use the method that did best on the validation data 
  # to predict the test data
  best <- names( which.max(acc) )
  
  out[r,5] <- best
  
  print(r)
}


# check out the error rate summary statistics
table(out[,5])
out

result = out[-c(5)]
meanAcc = colMeans(result)
meanAcc
sdAcc <- apply(result, 2, sd)/sqrt(R)
sdAcc

# plot
matplot(result, type = "l", lty = c(2,3,4,5), 
        col = c("darkorange2", "deepskyblue3","chartreuse","darkorchid1"), 
        xlab = "Replications", ylab = "Accuracy")

# add confidence intervals 
bounds1 <- rep( c(meanAcc[1] - 2*sdAcc[1], meanAcc[1] + 2*sdAcc[1]), each = R )
bounds2 <- rep( c(meanAcc[2] - 2*sdAcc[2], meanAcc[2] + 2*sdAcc[2]), each = R )
bounds3 <- rep( c(meanAcc[3] - 2*sdAcc[3], meanAcc[3] + 2*sdAcc[3]), each = R )
bounds4 <- rep( c(meanAcc[4] - 2*sdAcc[4], meanAcc[4] + 2*sdAcc[4]), each = R )
polygon(c(1:R, R:1), bounds1, col = adjustcolor("darkorange2", 0.2), border = FALSE) 
polygon(c(1:R, R:1), bounds2, col = adjustcolor("deepskyblue3", 0.2), border = FALSE)
polygon(c(1:R, R:1), bounds3, col = adjustcolor("chartreuse", 0.2), border = FALSE) 
polygon(c(1:R, R:1), bounds4, col = adjustcolor("darkorchid1", 0.2), border = FALSE)

# add estimated mean line 
abline(h = meanAcc, col = c("darkorange2", "deepskyblue3", "chartreuse", "darkorchid1"))

# add legend 
legend("bottomleft", fill = c("chartreuse", "darkorchid1", "deepskyblue3", "darkorange2"),
       legend = c("Random forest","Support vector machine",
                  "Logistic","Classification tree"), bty = "n")





### Train best model on dat and use it to classify dat_test
fitrf2 <- randomForest(count ~ ., data = dat, importance = TRUE)
fitrf2
varImpPlot(fitrf2, type = 1)

# The use this model to predict test data
predTestrf2 <- predict(fitrf2, type = "class", newdata = dat_test)
tabTestrf2 <- table(dat_test$count, predTestrf2)
tabTestrf2
accuracy <- sum(diag(tabTestrf2))/sum(tabTestrf2)
accuracy
