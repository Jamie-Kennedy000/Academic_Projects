
# Jamie Kennedy - 17372983

# get data
library(mlbench) 
data("Satellite")

Satellite$classes <- gsub(" ", "_", Satellite$classes) 
Satellite$classes <- factor( as.character(Satellite$classes) )

# to have the same initial split 
set.seed(777222)
N <- nrow(Satellite)  
keep <- sample(1:N, 5500) 
test <- setdiff(1:N, keep)  
dat <- Satellite[keep,]   
dat_test <- Satellite[test,]



# Data Visualization

library(DataExplorer)

#get general idea
introduce(Satellite)
plot_intro(Satellite)

# Boxplots
plot_boxplot(Satellite, by="classes")

# to visualize continuous features
plot_histogram(Satellite)

# to visualize discrete features
plot_bar(Satellite) 



M = nrow(dat)

# replicate the process a number of times
R <- 100
out <- matrix(NA, R, 3)
colnames(out) <- c("val_logistic", "val_rf", "best")
out <- as.data.frame(out)

library(randomForest)
library(nnet)

for ( r in 1:R ) {
  
  # split the data
  train <- sample(1:M, size = 0.75*M)                      
  val <- setdiff(1:M, train)    
  
  # fit classifiers to only the training data
  fitLog <- multinom(classes ~ ., data = dat, subset = train,maxit=500)
  fitrf <- randomForest(classes ~ ., data = dat[train,], importance = TRUE)
  
  # classify the validation data observations
  predValLog <- predict(fitLog, type = "class", newdata = dat[val,])    # logistic
  tabValLog <- table(dat$classes[val], predValLog)
  tabValLog
  accLog <- sum(diag(tabValLog))/sum(tabValLog)
  #
  predValrf <- predict(fitrf, type = "class", newdata = dat[val,])  # random forest
  tabValrf <- table(dat$classes[val], predValrf)
  tabValrf
  accrf <- sum(diag(tabValrf))/sum(tabValrf)
  
  # accuracy
  acc <- c(logistic = accLog, random_forest = accrf)
  out[r,1] <- accLog
  out[r,2] <- accrf
  
  best <- names( which.max(acc) )
  
  out[r,3] <- best
  
  print(r)
}


# check out the error rate summary statistics 
table(out[,3])


result = out[-c(3)]
meanAcc = colMeans(result)
meanAcc
sdAcc <- apply(result, 2, sd)/sqrt(R)
sdAcc

# plot
matplot(result, type = "l", lty = c(2,3), col = c("darkorange2", "deepskyblue3"), 
        xlab = "Replications", ylab = "Accuracy")

# add confidence intervals 
bounds1 <- rep( c(meanAcc[1] - 2*sdAcc[1], meanAcc[1] + 2*sdAcc[1]), each = R )
bounds2 <- rep( c(meanAcc[2] - 2*sdAcc[2], meanAcc[2] + 2*sdAcc[2]), each = R )
polygon(c(1:R, R:1), bounds1, col = adjustcolor("darkorange2", 0.2), border = FALSE) 
polygon(c(1:R, R:1), bounds2, col = adjustcolor("deepskyblue3", 0.2), border = FALSE)

# add estimated mean line 
abline(h = meanAcc, col = c("darkorange2", "deepskyblue3"))

# add legend 
legend("bottomleft", fill = c("darkorange2", "deepskyblue3"),
       legend = c("logistic", "Random Forest"), bty = "n")



# Train better model on dat so you can then use it to predict the test data
fitrf2 <- randomForest(classes ~ ., data = dat, importance = TRUE)
fitrf2
varImpPlot(fitrf2, type = 1)

# The use this model to predict test data
predTestrf2 <- predict(fitrf2, type = "class", newdata = dat_test)
tabTestrf2 <- table(dat_test$classes, predTestrf2)
tabTestrf2
accuracy <- sum(diag(tabTestrf2))/sum(tabTestrf2)
accuracy

