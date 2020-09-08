library(kernlab)
data("spam")
spam
?spam
spam = spam[,c(49:58)]
str(spam)

# make sure response variable is binary
spam$type <- ifelse(spam$type == "spam", 1, 0) # sets spam as 1 and nonspam as 0
str(spam)

# visualize data
a=length(spam$type)
x = seq(1,a,1)
plot(x,spam$type,xlab="x",ylab="y")


# make sure categorical variables are factors
spam$type = as.factor(spam$type)
str(spam)



# fit model that finds prob of being spam
fit <- glm(type ~ ., data = spam, family = "binomial")
summary(fit)

# try to improve model with stepwise regression
library(MASS)
st <- stepAIC(fit, k = 2)
summary(st)

# find confidence intervals for coefficients
w <- coef(st)
# compute the odds of the coefficients
exp(w) 
sm <- summary(st) 
sm
se <- sm$coef[,2]  
wLB <- w - 1.96 * se  
wUB <- w + 1.96 * se 
ci <- cbind(lb = wLB, w = w, ub = wUB)  
ci    




# look at plot of log odds against estimated probabilities
lg <- predict(st)   
head(lg)    
phat <- predict(st, type = "response") 
head(phat)
symb <- c(19, 17) 
col <- c("darkorange2", "deepskyblue3")
plot(lg, jitter(phat, amount = 0.1), pch = symb[spam$type], 
     col = adjustcolor(col[spam$type], 0.7), cex = 0.7, xlab = "Log-odds",
     ylab = "Fitted probabilities")



# now want to use estmiated probabilties from model to classify input variables. we do this 
# by setting threshold for probability.
threshold <- 0.5 
p <- fitted(st)
pred <- ifelse(p > threshold, 1, 0)  # assign 1 if prob>threshold and 0 if prob<threshold

# assess performace with cross tabulation of predicted values against actual values
table(spam$type, pred)

#Then want to calculate the different rates used to assess performance(use performance function)
library(ROCR) 
predObj <- prediction(fitted(st), spam$type) # create object with real and predicted values
perf <- performance(predObj, "tpr", "fpr") # this function calculates the different measure
plot(perf) 
abline(0,1, col = "darkorange2", lty = 2) # add bisect line
# compute the area under the ROC curve 
auc <- performance(predObj, "auc")
auc
auc@y.values


# The optimal threshold can be found maximizing the sum of sensitivity and specificity for 
# different values. 
sens <- performance(predObj, "sens") 
spec <- performance(predObj, "spec")
sens
tau <- sens@x.values[[1]]
sensSpec <- sens@y.values[[1]] + spec@y.values[[1]]
best <- which.max(sensSpec) 
plot(tau, sensSpec, type = "l")
points(tau[best], sensSpec[best], pch = 19, col = adjustcolor("darkorange2", 0.5))

tau[best] # optimal tau

# classification for optimal tau
pred <- ifelse(fitted(st) > tau[best], 1, 0) # do classification same as before except use 
# optimal threshold instead of just 0.5
table(spam$type, pred)





