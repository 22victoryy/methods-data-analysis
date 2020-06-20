rm(list = ls())

setwd("~/Dropbox/UofT Admin and TA/STA 302/Lectures/Lecture 12")
## Install UsingR package ##
#  install.packages("UsingR")
## Scatter plot 3d ##
#install.packages("scatterplot3d")

library(glmnet)
library(rms)

### Prostate Data ###
train <- read.table("https://gattonweb.uky.edu/sheather/book/docs/datasets/prostateTraining.txt", header = T)
head(train)
str(train)

test <- read.table("https://gattonweb.uky.edu/sheather/book/docs/datasets/prostateTest.txt", header = T)
    
### First fit a multiple linear regression ##
model.lm <- lm(lpsa ~ ., data = train[, -c(1)])
summary(model.lm)    

## Perform Prediction ##
pred.y <- predict(model.lm, newdata = test, type = "response")

## Prediction error ##
mean((test$lpsa - pred.y)^2)

## Fit a ridge penalty ##
model.ridge <- glmnet(x = as.matrix(train[,2:9]), y = train$lpsa, standardize = T, alpha = 0)

## Perform Prediction ##
pred.y.ridge <- predict(model.ridge, newx = as.matrix(test[,2:9]), type = "response")

## Prediction error ##
mean((test$lpsa - pred.y.ridge)^2)


## Fit a LASSO penalty ##
model.lasso <- glmnet(x = as.matrix(train[,2:9]), y = train$lpsa, standardize = T, alpha = 1)

## Perform Prediction ##
pred.y.lasso <- predict(model.lasso, newx = as.matrix(test[,2:9]), type = "response")

## Prediction error ##
mean((test$lpsa - pred.y.lasso)^2)

## Elastic net ##

model.EN <- glmnet(x = as.matrix(train[,2:9]), y = train$lpsa, standardize = T, alpha = 0.5)

## Perform Prediction ##
pred.y.EN <- predict(model.EN, newx = as.matrix(test[,2:9]), type = "response")

## Prediction error ##
mean((test$lpsa - pred.y.EN)^2)

####### Variable selection #######

## Step wise regression ###

sel.var.aic <- step(model.lm, trace = 0, k = 2) #log(nrow(dat1)))
select_var<-attr(terms(sel.var.aic), "term.labels")   
select_var

### LASSO selection ###

## Perform cross validation to choose lambda ##
set.seed(1002656486)
cv.out <- cv.glmnet(x = as.matrix(train[,2:9]), y = train$lpsa, standardize = T, alpha = 1)
plot(cv.out)
best.lambda <- cv.out$lambda.1se
best.lambda
co<-coef(cv.out, s = "lambda.1se")

#Selection of the significant features(predictors)

## threshold for variable selection ##

thresh <- 0.00
# select variables #
inds<-which(abs(co) > thresh )
variables<-row.names(co)[inds]
sel.var.lasso<-variables[!(variables %in% '(Intercept)')]
sel.var.lasso

## Step wise regression ###

### First fit a multiple linear regression ##
## Based on AIC ##
model.lm <- lm(lpsa ~ ., data = train[, -c(1)])
summary(model.lm)  
n <- nrow(train)
sel.var.aic <- step(model.lm, trace = 0, k = 2, direction = "both") 
sel.var.aic<-attr(terms(sel.var.aic), "term.labels")   
sel.var.aic

## Based on BIC ##
model.lm <- lm(lpsa ~ ., data = train[, -c(1)])
summary(model.lm)  
n <- nrow(train)
sel.var.bic <- step(model.lm, trace = 0, k = log(n), direction = "both") 
sel.var.bic<-attr(terms(sel.var.bic), "term.labels")   
sel.var.bic


### Cross Validation and prediction performance of AIC based selection ###
ols.aic <- ols(lpsa ~ ., data = train[,which(colnames(train) %in% c(sel.var.aic, "lpsa"))], 
               x=T, y=T, model = T)

## 10 fold cross validation ##    
aic.cross <- calibrate(ols.aic, method = "crossvalidation", B = 10)
## Calibration plot ##
pdf("aic_cross.pdf", height = 8, width = 16)
plot(aic.cross, las = 1, xlab = "Predicted Probability", main = "Cross-Validation calibration with AIC")
dev.off()

## Test Error ##
pred.aic <- predict(ols.aic, newdata = test[,which(colnames(train) %in% c(sel.var.aic, "lpsa"))])
## Prediction error ##
pred.error.AIC <- mean((test$lpsa - pred.aic)^2)


### Cross Validation and prediction performance of BIC based selection ###
ols.bic <- ols(lpsa ~ ., data = train[,which(colnames(train) %in% c(sel.var.bic, "lpsa"))], 
               x=T, y=T, model = T)

## 10 fold cross validation ##    
bic.cross <- calibrate(ols.bic, method = "crossvalidation", B = 10)
## Calibration plot ##
pdf("bic_cross.pdf", height = 8, width = 16)
plot(bic.cross, las = 1, xlab = "Predicted Probability", main = "Cross-Validation calibration with BIC")
dev.off()

## Test Error ##
pred.bic <- predict(ols.bic, newdata = test[,which(colnames(train) %in% c(sel.var.bic, "lpsa"))])
## Prediction error ##
pred.error.BIC <- mean((test$lpsa - pred.bic)^2)

### Cross Validation and prediction performance of lasso based selection ###
ols.lasso <- ols(lpsa ~ ., data = train[,which(colnames(train) %in% c(sel.var.lasso, "lpsa"))], 
                 x=T, y=T, model = T)

## 10 fold cross validation ##    
lasso.cross <- calibrate(ols.lasso, method = "crossvalidation", B = 10)
## Calibration plot ##
pdf("lasso_cross.pdf", height = 8, width = 16)
plot(lasso.cross, las = 1, xlab = "Predicted Probability", main = "Cross-Validation calibration with LASSO")
dev.off()

## Test Error ##
pred.lasso <- predict(ols.lasso, newdata = test[,which(colnames(train) %in% c(sel.var.lasso, "lpsa"))])
## Prediction error ##
pred.error.lasso <- mean((test$lpsa - pred.lasso)^2)

print(c(pred.error.AIC, pred.error.BIC, pred.error.lasso))

