library(UsingR)
library(scatterplot3d)
library(xtable)
library(car)
library(Matrix)
library(MPV)

rm(list = ls())
# setwd("~/Dropbox/UofT Admin and TA/STA 302/Lectures/Final Project")


library(NHANES)
library(tidyverse)
library(glmnet)
library(rms)

small.nhanes <- na.omit(NHANES[NHANES$SurveyYr=="2011_12"
                               & NHANES$Age > 17,c(1,3,4,8:11,13,17,20,21,25,46,50,51,52,61)])
small.nhanes <- as.data.frame(small.nhanes %>%
                                group_by(ID) %>% filter(row_number()==1) )
# nrow(small.nhanes)

## Checking whether there are any ID that was repeated. If not ##
## then length(unique(small.nhanes$ID)) and nrow(small.nhanes) are same ##
# length(unique(small.nhanes$ID))

## Create training and test set ##
set.seed(1003998757)
train <- small.nhanes[sample(seq_len(nrow(small.nhanes)), size = 400),]
# nrow(train)
length(which(small.nhanes$ID %in% train$ID))
test <- small.nhanes[!small.nhanes$ID %in% train$ID,]
# nrow(test)


## Running the model ##
### First fit a multiple linear regression ##
model.lm <- lm(BPSysAve ~ ., data = train[, -c(1)])
summary(model.lm)



# resid vs fitted train
resid <- rstudent(model.lm)
fitted <- predict(model.lm)
plot(resid~fitted)
lines(lowess(fitted, resid), col = "blue")

# resid vs fitted train entire
resid <- rstudent(lm(small.nhanes$BPSysAve~., data = small.nhanes[, -c(1)]))
fitted <- predict(lm(small.nhanes$BPSysAve~., data = small.nhanes[, -c(1)]))
plot(resid~fitted)
lines(lowess(fitted, resid), col = "blue")





# resid vs bpsys on training set
resid <- rstudent(lm(train$BPSysAve~., data = train[, -c(1)]))
BPSysAvetrain <- train$BPSysAve
plot(resid~BPSysAvetrain)
lines(lowess(BPSysAvetrain, resid), col = "red")


# resid vs bpsys entire model 
resid <- rstudent(lm(small.nhanes$BPSysAve~., data = small.nhanes[, -c(1)]))
BPSysAveModel <- small.nhanes$BPSysAve
plot(resid~BPSysAveModel)
lines(lowess(BPSysAveModel, resid), col = "green")

# leverage bs
h <- hatvalues(model.lm)
# print
thresh <- (2 * dim(model.matrix(model.lm))[2]) / nrow( train[, -c(1)])
w <- which(h>thresh)

print('Leverages:')
print(w)



### The Influential Observations ####
D <- cooks.distance(model.lm)
# p = 6 since 5 samples + 1
# n - p - 1 -> 4 params
which(D > qf(0.5, 39, 400))

print('Influentials:')
print(which(D > qf(0.5, 39, 400))
)



# ## DFFITS ##
print('Dfits:')
dfits <- dffits(model.lm)
which(abs(dfits) > 2*sqrt(39/nrow(train)))

## DFBETAS ##
print('DFbetas:')
dfb <- dfbetas(model.lm)
which(abs(dfb[,1]) > 2/sqrt(nrow(train)))


vif(model.lm)



## Step wise regression ###

### First fit a multiple linear regression ##
## Based on AIC ##
# model.lm <- lm(lpsa ~ ., data = train[, -c(1)])
summary(model.lm)  
n <- nrow(train)
sel.var.aic <- step(model.lm, trace = 0, k = 2, direction = "both") 
sel.var.aic<-attr(terms(sel.var.aic), "term.labels")   
sel.var.aic

# bic 

## Step wise regression ###

### First fit a multiple linear regression ##
## Based on AIC ##
# model.lm <- lm(lpsa ~ ., data = train[, -c(1)])
summary(model.lm)  
n <- nrow(train)
sel.var.bic <- step(model.lm, trace = 0, k = log(nrow(train)), direction = "both") 
sel.var.bic<-attr(terms(sel.var.bic), "term.labels")   
sel.var.bic



# ## Perform Prediction ##
pred.y <- predict(model.lm, newdata = test, type = "response")

# ## Prediction error ##
mean((test$BPSysAve - pred.y)^2)

## Fit a ridge penalty ##
model.ridge <- glmnet(x = model.matrix( ~ ., data = train[,-c(1,12)]), y = train$BPSysAve, 
                      standardize = T, alpha = 0)

# ## Perform Prediction ##
pred.y.ridge <- predict(model.ridge, newx = model.matrix( ~ ., data = test[,-c(1,12)]), type = "response")

# ## Prediction error ##
mean((test$BPSysAve - pred.y.ridge)^2)


# ## Fit a LASSO penalty ## --> shrinkage method 
model.lasso <- glmnet(x = model.matrix( ~ ., data = train[,-c(1,12)]), y = train$BPSysAve
                      , standardize = T, alpha = 1)

# ## Perform Prediction ##
pred.y.lasso <- predict(model.lasso, newx = model.matrix( ~ ., data = test[,-c(1,12)]), type = "response")
# ## Prediction error ##
mean((test$BPSysAve - pred.y.lasso)^2)


## Elastic net ##

# model.EN <- glmnet(x = data.matrix(train[,2:9]), y = train$BPSysAve, standardize = T, alpha = 0.5)
model.EN <- glmnet(x = model.matrix( ~ ., data = train[,-c(1,12)]), y = train$BPSysAve
                   , standardize = T, alpha = 1)
# ## Perform Prediction ##
pred.y.EN <- predict(model.EN, newx = model.matrix( ~ ., data = test[,-c(1,12)]), type = "response")

mean((test$BPSysAve - pred.y.EN)^2)



### LASSO selection ###

## Perform cross validation to choose lambda ##

# resid <- rstudent(lm(small.nhanes$BPSysAve~., data = small.nhanes[, -c(1)]))

# cv.out <- cv.glmnet(x = data.matrix(train[, -c(1)]), y = train$BPSysAve, standardize = T, alpha = 1)

cv.out <- cv.glmnet(x = model.matrix(~ ., data = train[,-c(1, 12)]), y = train$BPSysAve, standardize = T, alpha = 1)
plot(cv.out)
best.lambda <- cv.out$lambda.1se
best.lambda
co<-coef(cv.out, s = "lambda.1se")


## threfvshold for variable selection ##
# lasso 

thresh <- 0.00
# select variables #
inds<-which(abs(co) > thresh )
variables<-row.names(co)[inds]
sel.var.lasso<-variables[!(variables %in% '(Intercept)')]
sel.var.lasso
### Cross Validation and prediction performance of AIC based selection ###
ols.aic <- ols(BPSysAve ~ ., data = train[,which(colnames(train) %in% c(sel.var.aic, "BPSysAve"))], 
               x=T, y=T, model = T)

## 10 fold cross validation ##    
aic.cross <- calibrate(ols.aic, method = "crossvalidation", B = 10)
## Calibration plot ##
pdf("aic_cross.pdf", height = 8, width = 16)
plot(aic.cross, las = 1, xlab = "Predicted Probability", main = "Cross-Validation calibration with AIC")
dev.off()

## Test Error ##
pred.aic <- predict(ols.aic, newdata = test[,which(colnames(train) %in% c(sel.var.aic, "BPSysAve"))])
## Prediction error ##
pred.error.AIC <- mean((test$BPSysAve - pred.aic)^2)



### Cross Validation and prediction performance of BIC based selection ###
ols.bic <- ols(BPSysAve ~ ., data = train[,which(colnames(train) %in% c(sel.var.bic, "BPSysAve"))], 
               x=T, y=T, model = T)

## 10 fold cross validation ##    
bic.cross <- calibrate(ols.bic, method = "crossvalidation", B = 10)
## Calibration plot ##
pdf("bic_cross.pdf", height = 8, width = 16)
plot(bic.cross, las = 1, xlab = "Predicted Probability", main = "Cross-Validation calibration with BIC")
dev.off()

## Test Error ##
pred.bic <- predict(ols.bic, newdata = test[,which(colnames(train) %in% c(sel.var.bic, "BPSysAve"))])
## Prediction error ##
pred.error.BIC <- mean((test$BPSysAve - pred.bic)^2)




# ### Cross Validation and prediction performance of lasso based selection ###
ols.lasso <- ols(BPSysAve ~ ., data = train[,which(colnames(train) %in% c(sel.var.lasso, "BPSysAve"))], 
                 x=T, y=T, model = T)


## 10 fold cross validation ##    
lasso.cross <- calibrate(ols.lasso, method = "crossvalidation", B = 10)
## Calibration plot ##
pdf("lasso_cross.pdf", height = 8, width = 16)
plot(lasso.cross, las = 1, xlab = "Predicted Probability", main = "Cross-Validation calibration with LASSO")
dev.off()

## Test Error ##
pred.lasso <- predict(ols.lasso, newdata = test[,which(colnames(train) %in% c(sel.var.lasso, "BPSysAve"))])
## Prediction error ##
pred.error.lasso <- mean((test$BPSysAve - pred.lasso)^2)

print(c(pred.error.AIC, pred.error.BIC, pred.error.lasso))
dev.off()




