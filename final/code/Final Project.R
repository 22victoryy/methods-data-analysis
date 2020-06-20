rm(list = ls())
setwd("~/Dropbox/UofT Admin and TA/STA 302/Lectures/Final Project")

library(NHANES)
library(tidyverse)
library(glmnet)
small.nhanes <- na.omit(NHANES[NHANES$SurveyYr=="2011_12"
                               & NHANES$Age > 17,c(1,3,4,8:11,13,17,20,21,25,46,50,51,52,61)])
small.nhanes <- as.data.frame(small.nhanes %>%
  group_by(ID) %>% filter(row_number()==1) )
nrow(small.nhanes)

## Checking whether there are any ID that was repeated. If not ##
## then length(unique(small.nhanes$ID)) and nrow(small.nhanes) are same ##
length(unique(small.nhanes$ID))

## Create training and test set ##
set.seed(1002656486)
train <- small.nhanes[sample(seq_len(nrow(small.nhanes)), size = 400),]
nrow(train)
length(which(small.nhanes$ID %in% train$ID))
test <- small.nhanes[!small.nhanes$ID %in% train$ID,]
nrow(test)

## Running the model ##
### First fit a multiple linear regression ##
model.lm <- lm( BPSysAve ~ ., data = train[, -c(1)])
summary(model.lm)

## Perform Prediction ##
pred.y <- predict(model.lm, newdata = test, type = "response")

## Prediction error ##
mean((test$BPSysAve - pred.y)^2)

## Fit a ridge penalty ##
model.ridge <- glmnet(x = model.matrix( ~ ., data = train[,-c(1,12)]), y = train$BPSysAve, 
                      standardize = T, alpha = 0)

## Perform Prediction ##
pred.y.ridge <- predict(model.ridge, newx = model.matrix( ~ ., data = test[,-c(1,12)]), type = "response")

## Prediction error ##
mean((test$BPSysAve - pred.y.ridge)^2)


## Fit a LASSO penalty ##
model.lasso <- glmnet(x = model.matrix( ~ ., data = train[,-c(1,12)]), y = train$BPSysAve
                      , standardize = T, alpha = 1)

## Perform Prediction ##
pred.y.lasso <- predict(model.lasso, newx = model.matrix( ~ ., data = test[,-c(1,12)]), type = "response")
## Prediction error ##
mean((test$BPSysAve - pred.y.lasso)^2)
