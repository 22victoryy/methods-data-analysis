rm(list = ls())

setwd("~/Dropbox/UofT Admin and TA/STA 302/Lectures/Lecture 11")
## Install UsingR package ##
#  install.packages("UsingR")
## Scatter plot 3d ##
#install.packages("scatterplot3d")

library(UsingR)
library(scatterplot3d)
library(xtable)
library(car)
library(Matrix)
library(MPV)

### Polynomial Regression ###
prof <- read.table("https://gattonweb.uky.edu/sheather/book/docs/datasets/profsalary.txt", header = T)
head(prof)
str(prof)

## Scatter Plot ##
pdf("prof_scatter.pdf", height = 8, width = 16)
par(family = 'serif')
plot(prof$Salary ~ prof$Experience, type = "p", xlab = "Experience", 
     ylab = "Salary", cex.lab = 1.2,
     col = "red")
abline(lm(Salary ~ Experience, data = prof), col = "blue")
dev.off()

## A simple linear Regression ##
model1 <- lm(Salary ~ Experience, data = prof)
summary(model1)
resid <- rstudent(model1)


pdf("resid11.pdf", height = 8, width = 16)
par(family = 'serif', mfrow = c(1,2))
qqnorm(resid)
qqline(resid)
plot(resid ~ prof$Experience, type = "p", xlab = "Experience", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(prof$Experience, resid), col = "blue")
dev.off()


## Polynomial Regression ##
model2 <- lm(Salary ~ Experience + I(Experience^2), data = prof)
summary(model2)
resid2 <- rstudent(model2)

pdf("resid12.pdf", height = 8, width = 16)
par(family = 'serif', mfrow = c(1,2))
qqnorm(resid2)
qqline(resid2)
plot(resid2 ~ prof$Experience, type = "p", xlab = "Experience", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
dev.off()

### The defect dataset for multicolinearity ###

def <- read.table("https://gattonweb.uky.edu/sheather/book/docs/datasets/defects.txt", header = T)
head(def)
str(def)

## Square Root Transformation ##
model.new <- lm(I(sqrt(Defective)) ~ Temperature + Density + Rate, data = def)
summary(model.new)

## Model selection criteria ##
criteria <- function(model){
    n <- length(model$residuals)
    p <- length(model$coefficients) - 1
    RSS <- sum(model$residuals^2)
    R2 <- summary(model)$r.squared
    R2.adj <- summary(model)$adj.r.squared
    AIC <- n*log(RSS/n) + 2*p
    AICc <- AIC + (2*(p+2)*(p+3))/(n-p-1)
    BIC <- n*log(RSS/n) + (p+2)*log(n)
    res <- c(R2, R2.adj, AIC, AICc, BIC)
    names(res) <- c("R Squared", "Adjsuted R Squared", "AIC", "AICc", "BIC")
    return(res)
}

## The crteria ##
## model 1 ##
model.1 <- lm(I(sqrt(Defective)) ~ Temperature + Density + Rate, data = def)
crit1 <- criteria(model = model.1)

## model 2 ##
model.2 <- lm(I(sqrt(Defective)) ~ Temperature , data = def)
crit2 <- criteria(model = model.2)

## model 3 ##
model.3 <- lm(I(sqrt(Defective)) ~ Rate , data = def)
crit3 <- criteria(model = model.3)

## model 4 ##
model.4 <- lm(I(sqrt(Defective)) ~ Density , data = def)
crit4 <- criteria(model = model.4)

## model 5 ##
model.5 <- lm(I(sqrt(Defective)) ~ Temperature + Density , data = def)
crit5 <- criteria(model = model.5)

## model 6 ##
model.6 <- lm(I(sqrt(Defective)) ~ Temperature + Rate , data = def)
crit6 <- criteria(model = model.6)

## model 7 ##
model.7 <- lm(I(sqrt(Defective)) ~ Density + Rate , data = def)
crit7 <- criteria(model = model.7)

## model 8 ##
model.8 <- lm(I(sqrt(Defective)) ~ 1 , data = def)
crit8 <- criteria(model = model.8)

## Comapre the criteria for each model ##
rbind(crit1, crit2, crit3, crit4, crit5, crit6, crit7, crit8)


### Prostate Data ###
train <- read.table("https://gattonweb.uky.edu/sheather/book/docs/datasets/prostateTraining.txt", header = T)
head(train)
str(train)

test <- read.table("https://gattonweb.uky.edu/sheather/book/docs/datasets/prostateTest.txt", header = T)
####### Variable selection #######

## Step wise regression ###

### First fit a multiple linear regression ##
model.lm <- lm(lpsa ~ ., data = train[, -c(1)])
summary(model.lm)  
n <- nrow(train)
sel.var.aic <- step(model.lm, trace = 0, k = log(n), direction = "both") 
select_var<-attr(terms(sel.var.aic), "term.labels")   
select_var

