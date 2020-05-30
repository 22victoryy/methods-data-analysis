## Simulation ##
set.seed(1003998757)
beta0 <- rnorm(1, mean = 0, sd = 1) ## The population 
beta1 <- runif(n = 1, min = 1, max = 3) ## The population beta1 
sig2 <- rchisq(n = 1, df = 25) ## The error variance sigma^2

## Multiple simulation may require loops ## 
nsample = 5## Sample size
n.sim <- 100 ## The number of simulations 
sigX <- 0.2 ## The variances of X

## Simulate the predictor variable ##
X <- rnorm(nsample, mean = 0, sd = sqrt(sigX))



# nsample is 5.
# fix n.sim to 100

b0 <- vector() ## saves the sample estimates of beta0
b1 <- vector() ## saves the sample estimates of beta1
sigy <- vector()  ## Saves the estimate of error variance





varb1 <- vector()
varb0 <- vector()

conf_z_b1 <- vector()


count_b0z = 0
count_b1z = 0
count_b0t = 0
count_b1t = 0


for(i in 1:n.sim){
  Y <- beta0 + beta1*X + rnorm(n = nsample, mean = 0, sd = sqrt(sig2))
  model <- lm(Y ~ X)
  
  alpha <- 0.05
  SXX <- (var(X) * (nsample -1))
  
  b0[i] <- coef(model)[1]
  b1[i] <- coef(model)[2]
  
  varb1[i] <- sig2/(var(X) * (nsample -1))
  #     (var(X) * 4) SXX
  varb0[i] <- sig2*((1/nsample) + (mean(X)^2/(var(X) * (nsample-1))))
  
  sigy[i] <- summary(model)$sigma^2
  
  
  b0_l_z = b0[i] - qnorm(1 - (alpha)/2) * (sqrt(sig2) * sqrt((1/nsample) + mean(X)^2/SXX))
  b0_r_z = b0[i] + qnorm(1 - (alpha)/2) * (sqrt(sig2) * sqrt((1/nsample) + mean(X)^2/SXX))
  
  b1_l_z = b1[i] - qnorm(1 - (alpha)/2) * (sqrt(sig2)/sqrt(SXX))
  b1_r_z = b1[i] + qnorm(1 - (alpha)/2) * (sqrt(sig2)/sqrt(SXX))
  #     print(c(b1_ll_z, b1_ul_z))
  
  b0_l_t = b0[i] - qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])*sqrt((1/nsample) + (mean(X)^2/SXX)))
  b0_r_t = b0[i] + qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])*sqrt((1/nsample) + (mean(X)^2/SXX)))
  
  b1_l_t = b1[i] - qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])/(sqrt(SXX)))
  b1_r_t = b1[i] + qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])/(sqrt(SXX)))
  
  
  if(beta0 <= b0_r_z & beta0 >= b0_l_z){  # 95 b0z
    count_b0z = count_b0z + 1
    
  }
  
  if(beta1 <=  b1_r_z & beta1 >= b1_l_z){  # 97 b1z
    count_b1z = count_b1z + 1
    
  }
  
  if(beta0 <=  b0_r_t & beta0 >= b0_l_t){  # 94 b0t
    count_b0t = count_b0t + 1
    
  }
  
  
  if(beta1 <=  b1_r_t & beta1 >= b1_l_t){  # 96 b1t
    count_b1t = count_b1t + 1
    
  }  
  
  
}

print("Proportions:")

print("B0z:")
print(count_b0z)

print("B1z:")
print(count_b1z)

print("B0t:")
print(count_b0t)

print("B1t:")
print(count_b1t)


print('---------------------------------------------------------------------------')


m_b0 <- mean(b0)
a_0 = 'mean of b0:'
print(paste(a_0, m_b0))




m_b1 <- mean(b1)
a_1 = 'mean of b1:'
print(paste(a_1, m_b1))




m_sigy <- mean(sigy)
a_s = 'mean of sigy:'
print(paste(a_s, m_sigy))



print('actual beta0:')
print(beta0)

print('actual beta1:')
print(beta1)

print('actual sig:')
print(sig2)


hist(b0)
hist(b1)
hist(sigy)


######################### 10 samples ##########################

## Simulation ##
set.seed(1003998757)
beta0 <- rnorm(1, mean = 0, sd = 1) ## The population 
beta1 <- runif(n = 1, min = 1, max = 3) ## The population beta1 
sig2 <- rchisq(n = 1, df = 25) ## The error variance sigma^2

## Multiple simulation may require loops ## 
nsample = 10 ## Sample size 10 
n.sim <- 100 ## The number of simulations 
sigX <- 0.2 ## The variances of X

## Simulate the predictor variable ##
X <- rnorm(nsample, mean = 0, sd = sqrt(sigX))


b0 <- vector() ## saves the sample estimates of beta0
b1 <- vector() ## saves the sample estimates of beta1
sigy <- vector()  ## Saves the estimate of error variance


varb1 <- vector()
varb0 <- vector()

conf_z_b1 <- vector()


count_b0z = 0
count_b1z = 0
count_b0t = 0
count_b1t = 0


for(i in 1:n.sim){
  Y <- beta0 + beta1*X + rnorm(n = nsample, mean = 0, sd = sqrt(sig2))
  model <- lm(Y ~ X)
  
  alpha <- 0.05
  SXX <- (var(X) * (nsample -1))
  
  b0[i] <- coef(model)[1]
  b1[i] <- coef(model)[2]
  
  varb1[i] <- sig2/(var(X) * (nsample -1))
  #     (var(X) * 4) SXX
  varb0[i] <- sig2*((1/nsample) + (mean(X)^2/(var(X) * (nsample-1))))
  
  sigy[i] <- summary(model)$sigma^2
  
  
  b0_l_z = b0[i] - qnorm(1 - (alpha)/2) * (sqrt(sig2) * sqrt((1/nsample) + mean(X)^2/SXX))
  b0_r_z = b0[i] + qnorm(1 - (alpha)/2) * (sqrt(sig2) * sqrt((1/nsample) + mean(X)^2/SXX))
  
  b1_l_z = b1[i] - qnorm(1 - (alpha)/2) * (sqrt(sig2)/sqrt(SXX))
  b1_r_z = b1[i] + qnorm(1 - (alpha)/2) * (sqrt(sig2)/sqrt(SXX))
  #     print(c(b1_ll_z, b1_ul_z))
  
  b0_l_t = b0[i] - qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])*sqrt((1/nsample) + (mean(X)^2/SXX)))
  b0_r_t = b0[i] + qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])*sqrt((1/nsample) + (mean(X)^2/SXX)))
  
  b1_l_t = b1[i] - qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])/(sqrt(SXX)))
  b1_r_t = b1[i] + qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])/(sqrt(SXX)))
  
  
  if(beta0 <= b0_r_z & beta0 >= b0_l_z){  # 95 b0z
    count_b0z = count_b0z + 1
    
  }
  
  if(beta1 <=  b1_r_z & beta1 >= b1_l_z){  # 97 b1z
    count_b1z = count_b1z + 1
    
  }
  
  if(beta0 <=  b0_r_t & beta0 >= b0_l_t){  # 94 b0t
    count_b0t = count_b0t + 1
    
  }
  
  
  if(beta1 <=  b1_r_t & beta1 >= b1_l_t){  # 96 b1t
    count_b1t = count_b1t + 1
    
  }  
  
  
}

print("Proportions:")

print("B0z:")
print(count_b0z)

print("B1z:")
print(count_b1z)

print("B0t:")
print(count_b0t)

print("B1t:")
print(count_b1t)


print('---------------------------------------------------------------------------')


m_b0 <- mean(b0)
a_0 = 'mean of b0:'
print(paste(a_0, m_b0))




m_b1 <- mean(b1)
a_1 = 'mean of b1:'
print(paste(a_1, m_b1))




m_sigy <- mean(sigy)
a_s = 'mean of sigy:'
print(paste(a_s, m_sigy))



print('actual beta0:')
print(beta0)

print('actual beta1:')
print(beta1)

print('actual sig:')
print(sig2)


hist(b0)
hist(b1)
hist(sigy)

################### 25 samples #################
## Simulation ##
set.seed(1003998757)
beta0 <- rnorm(1, mean = 0, sd = 1) ## The population 
beta1 <- runif(n = 1, min = 1, max = 3) ## The population beta1 
sig2 <- rchisq(n = 1, df = 25) ## The error variance sigma^2

## Multiple simulation may require loops ## 
nsample = 25 ## Sample size 25
n.sim <- 100 ## The number of simulations 
sigX <- 0.2 ## The variances of X

## Simulate the predictor variable ##
X <- rnorm(nsample, mean = 0, sd = sqrt(sigX))



# nsample is 5.
# fix n.sim to 100

b0 <- vector() ## saves the sample estimates of beta0
b1 <- vector() ## saves the sample estimates of beta1
sigy <- vector()  ## Saves the estimate of error variance





varb1 <- vector()
varb0 <- vector()

conf_z_b1 <- vector()


count_b0z = 0
count_b1z = 0
count_b0t = 0
count_b1t = 0


for(i in 1:n.sim){
  Y <- beta0 + beta1*X + rnorm(n = nsample, mean = 0, sd = sqrt(sig2))
  model <- lm(Y ~ X)
  
  alpha <- 0.05
  SXX <- (var(X) * (nsample -1))
  
  b0[i] <- coef(model)[1]
  b1[i] <- coef(model)[2]
  
  varb1[i] <- sig2/(var(X) * (nsample -1))
  #     (var(X) * 4) SXX
  varb0[i] <- sig2*((1/nsample) + (mean(X)^2/(var(X) * (nsample-1))))
  
  sigy[i] <- summary(model)$sigma^2
  
  
  b0_l_z = b0[i] - qnorm(1 - (alpha)/2) * (sqrt(sig2) * sqrt((1/nsample) + mean(X)^2/SXX))
  b0_r_z = b0[i] + qnorm(1 - (alpha)/2) * (sqrt(sig2) * sqrt((1/nsample) + mean(X)^2/SXX))
  
  b1_l_z = b1[i] - qnorm(1 - (alpha)/2) * (sqrt(sig2)/sqrt(SXX))
  b1_r_z = b1[i] + qnorm(1 - (alpha)/2) * (sqrt(sig2)/sqrt(SXX))
  #     print(c(b1_ll_z, b1_ul_z))
  
  b0_l_t = b0[i] - qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])*sqrt((1/nsample) + (mean(X)^2/SXX)))
  b0_r_t = b0[i] + qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])*sqrt((1/nsample) + (mean(X)^2/SXX)))
  
  b1_l_t = b1[i] - qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])/(sqrt(SXX)))
  b1_r_t = b1[i] + qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])/(sqrt(SXX)))
  
  
  if(beta0 <= b0_r_z & beta0 >= b0_l_z){  # 95 b0z
    count_b0z = count_b0z + 1
    
  }
  
  if(beta1 <=  b1_r_z & beta1 >= b1_l_z){  # 97 b1z
    count_b1z = count_b1z + 1
    
  }
  
  if(beta0 <=  b0_r_t & beta0 >= b0_l_t){  # 94 b0t
    count_b0t = count_b0t + 1
    
  }
  
  
  if(beta1 <=  b1_r_t & beta1 >= b1_l_t){  # 96 b1t
    count_b1t = count_b1t + 1
    
  }  
  
  
}

print("Proportions:")

print("B0z:")
print(count_b0z)

print("B1z:")
print(count_b1z)

print("B0t:")
print(count_b0t)

print("B1t:")
print(count_b1t)


print('---------------------------------------------------------------------------')


m_b0 <- mean(b0)
a_0 = 'mean of b0:'
print(paste(a_0, m_b0))




m_b1 <- mean(b1)
a_1 = 'mean of b1:'
print(paste(a_1, m_b1))




m_sigy <- mean(sigy)
a_s = 'mean of sigy:'
print(paste(a_s, m_sigy))



print('actual beta0:')
print(beta0)

print('actual beta1:')
print(beta1)

print('actual sig:')
print(sig2)


hist(b0)
hist(b1)
hist(sigy)

######################## 150 samples ######################
## Simulation ##
set.seed(1003998757)
beta0 <- rnorm(1, mean = 0, sd = 1) ## The population 
beta1 <- runif(n = 1, min = 1, max = 3) ## The population beta1 
sig2 <- rchisq(n = 1, df = 25) ## The error variance sigma^2

## Multiple simulation may require loops ## 
nsample = 150 ## Sample size
n.sim <- 100 ## The number of simulations 
sigX <- 0.2 ## The variances of X

## Simulate the predictor variable ##
X <- rnorm(nsample, mean = 0, sd = sqrt(sigX))



# nsample is 5.
# fix n.sim to 100

b0 <- vector() ## saves the sample estimates of beta0
b1 <- vector() ## saves the sample estimates of beta1
sigy <- vector()  ## Saves the estimate of error variance





varb1 <- vector()
varb0 <- vector()

conf_z_b1 <- vector()


count_b0z = 0
count_b1z = 0
count_b0t = 0
count_b1t = 0


for(i in 1:n.sim){
  Y <- beta0 + beta1*X + rnorm(n = nsample, mean = 0, sd = sqrt(sig2))
  model <- lm(Y ~ X)
  
  alpha <- 0.05
  SXX <- (var(X) * (nsample -1))
  
  b0[i] <- coef(model)[1]
  b1[i] <- coef(model)[2]
  
  varb1[i] <- sig2/(var(X) * (nsample -1))
  #     (var(X) * 4) SXX
  varb0[i] <- sig2*((1/nsample) + (mean(X)^2/(var(X) * (nsample-1))))
  
  sigy[i] <- summary(model)$sigma^2
  
  
  b0_l_z = b0[i] - qnorm(1 - (alpha)/2) * (sqrt(sig2) * sqrt((1/nsample) + mean(X)^2/SXX))
  b0_r_z = b0[i] + qnorm(1 - (alpha)/2) * (sqrt(sig2) * sqrt((1/nsample) + mean(X)^2/SXX))
  
  b1_l_z = b1[i] - qnorm(1 - (alpha)/2) * (sqrt(sig2)/sqrt(SXX))
  b1_r_z = b1[i] + qnorm(1 - (alpha)/2) * (sqrt(sig2)/sqrt(SXX))
  #     print(c(b1_ll_z, b1_ul_z))
  
  b0_l_t = b0[i] - qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])*sqrt((1/nsample) + (mean(X)^2/SXX)))
  b0_r_t = b0[i] + qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])*sqrt((1/nsample) + (mean(X)^2/SXX)))
  
  b1_l_t = b1[i] - qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])/(sqrt(SXX)))
  b1_r_t = b1[i] + qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])/(sqrt(SXX)))
  
  
  if(beta0 <= b0_r_z & beta0 >= b0_l_z){  # 95 b0z
    count_b0z = count_b0z + 1
    
  }
  
  if(beta1 <=  b1_r_z & beta1 >= b1_l_z){  # 97 b1z
    count_b1z = count_b1z + 1
    
  }
  
  if(beta0 <=  b0_r_t & beta0 >= b0_l_t){  # 94 b0t
    count_b0t = count_b0t + 1
    
  }
  
  
  if(beta1 <=  b1_r_t & beta1 >= b1_l_t){  # 96 b1t
    count_b1t = count_b1t + 1
    
  }  
  
  
}

print("Proportions:")

print("B0z:")
print(count_b0z)

print("B1z:")
print(count_b1z)

print("B0t:")
print(count_b0t)

print("B1t:")
print(count_b1t)


print('---------------------------------------------------------------------------')


m_b0 <- mean(b0)
a_0 = 'mean of b0:'
print(paste(a_0, m_b0))




m_b1 <- mean(b1)
a_1 = 'mean of b1:'
print(paste(a_1, m_b1))




m_sigy <- mean(sigy)
a_s = 'mean of sigy:'
print(paste(a_s, m_sigy))



print('actual beta0:')
print(beta0)

print('actual beta1:')
print(beta1)

print('actual sig:')
print(sig2)


hist(b0)
hist(b1)
hist(sigy)


###################### 1000 samples ###################
## Simulation ##
set.seed(1003998757)
beta0 <- rnorm(1, mean = 0, sd = 1) ## The population 
beta1 <- runif(n = 1, min = 1, max = 3) ## The population beta1 
sig2 <- rchisq(n = 1, df = 25) ## The error variance sigma^2

## Multiple simulation may require loops ## 
nsample = 1000 ## Sample size
n.sim <- 100 ## The number of simulations 
sigX <- 0.2 ## The variances of X

## Simulate the predictor variable ##
X <- rnorm(nsample, mean = 0, sd = sqrt(sigX))



# nsample is 5.
# fix n.sim to 100

b0 <- vector() ## saves the sample estimates of beta0
b1 <- vector() ## saves the sample estimates of beta1
sigy <- vector()  ## Saves the estimate of error variance





varb1 <- vector()
varb0 <- vector()

conf_z_b1 <- vector()


count_b0z = 0
count_b1z = 0
count_b0t = 0
count_b1t = 0


for(i in 1:n.sim){
  Y <- beta0 + beta1*X + rnorm(n = nsample, mean = 0, sd = sqrt(sig2))
  model <- lm(Y ~ X)
  
  alpha <- 0.05
  SXX <- (var(X) * (nsample -1))
  
  b0[i] <- coef(model)[1]
  b1[i] <- coef(model)[2]
  
  varb1[i] <- sig2/(var(X) * (nsample -1))
  #     (var(X) * 4) SXX
  varb0[i] <- sig2*((1/nsample) + (mean(X)^2/(var(X) * (nsample-1))))
  
  sigy[i] <- summary(model)$sigma^2
  
  
  b0_l_z = b0[i] - qnorm(1 - (alpha)/2) * (sqrt(sig2) * sqrt((1/nsample) + mean(X)^2/SXX))
  b0_r_z = b0[i] + qnorm(1 - (alpha)/2) * (sqrt(sig2) * sqrt((1/nsample) + mean(X)^2/SXX))
  
  b1_l_z = b1[i] - qnorm(1 - (alpha)/2) * (sqrt(sig2)/sqrt(SXX))
  b1_r_z = b1[i] + qnorm(1 - (alpha)/2) * (sqrt(sig2)/sqrt(SXX))
  #     print(c(b1_ll_z, b1_ul_z))
  
  b0_l_t = b0[i] - qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])*sqrt((1/nsample) + (mean(X)^2/SXX)))
  b0_r_t = b0[i] + qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])*sqrt((1/nsample) + (mean(X)^2/SXX)))
  
  b1_l_t = b1[i] - qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])/(sqrt(SXX)))
  b1_r_t = b1[i] + qt(1 - (alpha/2), df = (nsample - 2))*(sqrt(sigy[i])/(sqrt(SXX)))
  
  
  if(beta0 <= b0_r_z & beta0 >= b0_l_z){  # 95 b0z
    count_b0z = count_b0z + 1
    
  }
  
  if(beta1 <=  b1_r_z & beta1 >= b1_l_z){  # 97 b1z
    count_b1z = count_b1z + 1
    
  }
  
  if(beta0 <=  b0_r_t & beta0 >= b0_l_t){  # 94 b0t
    count_b0t = count_b0t + 1
    
  }
  
  
  if(beta1 <=  b1_r_t & beta1 >= b1_l_t){  # 96 b1t
    count_b1t = count_b1t + 1
    
  }  
  
  
}

print("Proportions:")

print("B0z:")
print(count_b0z)

print("B1z:")
print(count_b1z)

print("B0t:")
print(count_b0t)

print("B1t:")
print(count_b1t)


print('---------------------------------------------------------------------------')


m_b0 <- mean(b0)
a_0 = 'mean of b0:'
print(paste(a_0, m_b0))




m_b1 <- mean(b1)
a_1 = 'mean of b1:'
print(paste(a_1, m_b1))




m_sigy <- mean(sigy)
a_s = 'mean of sigy:'
print(paste(a_s, m_sigy))



print('actual beta0:')
print(beta0)

print('actual beta1:')
print(beta1)

print('actual sig:')
print(sig2)


hist(b0)
hist(b1)
hist(sigy)


###################### sig2 decreased df = 20, sample size 1000 ######################
## Simulation ##
set.seed(1003998757)
beta0 <- rnorm(1, mean = 0, sd = 1) ## The population 
beta1 <- runif(n = 1, min = 1, max = 3) ## The population beta1 
sig2 <- rchisq(n = 1, df = 20) ## decrease gi2, df = 20

## Multiple simulation may require loops ## 
nsample = 1000 ## Sample size 1000
n.sim <- 100 ## The number of simulations 
sigX <- 0.2 ## The variances of X

## Simulate the predictor variable ##
X <- rnorm(nsample, mean = 0, sd = sqrt(sigX))



# nsample is 5.
# fix n.sim to 100

b0 <- vector() ## saves the sample estimates of beta0
b1 <- vector() ## saves the sample estimates of beta1
sigy <- vector()  ## Saves the estimate of error variance





varb1 <- vector()
varb0 <- vector()


for(i in 1:n.sim){
  Y <- beta0 + beta1*X + rnorm(n = nsample, mean = 0, sd = sqrt(sig2))
  model <- lm(Y ~ X)
  
  alpha <- 0.05
  SXX <- (var(X) * (nsample -1))
  
  b0[i] <- coef(model)[1]
  b1[i] <- coef(model)[2]
  
  varb1[i] <- sig2/(var(X) * (nsample -1))
  #     (var(X) * 4) SXX
  varb0[i] <- sig2*((1/nsample) + (mean(X)^2/(var(X) * (nsample-1))))
  
  sigy[i] <- summary(model)$sigma^2
}



print('---------------------------------------------------------------------------')


m_b0 <- mean(b0)
a_0 = 'mean of b0:'
print(paste(a_0, m_b0))




m_b1 <- mean(b1)
a_1 = 'mean of b1:'
print(paste(a_1, m_b1))




m_sigy <- mean(sigy)
a_s = 'mean of sigy:'
print(paste(a_s, m_sigy))



print('actual beta0:')
print(beta0)

print('actual beta1:')
print(beta1)

print('actual sig:')
print(sig2)


hist(b0)
hist(b1)
hist(sigy)



###################### sig2 increased df = 1000, sample size 1000 ######################
## Simulation ##
set.seed(1003998757)
beta0 <- rnorm(1, mean = 0, sd = 1) ## The population 
beta1 <- runif(n = 1, min = 1, max = 3) ## The population beta1 
sig2 <- rchisq(n = 1, df = 1000) ## increase sig2, df= 1000

## Multiple simulation may require loops ## 
nsample = 1000 ## Sample size 1000
n.sim <- 100 ## The number of simulations 
sigX <- 0.2 ## The variances of X

## Simulate the predictor variable ##
X <- rnorm(nsample, mean = 0, sd = sqrt(sigX))



# nsample is 5.
# fix n.sim to 100

b0 <- vector() ## saves the sample estimates of beta0
b1 <- vector() ## saves the sample estimates of beta1
sigy <- vector()  ## Saves the estimate of error variance





varb1 <- vector()
varb0 <- vector()



for(i in 1:n.sim){
  Y <- beta0 + beta1*X + rnorm(n = nsample, mean = 0, sd = sqrt(sig2))
  model <- lm(Y ~ X)
  
  alpha <- 0.05
  SXX <- (var(X) * (nsample -1))
  
  b0[i] <- coef(model)[1]
  b1[i] <- coef(model)[2]
  
  varb1[i] <- sig2/(var(X) * (nsample -1))
  #     (var(X) * 4) SXX
  varb0[i] <- sig2*((1/nsample) + (mean(X)^2/(var(X) * (nsample-1))))
  
  sigy[i] <- summary(model)$sigma^2
  
}

print('---------------------------------------------------------------------------')


m_b0 <- mean(b0)
a_0 = 'mean of b0:'
print(paste(a_0, m_b0))




m_b1 <- mean(b1)
a_1 = 'mean of b1:'
print(paste(a_1, m_b1))




m_sigy <- mean(sigy)
a_s = 'mean of sigy:'
print(paste(a_s, m_sigy))



print('actual beta0:')
print(beta0)

print('actual beta1:')
print(beta1)

print('actual sig:')
print(sig2)


hist(b0)
hist(b1)
hist(sigy)