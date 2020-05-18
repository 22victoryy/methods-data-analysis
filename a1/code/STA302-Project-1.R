# Step 1: Simulate


## Simulation ##
set.seed(1003998757)
beta0 <- rnorm(1, mean = 0, sd = 1) ## The population 
beta1 <- runif(n = 1, min = 1, max = 3) ## The population beta1 
sig2 <- rchisq(n = 1, df = 25) ## The error variance sigma^2

## Multiple simulation may require loops ## 
nsample <- 5 ## Sample size
n.sim <- 100 ## The number of simulations 
sigX <- 0.2 ## The variances of X

## Simulate the predictor variable ##
X <- rnorm(nsample, mean = 0, sd = sqrt(sigX))

# 2. Fix the sample size nsample = 5 . Execute 100 simulations (i.e., n.sim = 100). 
# For each simulation estimate the regression coefficients. Calculate the mean of the estimates 
# from the different simulations. Comment on your observations.


# 3. Plot the histogram of each of the regression parameters. Explain the pattern of the distribution.


# 4. Obtain the variance of the regression parameters for each simulation. This is the variance obtained 
# from the outputs of the lm function. Calculate their means. How do these means compare to the true 
# variance of the regression parameters? Explain.



# 5.Construct the 95% t and z confidence intervals for β0 and β1 during every simulation. 
# What is the proportion of the intervals for each method containing the true value of the parameters? 
# Is this consistent with the definition of confidence interval? What differences do you observe in the 
# t and z confidence intervals?


# 6. For steps 2-4 the sample size was fixed at 5. Start increasing the sample size (e.g., 10, 25, 50, 100) 
# and run steps 2-4. Explain what happens to the mean, variance and distribution of the estimates as sample
# size increases.


# 7. Choose the largest sample size you have used in step 5. Fix the sample size to that and start changing 
# the error variance (sig2). You can increase and decrease the value of the error variance. 
# For each value of error variance execute steps 2-4. Explain what happens to the mean, variance and 
# distribution of the estimates as the error variance changes.
