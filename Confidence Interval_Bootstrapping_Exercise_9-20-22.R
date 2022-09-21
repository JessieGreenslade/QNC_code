#95% Confidence Interval
x <- 10
sd <- 2
n <- c(5, 10,20, 40, 80, 160,1000)
z <- 1.96

#method 1
#SEM = s/sqrt(n)
SEM <- sd/sqrt(n)
data.frame(n,SEM)
CI_low <- x-SEM*1.96
CI_hi <- x + SEM*1.96
data.frame(CI_low, CI_hi)

#Method2 T-Distirbution
df <- n-1
t <- qt(0.05/2, df,lower.tail = FALSE)
#double check if this is correct t-score equation

CI_low2 <- x-SEM*t
CI_hi2 <- x + SEM*t
data.frame(CI_low2, CI_hi2)

#Method 3 Bootstrapped CI
library(boot)
library(ggplot2)
#rnorm(n, mean, sd): to create a sample dataset normally distributed

data <- data.frame(xs = rnorm(n,x,sd))
meandata <- function(data, i){
  d <- data[i, ]
  return(mean(d))   
}
bo <- boot(data[, "xs", drop = FALSE], statistic=meandata, R=5000)
#arbitrarily chose R = 5000??
boot.ci(bo, conf=0.95, type="bca")
#not sure if the is computing different CI for each n?

#Method 4 Bayesian Credible Interval
#install.packages("bayestestR")
library(bayestestR)
Sample <- rnorm(n,x,sd)
ci_eti <- ci(Sample, method = "ETI")
ci_eti
#when generating random distributions, should I set seed to compare the CI across methods?
