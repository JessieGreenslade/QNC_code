size <- 10 #size is the possible quanta
p<-0.2
k <-0:10 #probability of each 0-10 quanta being released

#Exercise 1: 10 quanta available and p=0.2
plot(k,dbinom(k,size,p), type='h', xlab='successes', ylab='dbinom_probability')

#Exercise 2
k1 <- 0:14
p1 <- seq(from = 0, to = 1, by =0.1 )

for (i in p1){
  plot(k1,dbinom(k1,14,p1), type='h', xlab='successes', ylab='dbinom_probability')}
#not sure how to script to plot probability for 8 quanta with decile probability values.

#Exercise3: compute the total likelihood and log likelihood of each measurement.
#total likelihood is the product of likelihoods for each measurement or log of each likelihood and their sum.

p2<-0.1
k2 <-0:14 #probability of each 0-10 quanta being released

plot(k2,dbinom(k2,14,p2), type='h', xlab='successes', ylab='dbinom_probability')

#not sure how to look up the value of the probability at 5 and 8 quanta.
#If could would take the log values and sum

#Exercise 4
n3 <- 100
k3 <- 0:14
#p_hat <-??? Does this replace p in problem or is this the likelihood?
#what is a sample's resolution
#dbinom gives the density, 
#pbinom gives the distribution function, 
#qbinom gives the quantile function and 
#rbinom generates random deviates
#not sure the difference in these values and if change based on whether we know the parameters?

#Exercise5
p4 <-0.3
Size <- 100000
k4 <- 0:14

plot(k4,dbinom(k4,Size,p4), type='h', xlab='successes', ylab='dbinom_probability')
