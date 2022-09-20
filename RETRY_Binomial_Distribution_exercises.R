#Exercise 1
n1 <- 10
p1 <- 0.2
k1 <- c(0:10)

Prob1 <- dbinom(k1, n1, p1)

data.frame(k1, Prob1)
plot(k1, Prob1, type = "h")

#Exercise 2
n2 <- 14
p2 <- 0.1
k2 <- 8

Prob2 <- dbinom(k2, n2, p2)
Prob2
#answer 1.595917e-05

n2.1 <- 14
p2.1 <- seq(from=0.1, to = 1, by=0.1)
k2.1 <- 8

Prob2.1 <- dbinom(k2.1, n2.1, p2.1)
data.frame(p2.1,Prob2.1)

Prob2.2 <- dbinom(k2.1,n2.1,0.1)
Prob2.2

#answer 
#p2.1      Prob2.1
#2   0.1 1.595917e-05
#3   0.2 2.015279e-03
#4   0.3 2.318001e-02
#5   0.4 9.182116e-02
#6   0.5 1.832886e-01
#7   0.6 2.065976e-01
#8   0.7 1.262023e-01
#9   0.8 3.224447e-02
#10  0.9 1.292693e-03
#11  1.0 0.000000e+00

#Exercise 3

n3 <- 14
p3 <- 0.1
k3 <- 5

Prob3 <- dbinom(k3, n3, p3)
Prob3

Total_prob <- (Prob2.2*Prob3)
Total_prob

Total_log_prob <- log(Prob2.2) + log(Prob3)
Total_log_prob
#Should my total prob and log prob be the same?

#Exercise 3.2
n4 <- 14
p4 <- seq(from =0.1, to =1, by = 0.1)
k4 <- 5

Prob4 <- dbinom(k4, n4, p4)
data.frame(p4,Prob4)

Likelihood <- (Prob2.1*Prob4)
plot(p4,Likelihood, type = 'l')

Log_Likelihood <- log(Prob2.1)+log(Prob4)
plot(p4,Log_Likelihood, type='l')

#Exercise 4

Experiments <- c(0,0,3,10,19,26,16,16,5,5,0,0,0,0,0)
n <- length(Experiments)-1
k <- seq(from = 0, to =14, by = 1)
p <- seq(from =0, to = 1, by= 0.1)

Prob <- dbinom(k,n,p)

likelihood_fcn <- (Prob * Experiments)
log_likelihood_fcn <- log(Prob) + log(Experiments)
 #still lost on exactly the matrix being set up to compute max likelihood in question 4.