#creating Poisson ditribution of Spike Rate data
success <- 0:20
n<- 107

Spike_data <-rpois(107,lambda = 2)

plot(Spike_data,
     type='h',
     main='Poisson Distribution (lambda = 2)')

#create Gaussian distribution for pupil data

Pupil_data <- rnorm(n)#default mean and sd

hist(Pupil_data)

#correlation coefficients of spike and pupil data
r <-cor(Spike_data, Pupil_data)

install.packages("WebPower")
library(WebPower)

wp.correlation(n=NULL,r, power=0.8)
#small effect size so would need very large n.  Post-hoc analysis would say this is underpowered?