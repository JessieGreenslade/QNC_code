WL <- c(10.4,10.8,11.1, 10.2, 10.3,10.2,10.7, 10.5,10.8, 11.2,10.6,11.4)
TL <- c(7.4,7.6,7.9,7.2,7.4,7.1,7.4,7.2,7.8,7.7,7.8,8.3)

plot(WL,TL)
#wing length and tail length look positively correlated

#Calculate first using the equations above and then 
#using either the Python numpy funciton corrcoef or Matlab's built-in corrcoef. Did you get the same answers?

WL_ranked <- rank(WL)
TL_ranked <- rank(TL)
di <- sum((WL_ranked-TL_ranked))#summed the differences of ranked WL and TL should be zero

d <- WL_ranked-TL_ranked
d2 <- (d)^2
sum_d2 <- sum(d2)

n <- 12
r <- 1-((6*sum_d2)/(n*((n^2)-1)))
r

#not sure difference of rx,y vs rY,X?

cor.test(WL,TL)
#answers are not exactly the same, but similar from manual to R's cor.test??? 

#What is the standard error of? The 95% confidence intervals computed from the standard error?
sr <- sqrt((1-(r^2))/(n-2))
sr

#z-transformation
z <- 0.5*log((1+r)/(1-r))
z

#st.dev of z
sz <- sqrt(1/(n-3))
sz

#CI of z space
scale <-qnorm(0.025)*sz
zCI_95 <- c(z+scale,z-scale)
zCI_95

#hypothesis Testing
t <- r/sr
pval <- pt(t,(n-2),lower.tail = T)
pval

prob <- 2*(1-pval)
prob

#Yale found r=0.75
z_yale <-0.5*log((1+0.75)/(1-0.75)) 
lambda <- (z_yale-z)/sz
prob2 = 2*(1-pnorm(lambda))
prob2
#different than homework answers???

#power analysis
#create z_ref
zref <- 0.5*log((1+0.5)/(1-0.5))
plambda = (z-zref)/sqrt((1/(n-3)))

alpha = 0.05
z_criterion = qnorm(1-alpha/2)

power = 1-pnorm(z_criterion-plambda)

#review how to find power and the desired n
desired_power = 0.99
predicted_n = ceiling(1/((z-zref) / (z_criterion - qnorm(1-desired_power)))**2+3)
#predicted n is 39, is difference due to variability in the qnorm function????