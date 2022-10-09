install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

age <- c(3,4,5,6,7,8,9,11,12,14,15,16,17)
length <- c(1.4,1.5,2.2, 2.4,3.1,3.2,3.2,3.9,4.1,4.7,4.5,5.2,5.0)
plot(age,length)

#manual linear regression equation
x <- sum(age)
y <- sum(length)
xy <- age*length
Exy <- sum(xy)
xsq <- (age)^2
ysq <- (length)^2
Exsq <- sum(xsq)
Eysq <- sum(ysq)

y_int <- ((y*Exsq)-(x*Exy))/(13*(Exsq)-(x)^2)
slope <- ((13*Exy)-(x*y))/((13*(Exsq))-(x)^2)

mod <- lm(length~age)
summary(mod)
#coefficients: intercept 0.8296 and length 0.2647 both manual and function
#note the order of 
abline(lm(length~age), col='red')

one_way <- aov(length~age)
summary(one_way)
#analysis of variance gives large F value and small p-value, indicating a high likelihood that the change in length is dependent on the change in age.

#confidence interval for the slope 0.23 , 0.30
confint(mod, level=0.95)

df <- data.frame(age,length)
ggplot(df,aes(age,length))+geom_point()+
  geom_ribbon(stat="smooth",method="lm",se=TRUE)+
  geom_line(stat="smooth",method="lm")


r <- ((13*Exy)-(x*y))/sqrt(((13*Exsq)-(x^2))*((13*Eysq)-(y^2)))
r #Pearson's
cor(age,length,method = "pearson") #Pearson calc by R

r^2 #coefficient of determination
summary(mod)$r.squared #coefficient of determination calc by R

