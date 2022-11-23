library(pwr)
library(ggplot2)

#Cingulum HC vs TDP----
d_seq <- seq(0, 2, by = 0.1)
pwr_list <- lapply(d_seq, function(d){
  pwr.t2n.test(n1 = 27, n2 = 84, 
               d = d,
               power = NULL,
               sig.level = 0.05, 
               alternative = "two.sided")
})
pwr <- sapply(pwr_list, '[[', 'power')

dfpwr <- data.frame(power = pwr, effect.size = d_seq)

ggplot(dfpwr, aes(effect.size, power)) +
  geom_point(size = 2, colour = "black") +
  geom_line(size = 0.5, colour = "red") +
  scale_y_continuous(labels = scales::percent) +
  xlab("effect size") +
  ylab(expression("test power =" ~ 1 - beta))
#draw a line at 80% power
pwr80 <- approx(x = pwr, y = d_seq, xout = 0.8)
pwr80


lbl80 <- paste("Power = 80%\n")
lbl80 <- paste(lbl80, "Effect size =", round(pwr80$y, 2))

ggplot(dfpwr, aes(effect.size, power)) +
  geom_point(size = 2, colour = "black") +
  geom_line(size = 0.5, colour = "red") +
  geom_hline(yintercept = 0.8, linetype = "dotted") +
  geom_text(x = pwr80$y, y = pwr80$x, 
            label = lbl80,
            hjust = 1, vjust = -1) +
  scale_y_continuous(labels = scales::percent) +
  xlab("effect size") +
  ylab(expression("test power =" ~ 1 - beta))


#Cingulum HC vs Tau----
d_seq <- seq(0, 2, by = 0.1)
pwr_list <- lapply(d_seq, function(d){
  pwr.t2n.test(n1 = 27, n2 = 58, 
               d = d,
               power = NULL,
               sig.level = 0.05, 
               alternative = "two.sided")
})
pwr <- sapply(pwr_list, '[[', 'power')

dfpwr <- data.frame(power = pwr, effect.size = d_seq)


pwr80 <- approx(x = pwr, y = d_seq, xout = 0.8)
pwr80

lbl80 <- paste("Power = 80%\n")
lbl80 <- paste(lbl80, "Effect size =", round(pwr80$y, 2))

ggplot(dfpwr, aes(effect.size, power)) +
  geom_point(size = 2, colour = "black") +
  geom_line(size = 0.5, colour = "red") +
  geom_hline(yintercept = 0.8, linetype = "dotted") +
  geom_text(x = pwr80$y, y = pwr80$x, 
            label = lbl80,
            hjust = 1, vjust = -1) +
  scale_y_continuous(labels = scales::percent) +
  xlab("effect size") +
  ylab(expression("test power =" ~ 1 - beta))

#Cingulum Tau vs TDP----
d_seq <- seq(0, 2, by = 0.1)
pwr_list <- lapply(d_seq, function(d){
  pwr.t2n.test(n1 = 84, n2 = 58, 
               d = d,
               power = NULL,
               sig.level = 0.05, 
               alternative = "two.sided")
})
pwr <- sapply(pwr_list, '[[', 'power')

dfpwr <- data.frame(power = pwr, effect.size = d_seq)


pwr80 <- approx(x = pwr, y = d_seq, xout = 0.8)
pwr80

lbl80 <- paste("Power = 80%\n")
lbl80 <- paste(lbl80, "Effect size =", round(pwr80$y, 2))

ggplot(dfpwr, aes(effect.size, power)) +
  geom_point(size = 2, colour = "black") +
  geom_line(size = 0.5, colour = "red") +
  geom_hline(yintercept = 0.8, linetype = "dotted") +
  geom_text(x = pwr80$y, y = pwr80$x, 
            label = lbl80,
            hjust = 1, vjust = -1) +
  scale_y_continuous(labels = scales::percent) +
  xlab("effect size") +
  ylab(expression("test power =" ~ 1 - beta))

#CC HC vs Tau----
d_seq <- seq(0, 2, by = 0.1)
pwr_list <- lapply(d_seq, function(d){
  pwr.t2n.test(n1 = 19, n2 = 42, 
               d = d,
               power = NULL,
               sig.level = 0.05, 
               alternative = "two.sided")
})
pwr <- sapply(pwr_list, '[[', 'power')

dfpwr <- data.frame(power = pwr, effect.size = d_seq)


pwr80 <- approx(x = pwr, y = d_seq, xout = 0.8)
pwr80

lbl80 <- paste("Power = 80%\n")
lbl80 <- paste(lbl80, "Effect size =", round(pwr80$y, 2))

ggplot(dfpwr, aes(effect.size, power)) +
  geom_point(size = 2, colour = "black") +
  geom_line(size = 0.5, colour = "red") +
  geom_hline(yintercept = 0.8, linetype = "dotted") +
  geom_text(x = pwr80$y, y = pwr80$x, 
            label = lbl80,
            hjust = 1, vjust = -1) +
  scale_y_continuous(labels = scales::percent) +
  xlab("effect size") +
  ylab(expression("test power =" ~ 1 - beta))

#CC HC vs TDP----
d_seq <- seq(0, 2, by = 0.1)
pwr_list <- lapply(d_seq, function(d){
  pwr.t2n.test(n1 = 19, n2 = 56, 
               d = d,
               power = NULL,
               sig.level = 0.05, 
               alternative = "two.sided")
})
pwr <- sapply(pwr_list, '[[', 'power')

dfpwr <- data.frame(power = pwr, effect.size = d_seq)


pwr80 <- approx(x = pwr, y = d_seq, xout = 0.8)
pwr80

lbl80 <- paste("Power = 80%\n")
lbl80 <- paste(lbl80, "Effect size =", round(pwr80$y, 2))

ggplot(dfpwr, aes(effect.size, power)) +
  geom_point(size = 2, colour = "black") +
  geom_line(size = 0.5, colour = "red") +
  geom_hline(yintercept = 0.8, linetype = "dotted") +
  geom_text(x = pwr80$y, y = pwr80$x, 
            label = lbl80,
            hjust = 1, vjust = -1) +
  scale_y_continuous(labels = scales::percent) +
  xlab("effect size") +
  ylab(expression("test power =" ~ 1 - beta))

#CC Tau vs TDP ----
d_seq <- seq(0, 2, by = 0.1)
pwr_list <- lapply(d_seq, function(d){
  pwr.t2n.test(n1 = 42, n2 = 56, 
               d = d,
               power = NULL,
               sig.level = 0.05, 
               alternative = "two.sided")
})
pwr <- sapply(pwr_list, '[[', 'power')

dfpwr <- data.frame(power = pwr, effect.size = d_seq)


pwr80 <- approx(x = pwr, y = d_seq, xout = 0.8)
pwr80

lbl80 <- paste("Power = 80%\n")
lbl80 <- paste(lbl80, "Effect size =", round(pwr80$y, 2))

ggplot(dfpwr, aes(effect.size, power)) +
  geom_point(size = 2, colour = "black") +
  geom_line(size = 0.5, colour = "red") +
  geom_hline(yintercept = 0.8, linetype = "dotted") +
  geom_text(x = pwr80$y, y = pwr80$x, 
            label = lbl80,
            hjust = 1, vjust = -1) +
  scale_y_continuous(labels = scales::percent) +
  xlab("effect size") +
  ylab(expression("test power =" ~ 1 - beta))

