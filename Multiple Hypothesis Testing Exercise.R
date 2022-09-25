pval <- function(){
  x <- rnorm(50)
  y <- rnorm(50)
  
  return(t.test(x, y)$p.value)
}

p_vals <- replicate(2500, pval())

correctedBH <- p.adjust(p_vals,"BH")
plot(correctedBH)

correctedBon <- p.adjust(p_vals,"bonferroni")
plot(correctedBon)

#setting samples means at 1 and 2
pval2 <- function(){
  x2 <- rnorm(50,1)
  y2 <- rnorm(50,2)
  
  return(t.test(x2, y2)$p.value)
}

p_vals2 <- replicate(2500, pval2())

correctedBH2 <- p.adjust(p_vals2,"BH")
plot(correctedBH2)

correctedBon2 <- p.adjust(p_vals2,"bonferroni")
plot(correctedBon2)
 #not sure how to display results