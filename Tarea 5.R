library(tidyverse)
library(dplyr)
library(tidyquant)
library(ggplot2)
library(ggthemes)

#Pregunta 2 

#Precios Microsoft
stockprices <- c("MSFT")
MSFT <- tq_get(stockprices,
               get = "stock.prices",
               from = "2000-01-01",
               to = "2018-08-31",
               periodicity = "monthly")

#Precios Apple
stockprices2 <- c("AAPL")
AAPL <- tq_get(stockprices2,
               get = "stock.prices",
               from = "2000-01-01",
               to = "2018-08-31",
               periodicity = "monthly")
#Pregunta 2A


MS<- MSFT$close
AP<- AAPL$close

fx_ret <- function(x) {
  a <- log(x)
  b <- diff(a)
  return(b)
}

retornosMS <- fx_ret(MS)
retornosMS <- as.data.frame(retornosMS)  
retornosAP <- fx_ret(AP)
retornosAP <- as.data.frame(retornosAP) 

#Letra B

dia<- MSFT %>% select("date")
colnames(dia) <- c("retornosMS")

b <- merge(retornosMS,dia, no.dups = FALSE)

ggplot(retornosMS, aes(x = date, y = close)) + geom_area()

#Pregunta 3A

set.seed(123)

reps = 10000

betas = matrix(NA, nrow = reps, ncol = 8)

beta0 = 2

beta1 = 2.5

beta2 = 1

su = 1

n = c(50, 100, 500, 1000)  #tamaño muestral

for (j in 1:length(n)) {
  
  X1=rnorm(n[j],20,1)
  
  e=rnorm(n[j],0,1)
  
  X2=0.8*X1+e
  
  for (i in 1:reps) {
    
    u= rnorm(n[j],0,su)
    
    v = beta2*X2 + u
    
    Y_omit = beta0 + beta1*X1 + v
    Y_pob = beta0 + beta1*X1 + beta2*X2 + u
    
    reg_omit = lm(Y_omit~X1)  
    
    betas[i,j] = reg_omit$coef[2]
    
    reg_pob = lm(Y_pob~X1+X2)
    
    betas[i,j+4] = reg_pob$coef[2]
    
    
  }
  
}

betas_df <- data.frame(betas)

#Betas 1 Con sesgo(1-4) y Sin sesgo (5-8)
apply(betas_df, 2, mean)

#Varianza Con sesgo(1-4) y Sin sesgo (5-8)
apply(betas_df,2,var)


#Letra B
#Graficos 


library(ggplot2)

library(gridExtra)

library(dplyr)

#Variable omitida
g11 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,1], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,1]), sd=sd(betas_df[,1])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=50") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g21 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,2], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,2]), sd=sd(betas_df[,2])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=100") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g31 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,3], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,3]), sd=sd(betas_df[,3])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=500") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g41 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,4], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,4]), sd=sd(betas_df[,4])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=1000") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

grid.arrange(g11, g21, g31, g41, nrow=2, ncol=2)



#Sin sesgo
g11 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,5], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,5]), sd=sd(betas_df[,5])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=50") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g21 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,6], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,6]), sd=sd(betas_df[,6])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=100") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g31 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,7], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,7]), sd=sd(betas_df[,7])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=500") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g41 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,8], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,8]), sd=sd(betas_df[,8])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=1000") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

grid.arrange(g11, g21, g31, g41, nrow=2, ncol=2)


#Letra C

set.seed(123)

reps = 10000

betas = matrix(NA, nrow = reps, ncol = 8)

beta0 = 2

beta1 = 2.5

beta2 = 1

su = 1

n = c(50, 100, 500, 1000)  #tamaño muestral

for (j in 1:length(n)) {
  
  X1=rnorm(n[j],20,1)
  
  X2=runif(n[j],0,1)
  
  for (i in 1:reps) {
    
    u= rnorm(n[j],0,su)
    
    v = beta2*X2 + u
    
    Y_omit = beta0 + beta1*X1 + v
    Y_pob = beta0 + beta1*X1 + beta2*X2 + u
    
    reg_omit = lm(Y_omit~X1)  
    
    betas[i,j] = reg_omit$coef[2]
    
    reg_pob = lm(Y_pob~X1+X2)
    
    betas[i,j+4] = reg_pob$coef[2]
    
    
  }
  
}

betas_df2 <- data.frame(betas)

#Betas 1 Con sesgo(1-4) y Sin sesgo (5-8)
apply(betas_df2, 2, mean)

#Varianza Con sesgo(1-4) y Sin sesgo (5-8)
apply(betas_df2,2,var)

#Variable omitida
g11 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,1], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,1]), sd=sd(betas_df2[,1])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=50") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g21 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,2], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,2]), sd=sd(betas_df2[,2])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=100") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g31 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,3], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,3]), sd=sd(betas_df2[,3])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=500") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g41 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,4], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,4]), sd=sd(betas_df2[,4])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=1000") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

grid.arrange(g11, g21, g31, g41, nrow=2, ncol=2)



#modelo poblacional
g11 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,5], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,5]), sd=sd(betas_df2[,5])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=50") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g21 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,6], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,6]), sd=sd(betas_df2[,6])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=100") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g31 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,7], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,7]), sd=sd(betas_df2[,7])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=500") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g41 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,8], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,8]), sd=sd(betas_df2[,8])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=1000") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

grid.arrange(g11, g21, g31, g41, nrow=2, ncol=2)
