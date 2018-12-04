#Pregunta 3A

set.seed(123)

reps = 10000

betas = matrix(NA, nrow = reps, ncol = 12)

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
    
    Y = beta0 + beta1*X1+ beta2*X2 + u 
    
    model = lm(Y~X1+X2)  
    
    betas[i,j] = model$coef[1]
    
    betas[i,j+4] = model$coef[2]
    
    betas[i,j+8] = model$coef[3]
    
  }
  
}

betas_df <- data.frame(betas)
apply(betas_df, 2, mean)

#Letra B

library(ggplot2)

library(gridExtra)

library(dplyr)

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

#no existe sesgo 




#Letra C

betas2 = matrix(NA, nrow = reps, ncol = 8)

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
    
    v = beta2*X2+u
    
    Y_CS = beta0 + beta1*X1 + v 
    Y_SS = beta0 + beta1*X1 + beta2*X2 + u
    
    modeloCS = lm(Y_CS~X1)  
    
    betas2[i,j] = modeloCS$coef[2]
    
    modeloSS = lm(Y_SS~X1+X2) 
    
    betas2[i,j+4] = modeloSS$coef[2]
    
  }
  
}

betas_df2 <- data.frame(betas2)

apply(betas_df2, 2, mean)

#Graficos

#Con sesgo 
g11 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,5], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,1]), sd=sd(betas_df2[,5])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=50") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g21 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,6], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,2]), sd=sd(betas_df2[,6])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=100") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g31 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,7], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,3]), sd=sd(betas_df2[,7])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=500") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g41 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,8], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,4]), sd=sd(betas_df2[,8])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=1000") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

grid.arrange(g11, g21, g31, g41, nrow=2, ncol=2)

#Sin Sesgo

g51 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,5], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,5]), sd=sd(betas_df2[,5])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=50") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g61 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,6], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,6]), sd=sd(betas_df2[,6])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=100") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g71 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,7], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,7]), sd=sd(betas_df2[,7])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=500") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g81 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,8], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,8]), sd=sd(betas_df2[,8])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=1000") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

grid.arrange(g51, g61, g71, g81, nrow=2, ncol=2)