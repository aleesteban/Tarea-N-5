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

#Con sesgo
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
    
    betas2[i,j] = reg_omit$coef[2]
    
    reg_pop = lm(Y_pop~X1+X2)
    
    betas2[i,j+4] = reg_pop$coef[2]
    
    
  }
  
}

betas_df2 <- data.frame(betas2)



