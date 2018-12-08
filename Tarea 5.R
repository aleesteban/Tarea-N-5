library(tidyverse)
library(dplyr)
library(tidyquant)
library(ggplot2)
library(ggthemes)

MSFT_price <- tq_get("MSFT",
                     get = "stock.prices",
                     from = "1999-11-01",
                     to = "2018-09-05",
                     periodicity = "monthly")

MSFT_quantmod <- getSymbols("MSFT",
                            src = "yahoo",
                            from = "1999-11-01",
                            to = "2018-09-05",
                            periodicity = "monthly")

AAPL_price <- tq_get("AAPL",
                     get = "stock.prices",
                     from = "1999-11-01",
                     to = "2018-09-05",
                     periodicity = "monthly")

AAPL_quantmod <- getSymbols("AAPL",
                            src = "yahoo",
                            from = "1999-11-01",
                            to = "2018-09-05",
                            periodicity = "monthly")

MSFT_quantmod <- as.data.frame(MSFT) %>%
  as.tibble() %>%
  mutate(date = index(MSFT)) %>%
  rename("open" = "MSFT.Open", "high" = "MSFT.High",
         "low" = "MSFT.Low" , "close"= "MSFT.Close",
         "volume" = "MSFT.Volume", "adjusted" = "MSFT.Adjusted")
x <- length(MSFT_price$adjusted)

AAPL_quantmod <- as.data.frame(AAPL) %>%
  as.tibble() %>%
  mutate(date = index(AAPL)) %>%
  rename("open" = "AAPL.Open", "high" = "AAPL.High",
         "low" = "AAPL.Low" , "close"= "AAPL.Close",
         "volume" = "AAPL.Volume", "adjusted" = "AAPL.Adjusted")
y <- length(AAPL_price$adjusted)

# letra (a)

for (i in 1:x) {
  MSFT_price[i+1,8] = log(MSFT_price[i+1,7] / MSFT_price[i,7])
  if(i <= 2) {MSFT_price[i+1,9] = MSFT_price[i+1,8] + MSFT_price[i,8]}
  else{MSFT_price[i+1,9] = MSFT_price[i,9] + MSFT_price[i+1,8]}
  
}

for(i in 1:y){
  AAPL_price[i+1,8] = log(AAPL_price[i+1,7]/AAPL_price[i,7])
  if(i <= 2){AAPL_price[i+1,9] = AAPL_price[i+1,8] + AAPL_price[i,8]}
  else{AAPL_price[i+1,9] = AAPL_price[i,9] + AAPL_price[i+1,8]}
}

MSFT_price = MSFT_price %>% rename(retornos_activos = "adjusted.1")
MSFT_price = MSFT_price %>% rename(retornos_acum_activos = "adjusted.1.1")

MSFT_retorno <- MSFT_price[3:226,]
r1 <- length(MSFT_retorno$retornos_activos)
sum(MSFT_retorno$retornos_activos)

AAPL_price = AAPL_price %>% rename(retornos_activos = "adjusted.1")
AAPL_price = AAPL_price %>% rename(retornos_acum_activos = "adjusted.1.1")

AAPL_retorno <- AAPL_price[3:226,]
r2 <- length(AAPL_price$retornos_activos)
sum(AAPL_price$retornos_activos)

# letra (b)

#Gráficos Microsoft

ggplot(MSFT_retorno) + geom_line(aes(x=date, y=retornos_activos), color = "red") + 
  labs(title = "Retorno mensual Microsoft", subtitle = "Desde Enero 2000 hasta Agosto 2018")+ 
  theme_tq() + scale_color_tq()

ggplot(MSFT_retorno) + geom_line(aes(x=date, y=retornos_acum_activos), color = "red") + 
  labs(title = "Retorno Acumulado mensual Microsoft", subtitle = "Desde Enero 2000 hasta Agosto 2018")+ 
  theme_tq() + scale_color_tq()

#Gráficos Apple

ggplot(AAPL_retorno) + geom_line(aes(x=date, y=retornos_activos), color = "red") + 
  labs(title = "Retorno mensual Apple", subtitle = "Desde Enero 2000 hasta Agosto 2018")+ 
  theme_tq() + scale_color_tq()

ggplot(AAPL_retorno) + geom_line(aes(x=date, y=retornos_activos), color = "red") + 
  labs(title = "Retorno Acumulado mensual Apple", subtitle = "Desde Enero 2000 hasta Agosto 2018")+ 
  theme_tq() + scale_color_tq()

# letra (c)

m1 <- sum(MSFT_retorno$retornos_activos)/r1 
m2 <- sum((MSFT_retorno$retornos_activos - m1)^2)  
m3 <- sum((MSFT_retorno$retornos_activos - m1)^3)  
m4 <- sum((MSFT_retorno$retornos_activos - m1)^4) 

asim1 <- (m3/m2^(3/2))^2 #calculo coeficiente de  
asim2 <- (m4/m2^2) 

#Test Jarque-Bera

JB_MSFT <- r1*asim1/6+(asim2-3)^2/24

#Chi-cuadrado

chi_cuadrado_MSFT <- 1 - pchisq(JB_MSFT, df = 2)

#H0: retornos presentan normalidad 
#H1: retornos no presentan normalidad

norm_MSFT = if(JB_MSFT > chi_cuadrado_MSFT){"Se rechaza la hipotesis nula"}else{"No se rechaza la hipotesis nula"}
norm_MSFT

a1 <- sum(AAPL_retorno$retornos_activos)/r2 
a2 <- sum((AAPL_retorno$retornos_activos - a1)^2) 
a3 <- sum((AAPL_retorno$retornos_activos - a1)^3)  
a4 <- sum((AAPL_retorno$retornos_activos - a1)^4) 

asima1 <- (a3/a2^(3/2))^2 
asima2 <- (a4/a2^2) 

JB_AAPL <- r2*asima1/6+(asima2-3)^2/24

chi_cuadrado_AAPL <- 1 - pchisq(JB_AAPL, df = 2)

norm_AAPL= if(JB_AAPL > chi_cuadrado_AAPL){"Se rechaza la hipotesis nula"}else{"No se rechaza la hipotesis nula"}
norm_AAPL






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

#Betas 1 
apply(betas_df, 2, mean)

#Varianzas
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
                
                geom="line", colour="red", size=1) + labs(subtitle = "y = β0 + β1x1 + v, donde v = β2x2 + u") +
  
  ylab("Densidad") +   ggtitle("Modelo con Variable Omitida (n=50)") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g21 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,2], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,2]), sd=sd(betas_df[,2])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("Modelo con Variable Omitida (n=100)") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()+ labs(subtitle = "y = β0 + β1x1 + v, donde v = β2x2 + u")

g31 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,3], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,3]), sd=sd(betas_df[,3])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("Modelo con Variable Omitida (n=500)") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()+ labs(subtitle = "y = β0 + β1x1 + v, donde v = β2x2 + u")

g41 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,4], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,4]), sd=sd(betas_df[,4])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("Modelo con Variable Omitida (n=1000)") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()+ labs(subtitle = "y = β0 + β1x1 + v, donde v = β2x2 + u")



#Sin variable omitida

g51 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,5], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,5]), sd=sd(betas_df[,5])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("Modelo sin Variable Omitida (n=50)") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()+ labs(subtitle = "y = β0 + β1x1 + β2x2 + u")

g61 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,6], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,6]), sd=sd(betas_df[,6])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("Modelo sin Variable Omitida (n=100)") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()+ labs(subtitle = "y = β0 + β1x1 + β2x2 + u")

g71 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,7], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,7]), sd=sd(betas_df[,7])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("Modelo sin Variable Omitida (n=500)") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()+ labs(subtitle = "y = β0 + β1x1 + β2x2 + u")

g81 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,8], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,8]), sd=sd(betas_df[,8])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("Modelo sin Variable Omitida (n=1000)") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()+ labs(subtitle = "y = β0 + β1x1 + β2x2 + u")

grid.arrange(g11, g21, g31, g41, g51, g61, g71, g81, nrow=2, ncol=4)


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

#Betas 1 
apply(betas_df2, 2, mean)

#Varianzas
apply(betas_df2,2,var)


#Modelo con Variable omitida
g12 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,1], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,1]), sd=sd(betas_df2[,1])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("Modelo con Variable Omitida (n=50)") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()+ labs(subtitle = "y = β0 + β1x1 + v, donde v = β2x2 + u")

g13 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,2], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,2]), sd=sd(betas_df2[,2])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("Modelo con Variable Omitida (n=100)") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()+ labs(subtitle = "y = β0 + β1x1 + v, donde v = β2x2 + u")

g14 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,3], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,3]), sd=sd(betas_df2[,3])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("Modelo con Variable Omitida (n=500)") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()+ labs(subtitle = "y = β0 + β1x1 + v, donde v = β2x2 + u")

g15 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,4], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,4]), sd=sd(betas_df2[,4])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("Modelo con Variable Omitida (n=1000)") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()+ labs(subtitle = "y = β0 + β1x1 + v, donde v = β2x2 + u")



#modelo sin variable omitida
g16 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,5], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,5]), sd=sd(betas_df2[,5])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("Modelo sin Variable Omitida (n=50)") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()+ labs(subtitle = "y = β0 + β1x1 + β2x2 + u")

g17 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,6], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,6]), sd=sd(betas_df2[,6])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("Modelo sin Variable Omitida (n=100)") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()+ labs(subtitle = "y = β0 + β1x1 + β2x2 + u")

g18 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,7], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,7]), sd=sd(betas_df2[,7])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("Modelo sin Variable Omitida (n=500)") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()+ labs(subtitle = "y = β0 + β1x1 + β2x2 + u")

g19 <- ggplot(betas_df2) + 
  
  geom_histogram(aes(betas_df2[,8], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df2[,8]), sd=sd(betas_df2[,8])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("Modelo sin Variable Omitida (n=1000)") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()+ labs(subtitle = "y = β0 + β1x1 + β2x2 + u")

grid.arrange(g12, g13, g14, g15, g16, g17, g18, g19, nrow=2, ncol=4)
