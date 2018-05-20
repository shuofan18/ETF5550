library(tidyverse)

linear<-function(i){
  n <- sample(c(100, 300), 1)
  beta <- sample(c((-10:-0.1), (0.1:10)), 1)
  sigma <- sample(1:12 , 1)
  x<-rnorm(n, 0, 1)
  y<-rnorm(n, beta*x, sigma)
  res <- y
  yhat <- x
  ct <- cor.test(res, yhat)
  return(ct$p.value)
}

norela<-function(i){
  n <- sample(c(100, 300), 1)
  beta <- 0
  sigma <- sample(1:12 , 1)
  x<-rnorm(n, 0, 1)
  y<-rnorm(n, beta*x, sigma)
  res <- y
  yhat <- x
  ct <- cor.test(res, yhat)
  return(ct$p.value)
}

acc_calculator <- function(alpha, i){
  alpha <- alpha*0.005
  power <- (sapply(1:i, linear) < alpha) %>% mean()
  alpha_acc <- (sapply(1:i, norela) > alpha) %>% mean()
  acc <- (power+alpha_acc)/2
  res <- c(alpha, acc)
  return(res)
}

results <- sapply(1:20, acc_calculator, i=200000)
results <- results %>% as.data.frame() %>% t()
results[,2] %>% which.max()  ###### when alpha=0.02, acc is maximized


#acc_t_power <- (sapply(1:200000, linear) < 0.05) %>% mean()
#acc_t_power
#acc_t_1_alpha <- (sapply(1:200000, norela) > 0.05) %>% mean()
#acc_t_1_alpha





