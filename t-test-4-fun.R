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

acc_t_power <- (sapply(1:200000, linear) < 0.05) %>% mean()
acc_t_power
acc_t_1_alpha <- (sapply(1:200000, norela) > 0.05) %>% mean()
acc_t_1_alpha





