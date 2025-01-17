library(tidyverse)
library(keras)
library(broom)
################### function for white-test statistic ###########
crit_value <- qchisq(0.95, df=2)
white_test <- function(res, yhat, n){
  res2 <- res^2
  yhat2 <- yhat^2
  model <- lm(res2 ~ yhat + yhat2)
  sss <- summary(model)$r.squared
  wt_stat <- n*sss
  return(wt_stat)
}
################ function for generate lineup for human experiment #########
heter_lineup<-function(i){
  
  n = sample(100:500, 1)
  x <- runif(n, -1, 1)
  beta <- runif(1,0.5,1)
  a <- runif(1, -2, -1) #*(-1)^(rbinom(1, 1, 0.5))
  sd <- a*x+rnorm(n, 0, 1)
  
  min <- min(sd)
  if (min<0){
    sd <- sd-min
  }
  
  y<-rnorm(n, beta*x, sd)
  
  df <- tibble(x, y)
  model<-lm(y ~ x, data=df)
  
  fit <- augment(model, df)
  sample_sd <- sd(fit$.resid)
  wt_stat <- white_test(fit$.resid, fit$.fitted, n)
  if (wt_stat > crit_value){
    wt_conclusion <- 1 #1=heter
  } else {
    wt_conclusion <- 0 #0=norela
  }
  ##generate lineup 
  pos <- sample(1:20, 1)
  lineup_data <- fit %>% select(x, .std.resid) %>% mutate(.sample = pos)
  if (pos == 1) {
    for (i2 in 2:20) {
      null_y <- rnorm(n, beta*x, sample_sd)
      null_df <- tibble(null_y, x)
      null_model<-lm(null_y ~ x, data=null_df)
      null_fit <- augment(null_model, null_df)
      tmp <- null_fit %>% select(x, .std.resid) %>% mutate(.sample = i2)
      lineup_data <- bind_rows(lineup_data, tmp)
    }
  } else if (pos == 20) {
    for (i2 in 1:19) {
      null_y <- rnorm(n, beta*x, sample_sd)
      null_df <- tibble(null_y, x)
      null_model<-lm(null_y ~ x, data=null_df)
      null_fit <- augment(null_model, null_df)
      tmp <- null_fit %>% select(x, .std.resid) %>% mutate(.sample = i2)
      lineup_data <- bind_rows(tmp , lineup_data)
    }
  } else {
    for (i2 in 1:(pos-1)) {
      null_y <- rnorm(n, beta*x, sample_sd)
      null_df <- tibble(null_y, x)
      null_model<-lm(null_y ~ x, data=null_df)
      null_fit <- augment(null_model, null_df)
      tmp <- null_fit %>% select(x, .std.resid) %>% mutate(.sample = i2)
      lineup_data <- bind_rows(tmp , lineup_data)
    }
    for (i2 in (pos+1):20) {
      null_y <- rnorm(n, beta*x, sample_sd)
      null_df <- tibble(null_y, x)
      null_model<-lm(null_y ~ x, data=null_df)
      null_fit <- augment(null_model, null_df)
      tmp <- null_fit %>% select(x, .std.resid) %>% mutate(.sample = i2)
      lineup_data <- bind_rows(lineup_data, tmp)
    }
  }
  
  ggplot(filter(lineup_data, lineup_data$.sample==pos), aes(x, .std.resid))+geom_point(alpha = 0.4)+
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks=element_blank(),
          aspect.ratio = 1)
  
  ggsave(filename = 
           paste(i, "_real(", pos,")a(" , round(a,digits = 2), ")n(", n,
                 ")wtst(", round(wt_stat, digits = 2) ,")wt(" , wt_conclusion,").png", 
                 sep = ""), height = 8, width = 10, dpi = 200)
  return(wt_conclusion)
}

set.seed(0517)
setwd("/volumes/5550/panda2/x&y")
accuracy_wtest_heter_train <- sapply(1:4, heter_lineup) %>% sum()/4
accuracy_wtest_heter_train





