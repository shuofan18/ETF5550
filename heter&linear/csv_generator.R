############################## generate data for training ######################
library(tidyverse)
library(broom)
crit_value <- qchisq(0.95, df=2)
white_test <- function(res, yhat, n){
  res2 <- res^2
  yhat2 <- yhat^2
  model <- lm(res2 ~ yhat + yhat2)
  sss <- summary(model)$r.squared
  wt_stat <- n*sss
  return(wt_stat)
}

heter_csv <- function(i){

  n = sample(100:500, 1)
  x <- runif(n, -1, 1)
  beta <- runif(1,0.5,1)
  a <- runif(1, 0.05, 5)*(-1)^(rbinom(1, 1, 0.5))
  sd <- a*x+rnorm(n, 0, 1)

  min <- min(sd)
  if (min<0){
    sd <- sd-min
  }

  y<-rnorm(n, beta*x, sd)
  df <- tibble(x, y)
  model<-lm(y ~ x, data=df)

  fit <- augment(model, df)
  wt_stat <- white_test(fit$.resid, fit$.fitted, n)

  if (wt_stat > crit_value){
    wt_conclusion <- 1 #1=heter
  } else {
    wt_conclusion <- 0 #0=norela
  }
  #### generate images of "heter"
  xyres <- tibble(x, y, fit$.std.resid)
   write.csv(xyres, file = paste0(i, "_n(", n, ")_beta(", round(beta, digits = 2), ")_a(",
     round(a, digits = 2), ")_wtstat(",
     round(wt_stat, digits=2),")_wt(", wt_conclusion, ").csv", sep=""))
}


linear_csv <- function(i){

  n = sample(100:500, 1)
  x <- runif(n, -1, 1)
  beta <- runif(1,0.5,1)
  a <- runif(1, 0.05, 5)*(-1)^(rbinom(1, 1, 0.5))
  sd <- a*x+rnorm(n, 0, 1)
  min <- min(sd)
  if (min<0){
    sd <- sd-min
  }
  sd <- mean(sd)
  y<-rnorm(n, beta*x, sd)
  df <- tibble(x, y)
  model<-lm(y ~ x, data=df)

  fit <- augment(model, df)
  wt_stat <- white_test(fit$.resid, fit$.fitted, n)

  if (wt_stat > crit_value){
    wt_conclusion <- 1 #1=heter
  } else {
    wt_conclusion <- 0 #0=norela
  }
  xyres <- tibble(x, y, fit$.std.resid)
  write.csv(xyres, file = paste0(i, "_n(", n, ")_beta(", round(beta, digits = 2), ")_a(",
                   round(a, digits = 2), ")_sd(", round(sd, digits=2),")_wtstat(",
                   round(wt_stat, digits=2),")_wt(", wt_conclusion, ").csv", sep=""))
}




set.seed(0522)
setwd("/Users/shuofanzhang/documents/heter&linear/train/heter/csv")
lapply(1:100000, heter_csv)
###### save heter images to test folder
setwd("/Users/shuofanzhang/documents/heter&linear/test/heter/csv")
lapply(1:100000, heter_csv)
###### save heter images to validation folder
setwd("/Users/shuofanzhang/documents/heter&linear/validation/heter/csv")
lapply(1:40000, heter_csv)
###### save linear images to train folder
setwd("/Users/shuofanzhang/documents/heter&linear/train/linear/csv")
lapply(1:100000, linear_csv)
###### save linear images to test folder
setwd("/Users/shuofanzhang/documents/heter&linear/test/linear/csv")
lapply(1:100000, linear_csv)
###### save linear images to validation folder
setwd("/Users/shuofanzhang/documents/heter&linear/validation/linear/csv")
lapply(1:40000, linear_csv)















