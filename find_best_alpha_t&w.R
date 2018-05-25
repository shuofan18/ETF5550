
####################### to find the best alpha for t-test for the "test set" in 1st experiment ######################

ttest_power <- read.csv("~/documents/linear&norela/parameters/parameters3.csv")
ttest_alpha <- read.csv("~/documents/linear&norela/parameters/parameters4.csv")

library(tidyverse)
datf<-NULL
for (alpha in seq(0.005,0.1, 0.005)) {
  tpower <- (ttest_power$ct.p.value<alpha) %>% mean()
  talpha <- (ttest_alpha$ct.p.value>alpha) %>% mean()
  acc <- (tpower+talpha)/2
  res <- c(tpower,talpha, acc,alpha)
  datf <- rbind(datf,res)
}

datf
datf[which.max(datf[,3]),]

####################### to find the best alpha for white-test for the "test set" in 2nd experiment ######################

wtest_power <- read.csv("~/documents/heter&linear/parameters/parameters3.csv")
wtest_alpha <- read.csv("~/documents/heter&linear/parameters/parameters4.csv")

wdatf<-NULL
for (alpha in seq(0.005,0.1, 0.005)) {
  crit <- qchisq(1-alpha,2)
  tpower <- (wtest_power$wt_stat > crit) %>% mean()
  talpha <- (wtest_alpha$wt_stat < crit) %>% mean()
  acc <- (tpower+talpha)/2
  res <- c(tpower,talpha, acc,alpha)
  wdatf <- rbind(wdatf,res)
}

wdatf

wdatf[which.max(wdatf[,3]),]


















