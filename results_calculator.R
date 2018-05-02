
setwd("/volumes/5550/turk2_results")

library(tidyverse)
data <- read.csv("data_turk2.csv")
ntrue <- data %>% group_by(pic_name) %>% summarise(ntrue = sum(response))
count <- table(data$pic_name) %>% data.frame() 

ntrue <- ntrue %>% 
  left_join(select(data, difficulty, pic_name, 
                   sample_size, p_value, beta, plot_location, sigma, replica)) %>% 
  distinct() %>% 
  left_join(count, by = c("pic_name"="Var1"))

newdata <- ntrue

p_cal <- function(k, x){
  prob <- dbinom(x, k, 0.05)
  return(prob)
}

p_value <- vector()
for (i in 1:70) {
  ki <- newdata$Freq[i]
  xi <- newdata$ntrue[i]
  p_value[i] <- p_cal(k=ki, x=xi)
}

newdata %>% mutate(p_value)
newdata$conclusion <- (newdata$p_value < 0.05)
sum(newdata$conclusion)

difficulty_true <- newdata %>% group_by(difficulty) %>% summarise(pb_true = mean(conclusion))


















