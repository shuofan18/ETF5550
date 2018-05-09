
setwd("/volumes/5550/turk2_results")

# the most common choices by human for each lineup = human_choices
# the actual location of the real plot in each lineup = newdata$plot_location


library(tidyverse)
data <- read.csv("data_turk2.csv")
file <- read.csv("/volumes/5550/turk2/file.csv")
ntrue <- data %>% group_by(pic_name) %>% summarise(ntrue = sum(response))
count <- table(data$pic_name) %>% data.frame() 
png_name <- as.character(file$png_nolc)
human_choices <- vector()
for (i in 1:70) {
  datai <- subset(data, pic_name == png_name[i])
  human_choices[i] <- names(which.max(table(datai$response_no)))
}

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
acc_human <- sum(newdata$conclusion)/70

difficulty_true <- newdata %>% group_by(difficulty) %>% summarise(pb_true = mean(conclusion))


















