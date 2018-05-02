install.packages("tidyverse")

library(tidyverse)
library(keras)

linear<-function(i){
  
  n <- sample(c(100, 300), 1)
  
  beta <- sample(c((-10:-0.1), (0.1:10)), 1)
  
  sigma <- sample(1:12 , 1)
  x<-rnorm(n, 0, 1)
  y<-rnorm(n, beta*x, sigma)
  
  res <- y
  yhat <- x
  tibble(res, yhat) %>% 
    ggplot(aes(x = yhat, y=res)) + 
    geom_point(alpha = 0.4) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          aspect.ratio = 1)
  ggsave(filename = paste("linear_", i, ".png", sep = ""), height = 2, width = 2, dpi = 150)
}


norela<-function(i){
  
  n <- sample(c(100, 300), 1)
  
  beta <- 0
  
  sigma <- sample(1:12 , 1)
  x<-rnorm(n, 0, 1)
  y<-rnorm(n, beta*x, sigma)
  
  res <- y
  yhat <- x
  tibble(res, yhat) %>% 
    ggplot(aes(x = yhat, y=res)) + 
    geom_point(alpha = 0.4) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          aspect.ratio = 1)
  ggsave(filename = paste("norela_", i, ".png", sep = ""), height = 2, width = 2, dpi = 150)
}

setwd("~/p2016030007/szha0004/no_lineup/train/linear")
mapply(linear, 1:100000)
setwd("~/p2016030007/szha0004/no_lineup/train/norela")
mapply(norela, 1:100000)

setwd("~/p2016030007/szha0004/no_lineup/test/linear")
mapply(linear, 1:50000)
setwd("~/p2016030007/szha0004/no_lineup/test/norela")
mapply(norela, 1:50000)

setwd("~/p2016030007/szha0004/no_lineup/validation/linear")
mapply(linear, 1:50000)
setwd("~/p2016030007/szha0004/no_lineup/validation/norela")
mapply(norela, 1:50000)















