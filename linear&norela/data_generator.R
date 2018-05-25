
library(tidyverse)
library(keras)

#parameters1 <- data.frame()  #train/linear
#parameters2 <- data.frame() #train/norela
#parameters3 <- data.frame() #test/linear
#parameters4 <- data.frame() #test/norela
#parameters5 <- data.frame() #validation/linear
#parameters6 <- data.frame() #train/norela


linear <- function(i){
  n <- sample(50:500, 1)
  beta <- runif(1, min=0.1, max=10)*(-1)^(rbinom(1,1,prob = 0.5))
  sigma <- runif(1, min=1, max=12)
  x<-rnorm(n, 0, 1)
  y<-rnorm(n, beta*x, sigma)
  ct <- cor.test(x, y)
  xy <- tibble(x, y)
  ggplot(xy, aes(x = x, y=y)) +
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
  ggsave(filename = paste0(i, "_n(", n, ")_beta(", beta, ")_sigma(", sigma,
                           ")_pv(", round(ct$p.value, digits=2), ").png", sep=""),
         height = 2, width = 2, dpi = 150)
  #write.csv(xy, file = paste0(i, "_n(", n, ")_beta(", beta, ")_sigma(", sigma,
  #                            ")_pv(", round(ct$p.value, digits=2), ").csv", sep=""))
  #parameters <- tibble(n,beta,sigma,ct$p.value)
  #return(parameters)
}

norela<-function(i){
  n <- sample(50:500, 1)
  beta <- 0
  sigma <- runif(1, min=1, max=12)
  x<-rnorm(n, 0, 1)
  y<-rnorm(n, beta*x, sigma)
  ct <- cor.test(x, y)
  xy <- tibble(x, y)
  ggplot(xy, aes(x = x, y=y)) +
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
  ggsave(filename = paste0(i, "_n(", n,  ")_sigma(", sigma,
                           ")_pv(", round(ct$p.value, digits=2), ").png", sep=""),
         height = 2, width = 2, dpi = 150)
  #write.csv(xy, file = paste0(i, "_n(", n, ")_beta(", beta, ")_sigma(", sigma,
  #                           ")_pv(", round(ct$p.value, digits=2), ").csv", sep=""))
  #parameters <- tibble(n,beta,sigma,ct$p.value)
  #return(parameters)
}

set.seed(0521)
setwd("/Users/shuofanzhang/documents/linear&norela/train/linear/plots")
lapply(1:100000, linear)

setwd("/Users/shuofanzhang/documents/linear&norela/test/linear/plots")
lapply(1:100000, linear)

setwd("/Users/shuofanzhang/documents/linear&norela/validation/linear/plots")
lapply(1:40000, linear)

setwd("/Users/shuofanzhang/documents/linear&norela/train/norela/plots")
lapply(1:100000, norela)

setwd("/Users/shuofanzhang/documents/linear&norela/test/norela/plots")
lapply(1:100000, norela)

setwd("/Users/shuofanzhang/documents/linear&norela/validation/norela/plots")
lapply(1:40000, norela)

parameters <- rbind(parameters1, parameters2, parameters3, parameters4, parameters5, parameters6)
parameters_b <- rbind(parameters1, parameters3, parameters5)
parameters_0 <- rbind(parameters2, parameters4, parameters6)
setwd("/Users/shuofanzhang/documents/linear&norela")
write.csv(parameters1, file="parameters1.csv")
write.csv(parameters2, file="parameters2.csv")
write.csv(parameters3, file="parameters3.csv")
write.csv(parameters4, file="parameters4.csv")
write.csv(parameters5, file="parameters5.csv")
write.csv(parameters6, file="parameters6.csv")
write.csv(parameters, file="parameters.csv")
write.csv(parameters_b, file="parameters_b.csv")
write.csv(parameters_0, file="parameters_0.csv")

# for the whole sample, we draw histogram of n, histogram of sigma, and histogram of ct_pvalue; scatter plot of sigma against n
hist_n <- ggplot(parameters, aes(n))+geom_histogram()
hist_n
hist_sigma <- ggplot(parameters, aes(sigma))+geom_histogram()
hist_sigma
hist_p <- ggplot(parameters, aes(parameters[,4]))+geom_histogram()
hist_p
scap_sigma_n <- ggplot(parameters, aes(x=n, y=sigma))+geom_point(alpha=0.05)
scap_sigma_n

#for linear group, we draw scatter plot of beta against n, and histogram of ct_pvalue and beta
hist_b <- ggplot(parameters_b, aes(beta))+geom_histogram(binwidth = 0.05)
hist_b
scap_beta_n <- ggplot(parameters_b, aes(x=n, y=beta))+geom_point(alpha=0.1)
scap_beta_n








