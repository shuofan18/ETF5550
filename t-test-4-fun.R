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
  res <- c(alpha, acc, power, alpha_acc)
  return(res)
}

results <- sapply(1:20, acc_calculator, i=200000)
results <- results %>% as.data.frame() %>% t()
results[,2] %>% which.max()  ###### when alpha=0.02, acc is maximized


#acc_t_power <- (sapply(1:200000, linear) < 0.05) %>% mean()
#acc_t_power
#acc_t_1_alpha <- (sapply(1:200000, norela) > 0.05) %>% mean()
#acc_t_1_alpha

white_power_test <- read.csv("/volumes/5550/heter&linear/parameters/parameters6.csv")
white_power_test <- white_power_test$wt_conclusion %>% mean()
library(keras)
model_loaded_15 <- load_model_hdf5("/Volumes/5550/heter&linear/checkpoints/523new15epoches/weights.11-0.08.hdf5") 
base_dir <- "/Volumes/5550/heter&linear"
test_dir <- file.path(base_dir,"test")
test_datagen <- image_data_generator(rescale = 1/255)
test_generator <- flow_images_from_directory(
  test_dir,
  test_datagen,
  target_size = c(150, 150),
  color_mode = "grayscale",
  batch_size = 100,
  class_mode = "binary"
)
  
acc11epoch_power <- model_loaded_15 %>% evaluate_generator(test_generator, steps = 1000)

model_loaded_4 <- load_model_hdf5("/Volumes/5550/heter&linear/checkpoints/523new15epoches/weights.04-0.09.hdf5")
acc4epoch_power <- model_loaded_4 %>% evaluate_generator(test_generator, steps = 1000)







