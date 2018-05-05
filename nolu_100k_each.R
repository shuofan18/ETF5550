
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
  
  ct <- cor.test(res, yhat)
  return(ct$p.value)
}


setwd("~/p2016030007/szha0004/no_lineup/train/linear")
accuracy_cortest_linear_train <- (sapply(1:100000, linear) < 0.05) %>% mean()
setwd("~/p2016030007/szha0004/no_lineup/train/norela")
accuracy_cortest_norela_train <- (sapply(1:100000, norela) > 0.05) %>% mean()


setwd("~/p2016030007/szha0004/no_lineup/test/linear")
accuracy_cortest_linear_test <- (sapply(1:50000, linear) < 0.05) %>% mean()
setwd("~/p2016030007/szha0004/no_lineup/test/norela")
accuracy_cortest_norela_test <- (sapply(1:50000, norela) > 0.05) %>% mean()

setwd("~/p2016030007/szha0004/no_lineup/validation/linear")
accuracy_cortest_linear_validation <- (sapply(1:50000, linear) < 0.05) %>% mean()
setwd("~/p2016030007/szha0004/no_lineup/validation/norela")
accuracy_cortest_norela_validation <- (sapply(1:50000, norela) > 0.05) %>% mean()
  
######## image generating finish, start training ###########

base_dir <- "~/p2016030007/szha0004/no_lineup"
train_dir <- file.path(base_dir,"train")
validation_dir <- file.path(base_dir,"validation")
test_dir <- file.path(base_dir,"test")

train_datagen <- image_data_generator(rescale = 1/255)             
validation_datagen <- image_data_generator(rescale = 1/255)  
test_datagen <- image_data_generator(rescale = 1/255)

train_generator <- flow_images_from_directory(
  train_dir,                                                       
  train_datagen,                                                   
  target_size = c(150, 150), 
  color_mode = "grayscale",
  batch_size = 1000,                                                 
  class_mode = "binary"
)

validation_generator <- flow_images_from_directory(
  validation_dir,
  validation_datagen,
  target_size = c(150, 150),
  color_mode = "grayscale",
  batch_size = 500,
  class_mode = "binary"
)

test_generator <- flow_images_from_directory(
  test_dir,
  test_datagen,
  target_size = c(150, 150),
  color_mode = "grayscale",
  batch_size = 500,
  class_mode = "binary"
)

############ define deep learning model ################

model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(150, 150, 1)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = optimizer_rmsprop(lr = 1e-4),
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

#train
history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 100,
  epochs = 100,
  validation_data = validation_generator,
  validation_steps = 500
)

plot(history)

model %>% save_model_hdf5("nolu_100k_each.h5")

# evaluate convnets on test set
model %>% evaluate_generator(test_generator, steps = 500)

# overall accuracy of cor.test on test set
accuracy_cortest_test <- mean(c(accuracy_cortest_linear_test, accuracy_cortest_norela_test))

# all cor.test performance
accuracy_cortest_linear_train 
accuracy_cortest_norela_train 
accuracy_cortest_linear_test 
accuracy_cortest_norela_test 
accuracy_cortest_linear_validation 
accuracy_cortest_norela_validation 



