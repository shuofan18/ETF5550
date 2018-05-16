
library(tidyverse)
library(keras)
################### function for white-test ###########
crit_value <- qchisq(0.95, df=2)
white_test <- function(res, yhat, n){
  res2 <- res^2
  yhat2 <- yhat^2
  model <- lm(res2 ~ yhat + yhat2)
  sss <- summary(model)$r.squared
  test_stat <- n*sss
  if (test_stat > crit_value){
    wt_conclusion <- 1 #1=heter
  } else {
    wt_conclusion <- 0 #0=norela
  }
  return(wt_conclusion)
}
################ function for generate heteroskedasticity plots ######
heter<-function(i){
  n=sample(20:1500, 1)
  x<-rnorm(n, 0, 3)
  beta<-runif(1,0.5,1)
  a <- runif(1,0,2)*rbinom(1,1,0.5)
  b <- runif(1,-4,4)
  c <- rnorm(1,0,2)
  variance <- 0.25*(a*x^2+b*x+c)+rnorm(n,0,0.25)
  index <- sample(0:1,1)
 if (index==1) {
    variance <- -variance
  }
  min <- min(variance)
  if (min<0){
    variance <- variance-min
  }
  max <- max(variance)
  while (max>5) {
    variance <- 0.8* variance
    max <- max(variance)
  }
  y<-rnorm(n, beta*x, variance)
  tibble(x, y)
  model<-lm(y ~ x)
  res<-residuals(model)
  yhat<-predict(model)
  res<-(res-mean(res))/sd(res)
  yhat<-(yhat-mean(yhat))/sd(yhat)
  
  tibble(res, yhat) %>%
  #tibble(x, y) %>%  
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
  ggsave(filename = paste("heter_", i, ".png", sep = ""), height = 2, width = 2, dpi = 150)
  wt_conclusion <- white_test(res, yhat, n)
  return(wt_conclusion)
}


setwd("/volumes/5550/panda2/res&yhat")
accuracy_wtest_heter_train <- sapply(1:100,  heter) %>% sum()/100
accuracy_wtest_heter_train
setwd("/volumes/5550/panda2/x&y")
accuracy_wtest_heter_train <- sapply(1:100,  heter) %>% sum()/100
accuracy_wtest_heter_train

setwd("/Users/shuofanzhang/documents/monarch/train/norela")
accuracy_cortest_norela_train_part2 <- (sapply(50001:100000, norela) > 0.05) %>% mean()
accuracy_cortest_norela_train_part2

setwd("/Users/shuofanzhang/documents/monarch/test/linear")
accuracy_cortest_linear_test_part2 <- (sapply(20001:40000, linear) < 0.05) %>% mean()
accuracy_cortest_linear_test_part2
setwd("/Users/shuofanzhang/documents/monarch/test/norela")
accuracy_cortest_norela_test_part2 <- (sapply(20001:40000, norela) > 0.05) %>% mean()
accuracy_cortest_norela_test_part2

setwd("/Users/shuofanzhang/documents/monarch/validation/linear")
accuracy_cortest_linear_validation_part2 <- (sapply(20001:40000, linear) < 0.05) %>% mean()
accuracy_cortest_linear_validation_part2 
setwd("/Users/shuofanzhang/documents/monarch/validation/norela")
accuracy_cortest_norela_validation_part2 <- (sapply(20001:40000, norela) > 0.05) %>% mean()
accuracy_cortest_norela_validation_part2


######## image generating finish, start training ########### need to start from here ################## 8th May 2018 afternoon 

setwd("/Volumes/5550/panda")

base_dir <- "/Volumes/5550/panda"
train_dir <- file.path(base_dir,"train")
validation_dir <- file.path(base_dir,"validation")
test_dir <- file.path(base_dir,"test")

train_datagen <- image_data_generator(rescale = 1/255)             
validation_datagen <- image_data_generator(rescale = 1/255)  
test_datagen <- image_data_generator(rescale = 1/255)



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

############ training  ################

train_generator <- flow_images_from_directory(
  train_dir,                                                       
  train_datagen,                                                   
  target_size = c(150, 150), 
  color_mode = "grayscale",
  batch_size = 100,                                                 
  class_mode = "binary"
)

validation_generator <- flow_images_from_directory(
  validation_dir,
  validation_datagen,
  target_size = c(150, 150),
  color_mode = "grayscale",
  batch_size = 100,
  class_mode = "binary"
)

test_generator <- flow_images_from_directory(
  test_dir,
  test_datagen,
  target_size = c(150, 150),
  color_mode = "grayscale",
  batch_size = 100,
  class_mode = "binary"
)

history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 2000,
  epochs = 5,
  callbacks = callback_model_checkpoint("weights.{epoch:02d}-{val_loss:.2f}.hdf5"),
  validation_data = validation_generator,
  validation_steps = 800
)

plot(history)
##################################################another model fitting try###########
setwd("/Users/shuofanzhang/documents/monarch")
model59 <- keras_model_sequential() %>%
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

model59 %>% compile(
  optimizer = optimizer_rmsprop(lr = 1e-4),
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)
history59 <- model59 %>% fit_generator(
  train_generator,
  steps_per_epoch = 2000,
  epochs = 10,
  callbacks = callback_model_checkpoint("weights.{epoch:02d}-{val_loss:.2f}.hdf5"),
  validation_data = validation_generator,
  validation_steps = 800
)

model59 %>% save_model_hdf5("new_100k_each_59.h5")

################################################# evaluate convnets on test set##############################
dl_acc_test_hat <- model %>% evaluate_generator(test_generator, steps = 800)

################# evaluate convnets on turk experiment data (70 real plots only) ###### human=0.5714286

evaluate_turk70real <- flow_images_from_directory(
  "/Users/shuofanzhang/documents/monarch/turk70real",
  test_datagen,
  target_size = c(150, 150),
  color_mode = "grayscale",
  batch_size = 1,
  class_mode = "binary"
)
dl_acc_turk70real <- model %>% evaluate_generator(evaluate_turk70real, steps = 70)

############# predictions of 20 plots/70 lineups= dl_prediction_turk70lu ####### deep learning model choices = dl_choices ########
#
#file <- read.csv("/Users/shuofanzhang/documents/monarch/file.csv")
#dl_prediction_turk70lu <- list()
#dl_choices <- vector()
#for (i in 1:70) {
#  evaluate_dir <- paste0("/Users/shuofanzhang/documents/monarch/70lineup/", file$name[i])
#  turk_generator <- flow_images_from_directory(
#    evaluate_dir,
#    test_datagen,
#    target_size = c(150, 150),
#    color_mode = "grayscale",
#    batch_size = 1,
#    class_mode = "binary"
#  )      
#  dl_prediction_turk70lu[[i]] <- model %>% predict_generator(turk_generator, steps = 20)
#  dl_choices[i] <- dl_prediction_turk70lu[[i]] %>% which.max()
#}
#dl_choices
#dl_acc_test_hat
##############################################

############
setwd("/Volumes/5550/panda/10epoch_m")
model_10e <- load_model_hdf5("weights.06-0.22.hdf5")
dl10_acc_test_hat <- model_10e %>% evaluate_generator(test_generator, steps = 400)
dl10_acc_test_hat
########## power of the 10 epoch dl model on test set #####









