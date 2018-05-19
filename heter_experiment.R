
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
    
ggplot(lineup_data, aes(x, .std.resid))+geom_point(alpha = 0.4)+
  theme(axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.ticks=element_blank(),
      aspect.ratio = 1) +facet_wrap(~.sample, ncol=5) 

ggsave(filename = 
         paste(i, "_real(", pos,")a(" , round(a,digits = 2), ")n(", n,
               ")wtst(", round(wt_stat, digits = 2) ,")wt(" , wt_conclusion,").png", 
               sep = ""), height = 8, width = 10, dpi = 200)
  return(wt_conclusion)
}

set.seed(0517)
setwd("/volumes/5550/panda2/lineup")
accuracy_wtest_heter_train <- sapply(1:10, heter) %>% sum()/10
accuracy_wtest_heter_train

############################## generate data for training ######################

heter_train <- function(i){
  
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
  train_data <- fit %>% select(x, .std.resid)
  ggplot(train_data, aes(x, .std.resid))+geom_point(alpha = 0.4)+
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
  ggsave(filename = 
           paste(i, "_wtst(", round(wt_stat, digits = 2) ,")_wt(" , wt_conclusion,").png", 
                 sep = ""), height = 2, width = 2, dpi = 150)
  return(wt_conclusion)
}

##########  generate images of linear for training

linear_train <- function(i){
  
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
  #### generate images of "linear"
  train_data <- fit %>% select(x, .std.resid)
  ggplot(train_data, aes(x, .std.resid))+geom_point(alpha = 0.4)+
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
  ggsave(filename = 
           paste(i, "_wtst(", round(wt_stat, digits = 2) ,")_wt(" , wt_conclusion,").png", 
                 sep = ""), height = 2, width = 2, dpi = 150)
  return(wt_conclusion)
}

###### save heter images to train folder
setwd("/Users/shuofanzhang/documents/heter&linear/train/heter")
accuracy_wtest_heter_train <- sapply(1:100000, heter_train) %>% mean()
accuracy_wtest_heter_train
###### save heter images to test folder
setwd("/Users/shuofanzhang/documents/heter&linear/test/heter")
accuracy_wtest_heter_test <- sapply(1:40000, heter_train) %>% mean()
accuracy_wtest_heter_test
###### save heter images to validation folder
setwd("/Users/shuofanzhang/documents/heter&linear/validation/heter")
accuracy_wtest_heter_validation <- sapply(1:40000, heter_train) %>% mean()
accuracy_wtest_heter_validation
###### save linear images to train folder
setwd("/Users/shuofanzhang/documents/heter&linear/train/linear")
accuracy_wtest_linear_train <- sapply(1:100000, linear_train) %>% mean()
accuracy_wtest_linear_train
###### save linear images to test folder
setwd("/Users/shuofanzhang/documents/heter&linear/test/heter")
accuracy_wtest_linear_test <- sapply(1:40000, linear_train) %>% mean()
accuracy_wtest_linear_test
###### save linear images to validation folder
setwd("/Users/shuofanzhang/documents/heter&linear/validation/heter")
accuracy_wtest_linear_validation <- sapply(1:40000, linear_train) %>% mean()
accuracy_wtest_linear_validation

######## image generating finish, start training ########### need to start from here ################## 8th May 2018 afternoon 

setwd("/Users/shuofanzhang/documents/heter&linear")

base_dir <- "/Users/shuofanzhang/documents/heter&linear"
train_dir <- file.path(base_dir,"train")
validation_dir <- file.path(base_dir,"validation")
test_dir <- file.path(base_dir,"test")

train_datagen <- image_data_generator(rescale = 1/255)             
validation_datagen <- image_data_generator(rescale = 1/255)  
test_datagen <- image_data_generator(rescale = 1/255)

############ define deep learning model ################

model_heter <- keras_model_sequential() %>%
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

history_heter <- model_heter %>% fit_generator(
  train_generator,
  steps_per_epoch = 2000,
  epochs = 10,
  callbacks = callback_model_checkpoint("weights.{epoch:02d}-{val_loss:.2f}.hdf5"),
  validation_data = validation_generator,
  validation_steps = 800
)

plot(history_heter)

################################################# evaluate convnets on test set##############################
dl_acc_test_hat <- model %>% evaluate_generator(test_generator, steps = 800)

################# evaluate convnets on new experiment data (29 real plots only) ###### human=0.5714286

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









