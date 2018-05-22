
library(tidyverse)
library(keras)

parameters1 <- data.frame()  #train/linear
parameters2 <- data.frame() #train/norela
parameters3 <- data.frame() #test/linear
parameters4 <- data.frame() #test/norela
parameters5 <- data.frame() #validation/linear
parameters6 <- data.frame() #train/norela


linear <- function(i){
  n <- sample(50:500, 1)
  beta <- runif(1, min=0.1, max=10)*(-1)^(rbinom(1,1,prob = 0.5))
  sigma <- runif(1, min=1, max=12)
  x<-rnorm(n, 0, 1)
  y<-rnorm(n, beta*x, sigma)
  ct <- cor.test(x, y)
  xy <- tibble(x, y)
  #write.csv(xy, file = paste0(i, "_n(", n, ")_beta(", beta, ")_sigma(", sigma, ")_pv(", round(ct$p.value, digits=2), ").csv"))
  parameters <- tibble(n,beta,sigma,ct$p.value)
  return(parameters)
}

norela<-function(i){
  n <- sample(50:500, 1)
  beta <- 0
  sigma <- runif(1, min=1, max=12)
  x<-rnorm(n, 0, 1)
  y<-rnorm(n, beta*x, sigma)
  ct <- cor.test(x, y)
  xy <- tibble(x, y)
  #write.csv(xy, file = paste0(i, "_n(", n, ")_beta(", beta, ")_sigma(", sigma, ")_pv(", round(ct$p.value, digits=2), ").csv"))
  parameters <- tibble(n,beta,sigma,ct$p.value)
  return(parameters)
}

set.seed(0521)
setwd("/Users/shuofanzhang/documents/linear&norela/csv/train/linear")
for(i in 1:100000) {
  parameters1 <- parameters1 %>% rbind(linear(i))                      
}

setwd("/Users/shuofanzhang/documents/linear&norela/csv/test/linear")
for(i in 1:40000) {
  parameters3 <- parameters3 %>% rbind(linear(i))
}

setwd("/Users/shuofanzhang/documents/linear&norela/csv/validation/linear")
for(i in 1:40000) {
  parameters5 <- parameters5 %>% rbind(linear(i))
}
                                                      
setwd("/Users/shuofanzhang/documents/linear&norela/csv/train/norela")
for(i in 1:100000) {
  parameters2 <- parameters2 %>% rbind(norela(i))
}

setwd("/Users/shuofanzhang/documents/linear&norela/csv/test/norela")
for(i in 1:40000) {
  parameters4 <- parameters4 %>% rbind(norela(i))
}
setwd("/Users/shuofanzhang/documents/linear&norela/csv/validation/norela")
for(i in 1:40000) {
  parameters6 <- parameters6 %>% rbind(norela(i))
}

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









