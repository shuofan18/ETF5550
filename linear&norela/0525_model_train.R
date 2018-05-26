
######## image generating finish, start training ########### 0523 start from here ################## 8th May 2018 afternoon
library(keras)
library(tidyverse)

setwd("/Users/shuofanzhang/documents/linear&norela")

base_dir <- "~/documents/linear&norela"
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

history2 <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 2000,
  epochs = 5,
  callbacks = callback_model_checkpoint("weights.{epoch:02d}-{val_loss:.2f}.hdf5"),
  validation_data = validation_generator,
  validation_steps = 800
)

#history12 <- rbind(as.data.frame(history), as.data.frame(history2))
#history12$epoch[21:40] <- rep(6:10, 4)

history12 <- read.csv("~/documents/etf5550/linear&norela/history_10_epoch.csv")

linear_history_plot <- ggplot(history12, aes(x=epoch, y=value, group=data, color=data))+
  geom_point()+
  geom_smooth(level=0)+
  facet_wrap(~metric, nrow = 2, strip.position = "left", scales = "free_y") 

setwd("~/documents/etf5550/monashthesis-master/figures")
ggsave(linear_history_plot, filename = "linear_history_plot.png")


################################################# evaluate convnets on test set##############################

dlacc_test_hat_linear_norela <- model %>% evaluate_generator(test_generator, steps = 2000)

linear10alpha <- model %>% evaluate_generator(test_generator, steps=1000)  ##### remember to re-do test-generator

linear1model <- load_model_hdf5("~/documents/etf5550/linear&norela/checkpoints/weights.01-0.19.hdf5")
linear2model <- load_model_hdf5("~/documents/etf5550/linear&norela/checkpoints/weights.02-0.18.hdf5")
linear3model <- load_model_hdf5("~/documents/etf5550/linear&norela/checkpoints/weights.03-0.18.hdf5")
linear4model <- load_model_hdf5("~/documents/etf5550/linear&norela/checkpoints/weights.04-0.18.hdf5")
linear5model <- load_model_hdf5("~/documents/etf5550/linear&norela/checkpoints/weights.05-0.19.hdf5")
linear6model <- load_model_hdf5("~/documents/etf5550/linear&norela/checkpoints/weights.06-0.18.hdf5")
linear7model <- load_model_hdf5("~/documents/etf5550/linear&norela/checkpoints/weights.07-0.18.hdf5")
linear8model <- load_model_hdf5("~/documents/etf5550/linear&norela/checkpoints/weights.08-0.18.hdf5")
linear9model <- load_model_hdf5("~/documents/etf5550/linear&norela/checkpoints/weights.09-0.18.hdf5")
linear10model <- load_model_hdf5("~/documents/etf5550/linear&norela/checkpoints/weights.10-0.18.hdf5")


linear1alpha <- linear1model %>% evaluate_generator(test_generator, steps=1000)
linear2alpha <- linear2model %>% evaluate_generator(test_generator, steps=1000)
linear3alpha <- linear3model %>% evaluate_generator(test_generator, steps=1000)
linear5alpha <- linear5model %>% evaluate_generator(test_generator, steps=1000)
linear7alpha <- linear7model %>% evaluate_generator(test_generator, steps=1000)
linear9alpha <- linear9model %>% evaluate_generator(test_generator, steps=1000)

linear1power <- linear1model %>% evaluate_generator(test_generator, steps=1000)
linear1power
linear2power <- linear2model %>% evaluate_generator(test_generator, steps=1000)
linear2power
linear3power <- linear3model %>% evaluate_generator(test_generator, steps=1000)
linear3power
linear5power <- linear5model %>% evaluate_generator(test_generator, steps=1000)
linear5power
linear7power <- linear7model %>% evaluate_generator(test_generator, steps=1000)
linear7power
linear9power <- linear9model %>% evaluate_generator(test_generator, steps=1000)
linear9power

linear4acc <- linear4model %>% evaluate_generator(test_generator, steps=1000)  ##### remember to re-do test-generator
linear6alpha <- linear6model %>% evaluate_generator(test_generator, steps=1000)  ##### remember to re-do test-generator

###################################################################################
#alphas <- c(linear1alpha$acc, linear2alpha$acc, linear3alpha$acc, linear4alpha$acc, 
#            linear5alpha$acc, linear6alpha, linear7alpha$acc, linear8alpha, 
#            linear9alpha$acc, linear10alpha)

#powers <- c(linear1power$acc, linear2power$acc, linear3power$acc, linear4power$acc, 
#            linear5power$acc, linear6power, linear7power$acc, linear8power, 
#            linear9power$acc, linear10power)
#epoches <- 1:10
#eap <- tibble(epoches, alphas, powers) %>% mutate(accuracy=(alphas+powers)/2)
#write.csv(eap, file = "~/documents/etf5550/linear&norela/test_acc_10epoches.csv")
################# evaluate convnets on turk data (70 real plots only) ###### 0523 up to here

evaluate_turk70real <- flow_images_from_directory(
  "~/documents/linear&norela/evaluate_turk_70real",
  test_datagen,
  target_size = c(150, 150),
  color_mode = "grayscale",
  batch_size = 1,
  class_mode = "binary"
)
dl_acc_turk70real <- linear10model %>% evaluate_generator(evaluate_turk70real, steps = 70)

######### t test accuracy #######
ttest_power <- read.csv("~/documents/linear&norela/parameters/parameters3.csv")
truet005 <- (ttest_power$ct.p.value<0.05) %>% mean()
ttest_alpha <- read.csv("~/documents/linear&norela/parameters/parameters4.csv")
truet005a <- (ttest_alpha$ct.p.value>0.05) %>% mean()

ruet002 <- (ttest_power$ct.p.value<0.02) %>% mean()
truet002a <- (ttest_alpha$ct.p.value>0.02) %>% mean()

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









