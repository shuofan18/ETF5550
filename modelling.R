library(keras)
library(tidyverse)

setwd("/Volumes/5550/thesis")
base_dir <- "/Volumes/5550/thesis"
train_dir <- file.path(base_dir,"train")
validation_dir <- file.path(base_dir,"validation")
test_dir <- file.path(base_dir,"test")

# read plots data from folders

train_datagen <- image_data_generator(rescale = 1/255)             
validation_datagen <- image_data_generator(rescale = 1/255)  
test_datagen <- image_data_generator(rescale = 1/255)


train_generator <- flow_images_from_directory(
  train_dir,                                                       
  train_datagen,                                                   
  target_size = c(150, 150), 
  color_mode = "grayscale",
  batch_size = 200,                                                 
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




# define deep learning model 
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

##### changed steps_per_epoch from 100 to 50

history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 50,
  epochs = 30,
  validation_data = validation_generator,
  validation_steps = 50
)

############################################################ 7:36pm




model %>% evaluate_generator(test_generator, steps = 30)


####### test for one image

test_datagen_lineup <- image_data_generator(rescale = 1/255)

test_generator_lineup <- flow_images_from_directory(
  test_lineup,
  test_datagen_lineup,
  target_size = c(150, 150),
  color_mode = "grayscale",
  batch_size = 20,
  class_mode = "categorical"
)

model %>% evaluate_generator(test_generator_lineup, steps=1)












