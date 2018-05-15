
setwd("/volumes/5550/turk2_results")

# the most common choices by human for each lineup = human_choices
# the actual location of the real plot in each lineup = newdata$plot_location


library(tidyverse)
data <- read.csv("/volumes/5550/turk2_results/data_turk2.csv")
file <- read.csv("/volumes/5550/turk2_results/file.csv")
ntrue <- data %>% group_by(pic_name) %>% summarise(ntrue = sum(response))
count <- table(data$pic_name) %>% data.frame() 
#png_name <- as.character(file$png_nolc)
#human_choices <- vector()
#for (i in 1:70) {
#  datai <- subset(data, pic_name == png_name[i])
#  human_choices[i] <- names(which.max(table(datai$response_no)))
#}

ntrue <- ntrue %>% 
  left_join(select(data, difficulty, pic_name, 
                   sample_size, beta, plot_location, sigma, replica)) %>% 
  distinct() %>% 
  left_join(count, by = c("pic_name"="Var1"))

newdata <- as.data.frame(ntrue)


p_cal <- function(k, x){
  prob <- 1-pbinom(x-1, k, 0.05)
  return(prob)
}

p_value <- vector()
for (i in 1:70) {
  ki <- newdata$Freq[i]
  xi <- newdata$ntrue[i]
  p_value[i] <- p_cal(k=ki, x=xi)
}

newdata$p_value <- p_value
pic_name <- newdata$pic_name
setwd("/volumes/5550/turk2_results")
write.csv(tibble(pic_name, p_value), "human_pvalue.csv")
newdata$conclusion <- (newdata$p_value < 0.05)
#newdata$one_choice_con <- (newdata$plot_location == newdata$human_choices)
newdata$conclusion[38] <- TRUE
newdata$conclusion[39] <- TRUE
newdata$conclusion[40] <- TRUE

acc_human <- sum(newdata$conclusion)/70

acc_human_one_choice <- sum(newdata$one_choice_con)/70

newdata$human_choices <- human_choices

difficulty_true <- newdata %>% group_by(difficulty) %>% summarise(pb_true = mean(conclusion))

############################ smallest p picked by cor.test ################
setwd("/Volumes/5550/turk2_txt_files")
turk_lu <- list()
ctest_choices <- vector()
ct_real_pv <- vector()
for (i in 1:70) {
  turk_lu[[i]] <- as.character(file$txt[i]) %>% read.delim(sep = " ") 
  ct_pvalue <- vector()
  for (index in 1:20){
    X <- turk_lu[[i]][,1]
    Y <- turk_lu[[i]][,index+1]
    ct_pvalue[index] <- cor.test(X,Y)$p.value
  }
  ct_real_pv[i] <- ct_pvalue[file$location[i]]
  ctest_choices[i] <- ct_pvalue %>% which.min()
}

newdata <- newdata %>% mutate(ctest_choices)
newdata$same_choice_human_ct <- (human_choices==ctest_choices)
sum(newdata$same_choice_human_ct)/70
newdata$right_choices_ct <- (newdata$ctest_choices==newdata$plot_location)
sum(newdata$right_choices_ct)/70

realplot_right_ct <- (ct_real_pv < 0.05)
(sum(realplot_right_ct)+3)/70

########################### 5 epoch dl model
library(keras)
setwd("/Volumes/5550/panda/5epoch_m")
model_5e <- load_model_hdf5("new_100k_each.h5")

########################### 10 epoch dl model ###### the best one is the 4th epoch
setwd("/Volumes/5550/panda/10epoch_m")
library(keras)
model_10e <- load_model_hdf5("weights.06-0.22.hdf5")

base_dir <- "/Volumes/5550/panda"
turk_dir <- file.path(base_dir,"turk70real")

test_datagen <- image_data_generator(rescale = 1/255)
############################################################# crazy test
turk_generator <- flow_images_from_directory(
  turk_dir,
  test_datagen,
  target_size = c(150, 150),
  color_mode = "grayscale",
  batch_size = 1,
  class_mode = "binary"
)
#orders1 <- turk_generator$filenames
#p1 <- model_10e %>% predict_generator(turk_generator, steps = 1)

#pc_predict <- model_10e %>%
#  predict_generator(turk_generator, step = 70, verbose = 1)

# (Eye-ball check the prediction)
# (Essentially predict_proba is the probability of the image being norela)
#stat_df <- as.tibble(cbind(pc_predict, turk_generator$filenames, turk_generator$classes)) %>%
#  rename(
#    predict_proba = V1,
#    filename = V2,
#    test_label = V3
#  ) %>%
#  mutate(predicted_label = ifelse(predict_proba > 0.5, 1, 0)) %>%
  # sample_n(size= 20) %>% 
#  mutate(predicted_label = as.integer(predicted_label)) %>%
#  mutate(predicted_label_name = ifelse(predicted_label == 0, "cats", "dogs")) %>%
#  separate(filename, into=c("true_label","fname"), sep = "[//]" )

#dl10_acc_turk167 <- model_10e %>% evaluate_generator(turk_generator, steps = 67)




