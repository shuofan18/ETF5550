setwd("/Volumes/5550/turk2")

file <- read.csv("file.csv")
txt <- vector()
name <- vector()
no_i <- vector()
png_i <- vector()
txt <- as.character(file$txt)
name <- as.character(file$name)
no_i <- as.character(file$no_i)
png_i <- as.character(file$png)


########## create 70 folders in thesis folder

base_dir <- "/Volumes/5550/thesis"

compare_dir <- vector()
for (i in 1:70) {
  name_i <- paste(name[i])
  compare_dir[i] <- file.path(base_dir, name[i])
  dir.create(compare_dir[i])
}

########## save 20 images to each folder

savepic2 <- function(i){ 
  
  setwd("/Volumes/5550/turk2")
  txt_i <- txt[i]
  pdata <- read.delim(txt_i, header = TRUE, sep = "", dec = ".")
  
  setwd(compare_dir[i])
  for (index in 1:20){
  
    x <- pdata[ , 1]
    y <- pdata[ , index+1]
    tibble(x,y) %>%  
      ggplot(aes(x = x, y = y)) + 
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
    ggsave(filename = paste(no_i[i], index, ".png", sep = ""), height = 2, width = 2, dpi = 150)
  }
  }
  
mapply(savepic2, 1:70)

########### select 'real plot' to a separate folder "evaluate_turk"

test_turk_dir <- "/Volumes/5550/evaluate_turk"

slt <- function(i) {
  
    fnames <- paste0(no_i[i],1:20,".png")
  
  file.copy(file.path(compare_dir[i],fnames),
            file.path(test_turk_dir))
}

mapply(slt, 1:70)

test_turk_dir_linear <- "/Volumes/5550/evaluate_turk/linear"
dir.create(test_turk_dir_linear)
test_turk_dir_norela <- "/Volumes/5550/evaluate_turk/norela"
dir.create(test_turk_dir_norela)

realslt <- function(i) {
  fnames <- paste0(png_i[i])
  
  file.copy(file.path(test_turk_dir, fnames),
            file.path(test_turk_dir_linear))
}

mapply(realslt, 1:70)

######### get prediction for each pic

library(keras)

test_datagen <- image_data_generator(rescale = 1/255)

test_generator_compare <- flow_images_from_directory(
  compare_dir[1],
  test_datagen,
  target_size = c(150, 150),
  color_mode = "grayscale",
  batch_size = 20,
  class_mode = "binary"
)

model %>% predict_generator(test_generator_compare, steps = 1)

######## evaluate only on real data (without lineup)   0.4142 acc 

test_datagen <- image_data_generator(rescale = 1/255)
test_turk_dir <- "/Volumes/5550/evaluate_turk"

test_generator_turk <- flow_images_from_directory(
  test_turk_dir,
  test_datagen,
  target_size = c(150, 150),
  color_mode = "grayscale",
  batch_size = 70,
  class_mode = "binary"
)


acc_wo_lineup <- model %>% evaluate_generator(test_generator_turk, steps = 1)
pred_wo_lineup <- model %>% predict_generator(test_generator, steps = 1)

stat_df <- as.tibble(cbind(pred_wo_lineup, test_generator$filenames, test_generator$classes)) %>%
  rename(
    predict_proba = V1,
    filename = V2,
    test_label = V3
  ) %>%
  mutate(predicted_label = ifelse(predict_proba > 0.5, 1, 0)) %>%
  mutate(predicted_label = as.integer(predicted_label)) %>%
  mutate(predicted_label_name = ifelse(predicted_label == 0, "norela", "linear")) %>%
  separate(filename, into=c("true_label","fname"), sep = "[//]" )
stat_df

write.table(stat_df, "stat_df.txt", sep="\t")

############# evaluate on real data with lineup
############ create "linear" and "norela" for each folder

a_lot_of_dir <- vector()

for (i in 1:70) {
  nam <- paste0("/Volumes/5550/thesis/", name[i])
  linear_dir <- file.path(nam, "linear")
  norela_dir <- file.path(nam, "norela")
  dir.create(linear_dir)
  dir.create(norela_dir)
}

########## copy the real plot into "linear" and the rest to "norela"
for (i in 1:70) {
  
  nam <- paste0("/Volumes/5550/thesis/", name[i])
  linear_dir <- file.path(nam, "linear")
  norela_dir <- file.path(nam, "norela")
  
fnames <- paste0(png_i[i])

file.copy(file.path(nam, fnames),
          file.path(linear_dir))

det <- paste0(nam, "/", fnames, sep="")
unlink(det)

  nana <- paste0(no_i[i], 1:20, ".png", sep="")  
  nana <- nana [! nana %in% png_i[i]]
  
  file.copy(file.path(nam, nana),
            file.path(norela_dir))
det2 <- paste0(nam, "/", nana, sep="")
unlink(det2)
}

############# evaluate 70 sets of 20 plots one by one, nnnn has the max predicted values 
#(to be linear)

test_datagen <- image_data_generator(rescale = 1/255)
pred_max <- vector()
nnnn <- vector()
acc <- vector()
for (i in 1:70) {
  nam <- paste0("/Volumes/5550/thesis/", name[i])
  
  test_generator_i <- flow_images_from_directory(
    nam,
    test_datagen,
    target_size = c(150, 150),
    color_mode = "grayscale",
    batch_size = 20,
    class_mode = "binary"
  ) 
  pred_max[i] <- model %>% predict_generator(test_generator_i, steps = 1) %>% which.max() 
  ll <- pred_max[i]
  nnnn[i] <- test_generator_i$filenames[ll]
  acc[i] <- model %>% evaluate_generator(test_generator_i, steps = 1)
}




















