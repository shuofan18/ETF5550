setwd("/Volumes/5550/turk2")

file <- read.csv("file.csv")
txt <- vector()
name <- vector()
txt <- as.character(file$txt)
name <- as.character(file$name)

########## read 70 txt files
readata <- function(i) {
  txt_i <- txt[i]
  name_i <- paste(name[i])
  data_i <- read.delim(txt_i, header = TRUE, sep = "", dec = ".")
  assign(name_i, data_i)
  return(name_i)
}

mapply(readata, 1:70)

########## create 70 folders in test_dir

base_dir <- "/Volumes/5550/thesis"

compare_dir <- vector()
for (i in 1:70) {
  name_i <- paste(name[i])
  compare_dir[i] <- file.path(base_dir, name_i)
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
    ggsave(filename = paste(index, ".png", sep = ""), height = 2, width = 2, dpi = 150)
  }
  }
  
mapply(savepic2, 1:70)

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











