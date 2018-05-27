
########### get prediction for each real pic by PC ######### finished ###### results saved in csv 

library(keras)
heter11model <- load_model_hdf5("~/documents/github/etf5550/heter&linear/checkpoints/weights.11-0.08.hdf5")
test_datagen <- image_data_generator(rescale = 1/255)
lineup_test <- flow_images_from_directory(
  "~/documents/github/etf5550/heter&linear/000_only_real_plots",
  test_datagen,
  target_size = c(150, 150),
  color_mode = "grayscale",
  batch_size = 1,
  class_mode = "binary"
)
prediction <- heter11model %>% predict_generator(lineup_test, steps = 1)
prediction 

################## make graph 
library(reshape2)
survey <- read.csv("~/documents/github/etf5550/heter&linear/survey_ans_nonull.csv")
survey <- survey %>% mutate(wtpvalue=1-pchisq(wt.stat,2))
graphdata <- survey %>% select(form, question, pval, pcpvalue, wtpvalue, a) %>% 
  mutate(id=1:27)
graphdata <- graphdata %>% melt(id=c("form","question","id"))
#graphdata <- graphdata %>% filter(variable!="pval")
ggplot(graphdata, aes(x=id, y=value, group=variable, color=variable))+
  geom_point()+
  geom_line()+
  facet_wrap(~variable, nrow = 3)




