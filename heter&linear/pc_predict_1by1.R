
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

################## make graph of pvalues from human, computer, and white test
library(reshape2)
survey <- read.csv("~/documents/github/etf5550/heter&linear/survey_ans_nonull.csv")
survey <- survey %>% mutate(wtpvalue=1-pchisq(wt.stat,2))
graphdata <- survey %>% select(form, question, pval, pcpvalue, wtpvalue, a) %>% 
  mutate(id=1:27)
colnames(graphdata) <- c("form", "question", "Human p.values",
                         "Computer prediction of Null", "White-test p.values",
                         "a", "ID")

graphdata <- graphdata %>% melt(id=c("form","question","ID","a"))

ggplot(graphdata, aes(x=ID, y=value, group=variable, color=variable))+
  geom_point()+
  geom_line()+
  facet_wrap(~variable, nrow = 3)+
  xlab("Lineup Questions")
 # scale_x_discrete(name="Lineup Questions", breaks=1:27, 
  #                 labels=c("1","2","3","4","5","6","7","8",
   #                         "9","10","11","12","13","14","15",
    #                        "16","17","18","19","20","21","22",
     #                       "23","24","25","26","27"))
#ggsave(filename = "~/documents/github/etf5550/heter&linear/graph_pvalues_3tests.png")



