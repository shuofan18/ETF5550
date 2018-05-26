library(tidyverse)
heter_history_plot <- read.csv("~/documents/etf5550/heter&linear/history_15epoch.csv")

heter_history_plot <- ggplot(heter_history_plot, aes(x=epoch, y=value, group=data, color=data))+
  geom_point()+
  geom_smooth(level=0)+
  facet_wrap(~metric, nrow = 2, strip.position = "left", scales = "free_y")+
  scale_color_brewer(palette = "Dark2")

heter_history_plot

setwd("~/documents/etf5550/heter&linear")
ggsave(heter_history_plot, filename = "heter_history_plot_2.png")
