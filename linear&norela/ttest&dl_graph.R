#tt <- read.csv("~/documents/etf5550/linear&norela/ttest_test_set.csv")
tt <- read.csv("~/documents/github/etf5550/linear&norela/ttest_test_set.csv")
tt <- select(tt, alpha, power, accuracy)
tt <- filter(tt, alpha<0.04)
tt <- filter(tt, alpha>0.01)
colnames(tt) <- c("alpha", "power", "accuracy")
tt <- tt %>% mutate(model="T-test")


#dl <- read.csv("~/documents/etf5550/linear&norela/test_acc_10epoches.csv")
dl <- read.csv("~/documents/github/etf5550/linear&norela/test_acc_10epoches.csv")
dl <- dl %>% mutate(alpha=1-alphas)
dl <- select(dl, alpha, powers, accuracy) 
colnames(dl) <- c("alpha", "power", "accuracy")
dl <- dl %>% mutate(model="Deep learning model")

tt_dl <- rbind(tt,dl)

library(reshape2)
tt_dl <- tt_dl %>% melt(id=c("model","alpha"))

ttdl <- ggplot(tt_dl, aes(x=alpha, y=value, group=variable, color=model))+
  geom_point(size=4)+
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE)
setwd("/Users/stanza/Documents/GitHub/ETF5550")

#ggsave(ttdl, filename = "tt_dl_graph.png")







