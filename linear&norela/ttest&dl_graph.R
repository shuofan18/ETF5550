
tt <- read.csv("~/documents/etf5550/linear&norela/ttest_test_set.csv")
tt <- select(tt, alpha, power, accuracy)
colnames(tt) <- c("alpha", "power", "accuracy")
tt <- tt %>% mutate(model="t-test")


dl <- read.csv("~/documents/etf5550/linear&norela/test_acc_10epoches.csv")
dl <- dl %>% mutate(alpha=1-alphas)
dl <- select(dl, alpha, powers, accuracy) 
dl <- filter(dl, alpha<0.05)
colnames(dl) <- c("alpha", "power", "accuracy")
dl <- dl %>% mutate(model="deep learning model")

tt_dl <- rbind(tt,dl)

library(reshape2)
tt_dl <- tt_dl %>% melt(id=c("model","alpha"))

ggplot(tt_dl, aes(x=alpha, y=value, group=variable, color=variable))+
  geom_point()+
  facet_wrap(~model, nrow = 2)
