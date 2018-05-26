pvalue_turk2 <- read.csv("/volumes/5550/turk2_results/pvalue_turk2.csv")
pvalue_real <- read.csv("/volumes/5550/turk2_results/real_pvalue_cor.csv")

acc <- ((pvalue_real$ct_real_pv < 0.05)) %>% mean()
acc <- (acc*70+3)/70
acc
