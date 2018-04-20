data <- read.csv("\\\\ad.monash.edu/home/User073/szha0004/Desktop/file.csv")


power_t <- vector()

for (i in 1:70) {
  
  n=data$n[i]
  d=data$e[i]
  sd=data$sd[i]
  
  pp<-power.t.test(n=n, d=d, sd=sd, 
                           sig.level = 0.05, alternative = "two.sided")
  
  power_t[i] <- pp$power
}
