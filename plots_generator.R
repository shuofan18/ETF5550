library(tidyverse)


# 1/4 for classic linear model
classic<-function(n){
  
  beta<-runif(1,0.5,1)
  
  x<-rnorm(n, 0, 3)
  y<-rnorm(n, beta*x, 1)
  
  tibble(x, y)
  model<-lm(y ~ x)
  res<-residuals(model)
  yhat<-predict(model)
  res<-(res-mean(res))/sd(res)
  yhat<-(yhat-mean(yhat))/sd(yhat)
  return(tibble(res, yhat))
}

# 2/4 for linear model with heteroskedasticity 
heter<-function(n){
  
  x<-rnorm(n, 0, 3)
  beta<-runif(1,0.5,1)
  
  a <- runif(1,0,2)*rbinom(1,1,0.5)
  b <- runif(1,-4,4)
  c <- rnorm(1,0,2)
  
  variance <- 0.25*(a*x^2+b*x+c)+rnorm(n,0,0.25)
  
  index <- sample(0:1,1)
  if (index==1) {
    variance <- -variance
  }
  
  min <- min(variance)
  
  if (min<0){
    variance <- variance-min
  }
  
  max <- max(variance)
  while (max>5) {
    variance <- 0.8* variance
    max <- max(variance)
  }
  
  y<-rnorm(n, beta*x, variance)
  
  tibble(x, y)
  model<-lm(y ~ x)
  res<-residuals(model)
  yhat<-predict(model)
  res<-(res-mean(res))/sd(res)
  yhat<-(yhat-mean(yhat))/sd(yhat)
  return(tibble(res, yhat))
}


# 3/4 for non-linear model (polynomial) 
nonpoly<-function(n){
  
  p1<-rnorm(1,0,2)
  p2<-rnorm(1,0,2)
  p3<-rnorm(1,0,2)*rbinom(1,1,0.5)
  p4<-rnorm(1,0,2)*rbinom(1,1,0.3)
  p5<-rnorm(1,0,1)*rbinom(1,1,0.2)
  
  x<-rnorm(n, 0, 3)
  y<-rnorm(n, p1*x+p2*x^2+p3*x^3+p4*x^4+p5*x^5, p1^2+2*p2^2+2*p3^2+2*p4^2+2*p5^2)
  
  tibble(x, y)
  model<-lm(y ~ x)
  res<-residuals(model)
  yhat<-predict(model)
  res<-(res-mean(res))/sd(res)
  yhat<-(yhat-mean(yhat))/sd(yhat)
  return(tibble(res, yhat))
}

#nonpoly(200) %>% 
  #ggplot(aes(x=yhat,y=res))+geom_point()

#generating data from three models
N <- sample(20:1500, 19, replace = TRUE)
  classic_ <- tibble(N) %>%
  mutate(res = map(N, classic))
  classic_<-classic_$res

  heter_ <- tibble(N) %>%
  mutate(res = map(N, heter))
  heter_<-heter_$res

  poly_ <- tibble(N) %>%
  mutate(res = map(N, nonpoly))
  poly_<-poly_$res

names(classic_) <- c(1:19)
names(heter_) <- c(1:100)
names(poly_) <- c(1:100)

savepic <- function(pdata, i){ 
  ggplot(as.data.frame(pdata), aes(x = yhat, y=res, alpha = 0.01)) + 
    geom_point() + 
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
          plot.background=element_blank())
  ggsave(filename = paste(name, i, ".png", sep = ""), height = 2, width = 2, dpi = 150)
}

#saving all plots to the original data folder

setwd("/Volumes/5550/original data")

name <- deparse(substitute(classic_))
mapply(savepic, classic_, c(1:19))

name <- deparse(substitute(heter_))
mapply(savepic, heter_, c(1:100))

name <- deparse(substitute(poly_))
mapply(savepic, poly_, c(1:100))

#separate plots to train, test, validation

original_dataset_dir <- "/Volumes/5550/original data"
base_dir <- "/Volumes/5550/thesis"
train_dir <- file.path(base_dir,"train")
validation_dir <- file.path(base_dir,"validation")
test_dir <- file.path(base_dir,"test")
train_classic_dir <- file.path(train_dir, "classic")
train_heter_dir <- file.path(train_dir, "heter")
train_poly_dir <- file.path(train_dir, "poly")
validation_classic_dir <- file.path(validation_dir, "classic")
validation_heter_dir <- file.path(validation_dir, "heter")
validation_poly_dir <- file.path(validation_dir, "poly")
test_classic_dir <- file.path(test_dir, "classic")
test_heter_dir <- file.path(test_dir, "heter")
test_poly_dir <- file.path(test_dir, "poly")


dir.create(train_dir)
dir.create(validation_dir)
dir.create(test_dir)
dir.create(train_classic_dir)
dir.create(train_heter_dir)
dir.create(train_poly_dir)
dir.create(validation_classic_dir)
dir.create(validation_heter_dir)
dir.create(validation_poly_dir)
dir.create(test_classic_dir)
dir.create(test_heter_dir)
dir.create(test_poly_dir)



#copy images from original folder to separate train, test, validation folders
fnames <- paste0("classic_",1:300,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(train_classic_dir))
fnames <- paste0("classic_",1:100,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(validation_classic_dir))
fnames <- paste0("classic_",1:200,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(test_classic_dir))

fnames <- paste0("heter_",1:300,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(train_heter_dir))
fnames <- paste0("heter_",1:100,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(validation_heter_dir))
fnames <- paste0("heter_",1:200,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(test_heter_dir))

fnames <- paste0("poly_",1:300,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(train_poly_dir))
fnames <- paste0("poly_",1:100,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(validation_poly_dir))
fnames <- paste0("poly_",1:200,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(test_poly_dir))


#################### experiments

test_lineup <- file.path(test_dir,"lineup")
test_lineup_null <- file.path(test_lineup, "classic")
test_lineup_heter <- file.path(test_lineup, "heter")
test_lineup_poly <- file.path(test_lineup, "poly")

dir.create(test_lineup)
dir.create(test_lineup_null)
dir.create(test_lineup_heter)
dir.create(test_lineup_poly)



N <- rep(3000, each=300)
classic_ <- tibble(N) %>%
  mutate(res = map(N, classic))
classic_<-classic_$res

names(classic_) <- c(1:300)
name <- deparse(substitute(classic_))
mapply(savepic, classic_, c(1:300))


fnames <- paste0("classic_",1:300,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(test_lineup_null))

N <- rep(3000, each=300)
heter_ <- tibble(N) %>%
  mutate(res = map(N, heter))
heter_<-heter_$res

names(heter_) <- c(1:300)
name <- deparse(substitute(heter_))
mapply(savepic, heter_, c(1:300))


fnames <- paste0("heter_",1:300,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(test_lineup_heter))










