library(tidyverse)

#test for changes
# 1/4 for classic linear model
classic<-function(n){
  
  beta<-runif(1,0.5,1.5)
  
  x<-runif(n, -2, 2)
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
  lambda <- runif(1,-1,1)
  beta<-runif(1,0.5,1.5)
  a <- abs(lambda)
  b <- rnorm(1)
  c <- rnorm(1)
  
  x<-runif(n, -2, 2)
  m <- mean(x)
  
  index <- sample(c(1:3),1)
  if (index==1){
    y <- rnorm(n, beta*x, exp(x*lambda)+rexp(n))
  } else if (index==2) {
    y <- rnorm(n, beta*x, a*x^2+b*x+c+abs(c-b^2/(2*a))+rexp(n))
  } else {
    y <- rnorm(n, beta*x, dnorm(x, mean = m)+rexp(n))
  }
  
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
  
  p1<-rnorm(1,0,1)
  p2<-rnorm(1,0,1)
  p3<-rnorm(1,0,1)
  p4<-rnorm(1,0,1)*rbinom(1,1,0.5)
  p5<-rnorm(1,0,1)*rbinom(1,1,0.5)
  
  x<-runif(n,-2,2)
  y<-rnorm(n, p1*x+p2*x^2+p3*x^3+p4*x^4+p5*x^5, p1^2+p2^2+2*p3^2+2*p4^2+2*p5^2)
  
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
N <- sample(20:800, 300, replace = TRUE)
  classic_ <- tibble(N) %>%
  mutate(res = map(N, classic))
  classic_<-classic_$res
NH <- sample(20:800, 300, replace = TRUE)
  heter_ <- tibble(NH) %>%
  mutate(res = map(NH, heter))
  heter_<-heter_$res
NP <- sample(20:800, 300, replace = TRUE)
  poly_ <- tibble(NP) %>%
  mutate(res = map(NP, nonpoly))
  poly_<-poly_$res

names(classic_) <- c(1:300)
names(heter_) <- c(1:300)
names(poly_) <- c(1:300)

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
mapply(savepic, classic_, c(1:300))

name <- deparse(substitute(heter_))
mapply(savepic, heter_, c(1:300))

name <- deparse(substitute(poly_))
mapply(savepic, poly_, c(1:300))

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
fnames <- paste0("classic_",1:100,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(train_classic_dir))
fnames <- paste0("classic_",101:200,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(validation_classic_dir))
fnames <- paste0("classic_",201:300,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(test_classic_dir))

fnames <- paste0("heter_",1:100,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(train_heter_dir))
fnames <- paste0("heter_",101:200,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(validation_heter_dir))
fnames <- paste0("heter_",201:300,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(test_heter_dir))

fnames <- paste0("poly_",1:100,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(train_poly_dir))
fnames <- paste0("poly_",101:200,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(validation_poly_dir))
fnames <- paste0("poly_",201:300,".png")
file.copy(file.path(original_dataset_dir,fnames),
          file.path(test_poly_dir))






