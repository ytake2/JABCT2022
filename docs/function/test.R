source("../function/makeABdata2.R")
source("../function/level_trend_plot.R")

n_data<-14 #number of session
start_trt<-7 #start session number for treatment 
beta_0<-20# starting value
beta_1<-0 # baseline slope
beta_2<--15 # level change
beta_3<-0 # slope change
rho<-0.10 # autocorrelation

set.seed(1222)
dat1<-ABdata(n_data,start_trt,
             beta_0,beta_1,beta_2,beta_3,
             rho)

library(tidyverse)
raw_plot(dat1,"y","session","treatment",line="level",ymax=30)

n_data<-14 #number of session
start_trt<-7 #start session number for treatment 
beta_0<-15 # starting value
beta_1<-0 # baseline slope
beta_2<--1 # level change
beta_3<--1 # slope change
rho<-0.20 # autocorrelation

set.seed(1122)
dat2<-ABdata(n_data,start_trt,
             beta_0,beta_1,beta_2,beta_3,
             rho)


n_data<-14 #number of session
start_trt<-7 #start session number for treatment 
beta_0<-15 # starting value
beta_1<--1# baseline slope
beta_2<--0.3 # level change
beta_3<-0 # slope change
rho<-0.10 # autocorrelation

set.seed(2444)
dat3<-ABdata(n_data,start_trt,
             beta_0,beta_1,beta_2,beta_3,
             rho)
