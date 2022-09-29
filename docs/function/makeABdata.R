ABdata<-function(n_data,start_trt,beta_0,beta_1,beta_2,beta_3,rho){
  #n_data<-14 #number of session
  #start_trt<-6 #start session number for treatment 
  #beta_0<-20 # starting value
  #beta_1<--1 # baseline slope
  #beta_2<--5 # level change
  #beta_3<--0.5 # slope change
  #rho<-0.10 # autocorrelation
  
  
  Time<-1:n_data
  Time<-Time-(start_trt+1)
  
  D<-rep(0,n_data)
  D[1:start_trt]<-1

  


 #AR1
    
  u  <- arima.sim(list(ar=rho), n=n_data, sd=1)
  
  
  #generate data
  D2<-ifelse(D==1,0,1)
  y<-beta_0+beta_1*Time+beta_2*D2+beta_3*D2*Time+u
  d<-data.frame(y=y,session=1:length(y),treatment=D2)
  d
  return(d)
}
