raw_plot<-function(data,out_varname, time_varname,treatment_varname,line=c("level","trend"),ymax){
  data<-data[,c(out_varname,time_varname,treatment_varname)]
  names(data)<-c("y","time","treatment")
  y<-data[,1]
  #ymax<-max(y)+5
  data%>% group_by(treatment)%>%
    summarize(mean=mean(y)
              ,median=median(y))->mean
  data%>% group_by(treatment) %>% count() -> N_session
  
  M_base1<-as.numeric(mean[1,2])
  M_trt1<-as.numeric(mean[2,2])
  
  N_base1<-as.numeric(N_session[1,2])
  N_trt1<-as.numeric(N_session[2,2])
  
  g1<-ggplot(data)+ 
    geom_line(aes(x=time,y= y,group=treatment),col="gray70",size=0.7)+
    geom_point(aes(x=time,y= y,group=treatment),col="gray70",size=2,shape=17)+
    geom_vline(
      xintercept=which.max(
        data$time[data$treatment==0])+0.5,linetype=2,col="gray50")+
    scale_x_continuous(breaks=1:nrow(data))+
    ylab("Score")+xlab("Sessions")+
    ylim(0,ymax)+theme_classic()
  
  if(line=="trend"){
    g1+geom_smooth(aes(x=time,y= y,group=treatment),method = "lm",col="black",se=F)}
  else if(line=="level"){
    g1+geom_segment(aes(x=1,
                        xend=N_base1,
                        y=M_base1,
                        yend=M_base1),
                    col="black",
                    size=0.5)+
      geom_segment(aes(x=N_base1+1,
                       xend=N_base1+1+N_trt1,
                       y=M_trt1,
                       yend=M_trt1),
                   col="black",
                   size=0.5)}else{g1}
  
}




