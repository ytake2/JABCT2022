CDC_plot<-function(data,session,y,treatment){

  #data: データ
  #session: 総セッション数
  #y:アウトカムの変数名
  #treatment: 治療変数名
  dat_bl<-subset(data,treatment==0)
  bl_mean<-mean(dat_bl$y,na.rm=T)
  bl_sd<-sd(dat_bl$y,na.rm=T)
  bl_mean2<-bl_mean-bl_sd*0.25
  res_lm<-lm(y~session,dat_bl)
  int<-coef(res_lm)[1]-bl_sd*0.25
  ss<-coef(res_lm)[2]

  
  lower<-int+data$session*ss>data$y
  u<-rep(0,14)
  u[7:14][which(lower[7:14]==T)]<-1
  
  ggplot(data)+
    geom_line(aes(x=session,y=y,group=treatment),size=1,col="gray70")+
    # geom_smooth(aes(x=time,y=outcome1,group=treatment),method = "lm",se=F)+
    geom_point(aes(x=session,y=y,group=treatment,
                   color=I(ifelse(u<1,"gray70","black"))),
               size=2,shape=15)+
    scale_x_continuous(breaks=1:nrow(data))+
    geom_abline(aes(intercept=int,slope=ss))+
    geom_hline(yintercept=bl_mean2)+
    geom_vline(
      xintercept=which.max(
        data$session[data$treatment==0])+0.5,linetype=2)+
    scale_x_continuous(breaks=1:nrow(data))+
    ylab("Score")+xlab("Sessions")+theme_classic()
}



