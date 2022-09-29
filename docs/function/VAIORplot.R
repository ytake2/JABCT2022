#プロット用関数
vaior_plot<-function(data,session,y,treatment){
  #data: データ
  #session: 総セッション数
  #y:アウトカムの変数名
  #treatment: 治療変数名
 # data<-dat3
  dat_bl<-subset(data,treatment==0)
  res_mblm<-mblm::mblm(y~session,dat_bl)
  int<-coef(res_mblm)[1]
  m_error<-mad(predict(res_mblm))
  val_int<-c(int-m_error,int+m_error)
  ss<-coef(res_mblm)[2]
  
  lower<-val_int[1]+data$session*ss>data$y
  mid<-(data$y>val_int[1]+data$session*ss)&(data$y<val_int[2]+data$session*ss)
  upper<-data$y>val_int[2]+data$session*ss&data$y
  
  u<-rep(0,14)
  u[7:14][which(lower[7:14]==T)]<-1

  
  ggplot(data)+
    geom_abline(aes(intercept=int,slope=ss))+
    geom_abline(intercept = val_int, slope = ss, linetype = 'dotted')+
    
    geom_line(aes(x=session,y=y,group=treatment),size=1,col="gray70")+
    # geom_smooth(aes(x=time,y=outcome1,group=treatment),method = "lm",se=F)+
    geom_point(aes(x=session,y=y,group=treatment,
                 color=I(ifelse(u<1,"gray70","black"))),size=2,shape=15)+
    geom_vline(
      xintercept=which.max(
        data$session[data$treatment==0])+0.5,linetype=2)+
    scale_x_continuous(breaks=1:nrow(data))+
    ylab("Score")+xlab("Sessions")+
    theme_classic()
}

