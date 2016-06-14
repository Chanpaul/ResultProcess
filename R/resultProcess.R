library(ggplot2)
library(gridExtra)
library(dplyr)
#@dt dataset
#@r  radius
#@k  the least number of neighbors of inlier
#@w  window width
#@dfName1 the original data
#@dfName2 the detection result
outlierStatistics<-function(dfName1,dfName2,R,K,W){
  patientID<-""
  resDir<-strsplit(dfName2,"//")
  resDir<-paste(resDir[[1]][-length(resDir[[1]])],collapse="//")
  dt1<-read.csv(dfName1,sep=",",header=TRUE)
  dt2<-read.csv(dfName2,sep=" ",header=TRUE)
  regPat<-"patient\\d+"
  regRus<-regexpr(regPat,dfName2,perl=TRUE)
  patientID<-regmatches(dfName,regRus);
  resCvs<-paste(resDir,patientID,sep="//")
  sign<-dt2%>%filter(radius==R)%>%group_by(year,month,day,hour)%>%summarize(new_status=(max(nn)>K-1));
  dt1<-dt1%>%left_join(sign,by=c("year","month","day","hour"))
  dt<-dt1%>%group_by(new_status)%>%summarize(avgZ=mean(avg.Z.axis.),avgY=mean(avg.Y.axis.),avgX=mean(avg.X.axis.),
                                         avgCelsius=mean(avg.Celsius.),avgEDA=mean(avg.EDA.),
                                         stdZ=std(avg.Z.axis.),stdY=std(avg.Y.axis.),stdX=std(avg.X.axis.),
                                         stdCelsius=std(avg.Celsius.),stdEDA=std(avg.EDA.))
  write.csv(dt,resCvs,sep=",",header=TRUE)
  
}

outlierRate<-function(dt,r,k,w){
  new_dt<-dt%>%filter(radius==r)%>%group_by(year,month,day,hour)%>%summarize(new_status=(max(nn)>k-1));
  
  totalSz<-nrow(new_dt);
  outliers<-new_dt%>%filter(new_status==FALSE)%>%nrow;
  return (outliers/totalSz);
}
multiOutlierRate<-function(dfName,metricName,R,K,W){
  patientID<-""
  
  dt<-read.csv(dfName,sep=" ",header=TRUE)
  regPat<-"patient\\d+"
  regRus<-regexpr(regPat,dfName,perl=TRUE)
  patientID<-regmatches(dfName,regRus);
  if (metricName=="radius"){
    outlier_rate<-lapply(R,outlierRate,dt=dt,k=K,w=W);
    outlierDF<-data.frame(R,sapply(outlier_rate,function(z) z))
    colnames(outlierDF)<-c("radius","rate");
  } else if (metricName=="outlierK"){
    outlier_rate<-lapply(K,outlierRate,dt=dt,r=R,w=W);
    outlierDF<-data.frame(K,sapply(outlier_rate,function(z) z))
    colnames(outlierDF)<-c("K","rate");
  } else{
    outlier_rate<-lapply(W,outlierRate,dt=dt,k=K,r=R);
    outlierDF<-data.frame(W,sapply(outlier_rate,function(z) z))
    colnames(outlierDF)<-c("W","rate");
  }
  outlierDF<-outlierDF%>%mutate(id=rep(patientID,nrow(outlierDF)))
  return(outlierDF)
}


dataDir<-"C://Users//wangc//Results//WhereIsOutlierScala//PostProcess//bioSensor//"

#*************************Evaluation I Outlier vs radius*********************************
patientID<-c(6,12,14)
k<-2
winWidth<-4
R<-seq(1,10,1)

tempDtFn<-sapply(patientID,function(ID,K,W)sprintf("patient%d_k_%dleap_windowSize_%d",ID,K,W),K=k,W=winWidth);
dtFn<-sapply(tempDtFn, function(dir,fn) paste(dir,fn,sep=""),dir=dataDir)

rateAgainstR<-lapply(dtFn,multiOutlierRate,metricName="radius",R=R,K=k,W=winWidth)
outlierRateWithR<-ggplot()
for (tdf in rateAgainstR){
  outlierRateWithR<-outlierRateWithR+geom_line(data=tdf,aes(x=radius,y=rate,linetype=id),size=1)
}
outlierRateWithR<-outlierRateWithR+labs(x="Radius",y="Outerlier ratio")+
  scale_x_continuous(breaks=1:10)+
  theme(legend.background=element_rect(colour="black"))+
  theme(legend.text=element_text(size=12,colour="black"))+
  scale_linetype_discrete(name="Patient")+
  theme(axis.text.x=element_text(size=12),axis.text.y=element_text(size=12))+
  theme(axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))
  
  

objFile<-sprintf("OuterlierRatevsR_k%d_w%d.png",k,winWidth);
objFile=paste(dataDir,objFile,sep="");   #"patient6_r6_k2_w3.png"
ggsave(objFile,height=9,width=12,dpi=100)

#*******************************Evaluation II Outlier vs window width*******************************************************
patientID<-c(6,12,14)
k<-2
winWidth<-seq(3,10,1)
R<-3

tempDtFn<-sapply(patientID,function(ID,K,R)sprintf("patient%d_k_%dleap_Radius_%d",ID,K,R),K=k,R=R);
dtFn<-sapply(tempDtFn, function(dir,fn) paste(dir,fn,sep=""),dir=dataDir)

rateAgainstW<-lapply(dtFn,multiOutlierRate,metricName="W",R=R,K=k,W=winWidth)
outlierRateWithW<-ggplot()
for (tdf in rateAgainstW){
  outlierRateWithW<-outlierRateWithW+geom_line(data=tdf,aes(x=W,y=rate,linetype=id),size=1)
}
outlierRateWithW<-outlierRateWithW+labs(x="Window Width",y="Outerlier ratio")+
  scale_x_continuous(breaks=3:10)+
  scale_y_continuous(breaks=seq(0,0.1,0.001))+
  theme(legend.background=element_rect(colour="black"))+
  theme(legend.text=element_text(size=12,colour="black"))+
  scale_linetype_discrete(name="Patient")+
  theme(axis.text.x=element_text(size=12),axis.text.y=element_text(size=12))+
  theme(axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))

objFile<-sprintf("OuterlierRatevsW_k%d_r%d.png",k,R);
objFile=paste(dataDir,objFile,sep="");   #"patient6_r6_k2_w3.png"
ggsave(objFile,height=9,width=12,dpi=100)
# 
# browser()
#*******************************Evaluation III Outlier vs K*******************************************************
patientID<-c(6,12,14)
k<-seq(2,6,1)
winWidth<-8
R<-3

tempDtFn<-sapply(patientID,function(ID,W,R)sprintf("patient%d_w_%dleap_Radius_%d",ID,W,R),W=winWidth,R=R);
dtFn<-sapply(tempDtFn, function(dir,fn) paste(dir,fn,sep=""),dir=dataDir)
rateAgainstK<-lapply(dtFn,multiOutlierRate,metricName="outlierK",R=R,K=k,W=winWidth)
outlierRateWithK<-ggplot()
for (tdf in rateAgainstK){
  outlierRateWithK<-outlierRateWithK+geom_line(data=tdf,aes(x=K,y=rate,linetype=id),size=1)
}
outlierRateWithK<-outlierRateWithK+labs(x="k",y="Outerlier ratio")+
  scale_x_continuous(breaks=2:5)+
  theme(legend.background=element_rect(colour="black"))+
  theme(legend.text=element_text(size=12,colour="black"))+
  scale_linetype_discrete(name="Patient")+
  theme(axis.text.x=element_text(size=12),axis.text.y=element_text(size=12))+
  theme(axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))


objFile<-sprintf("OuterlierRatevsK_w%d_r%d.png",winWidth,R);
objFile=paste(dataDir,objFile,sep="");   #"patient6_r6_k2_w3.png"
ggsave(objFile,height=9,width=12,dpi=100)

#browser()

#************************Evaluation IV*******************************
patientID<-6
k<-2
winWidth<-3
R<-3
fn<-sprintf("patient%d_k_%dleap_windowSize_%d",patientID,k,winWidth);
dataFile=paste(dataDir,fn,sep="")

dt<-read.csv(dataFile,sep=" ",header=TRUE)
dt<-dt%>%mutate(new_date=paste(year,"/",month,"/",day,"",sep=""))%>%mutate(nn=as.integer(nn))

regPat1<-"\\d+\\z"
regRus1<-regexpr(regPat1,fn,perl=TRUE)
winSz<-as.integer(regmatches(fn,regRus1));

dt$new_date<-factor(dt$new_date,levels=unique(dt$new_date));  #change the order of variable levels with factor()
dt<-dt%>%filter(radius==R)
sign<-dt%>%group_by(year,month,day,hour)%>%summarize(new_status=(max(nn)>k-1));
dt<-dt%>%left_join(sign,by=c("year","month","day","hour"))
#browser()
cod_cpu<-ggplot(dt,mapping=aes(x=hour,y=nn,color=new_status))+geom_point(size=2)+
  geom_hline(yintercept=k,color="black",linetype="dashed")+
  facet_wrap(~new_date)+
  theme(axis.text.x=element_text(size=9,angle=45),axis.text.y=element_text(size=16))+
  theme(axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))+
  theme(axis.ticks.x = element_line(size = 1))+
  theme(strip.text=element_text(size=16))+
  scale_x_continuous( breaks=seq(0,23,3),minor_breaks=NULL,limits=c(1,24))+
  scale_y_continuous(breaks=seq(0,winSz,1))+
  scale_color_discrete(name="Drug Abusing Status",labels=c("Positive","Negative"))+
  labs(x="Day",y="Number of Nearest neighbors")
objFile<-sprintf("patient%d_r%d_k%d_w%d.png",patientID,R,k,winWidth);
objFile=paste(dataDir,objFile,sep="");   #"patient6_r6_k2_w3.png"
ggsave(objFile,height=9,width=12,dpi=100)

#***********************Evaluation V statistics of inlier and outlier***************************
patientID<-c(6,12,14)
k<-seq(2,6,1)
winWidth<-8
R<-3
tempDtFn<-sapply(patientID,function(ID,W,R)sprintf("patient%d_w_%dleap_Radius_%d",ID,W,R),W=winWidth,R=R);
dtFn<-sapply(tempDtFn, function(dir,fn) paste(dir,fn,sep=""),dir=dataDir)
lapply(dtFn,outlierStatistics(dfName1,dfName2,R,K,W))

#************************Evaluation VI resource usage*******************************
# patientID<-6
# k<-2
# winWidth<-3
# R<-3
# fn<-sprintf("patient%d_k_%dleap_windowSize_%d",patientID,k,winWidth);
# dataFile=paste(dataDir,fn,sep="")
# 
# dt<-read.csv(dataFile,sep=" ",header=TRUE)
# dt<-dt%>%mutate(new_date=paste(year,"/",month,"/",day,"",sep=""))%>%mutate(nn=as.integer(nn))
# 
# regPat1<-"\\d+\\z"
# regRus1<-regexpr(regPat1,fn,perl=TRUE)
# winSz<-as.integer(regmatches(fn,regRus1));

# 
# cod_cpu<-ggplot(dt,mapping=aes(x=V1,y=V3))+geom_line(color="black",size=2)+
#   geom_point(position="jitter",col=2,pch=16,cex=1)+
#   theme(axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))+
#   theme(axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))+
#   theme(axis.ticks.x = element_line(size = 1))+
#   scale_x_continuous( breaks=dt$V1,minor_breaks=NULL)+
#   xlab("Window width")+ylab("CPU time(s)")
# cod_mem<-ggplot(dt,mapping=aes(x=V1,y=V2))+geom_line(color="black",size=2)+
#   geom_point(position="jitter",col=2,pch=16,cex=1)+
#   theme(axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))+
#   theme(axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))+
#   theme(axis.ticks.x = element_line(size = 1))+
#   scale_x_continuous( breaks=dt$V1,minor_breaks=NULL)+
#   xlab("Window width")+ylab("Memory Usage(MB)")

#objFile=paste(dataDir,"patient6_r6_k2_w3.png",sep="");
#ml <- marrangeGrob(list(cod_cpu,cod_mem), nrow=1, ncol=2)
# objFile<-sprintf("patient%d_r%d_k%d_w%d.png",patientID,R,k,winWidth);
# objFile=paste(dataDir,objFile,sep="");   #"patient6_r6_k2_w3.png"
# ggsave(objFile,height=9,width=12,dpi=100)
