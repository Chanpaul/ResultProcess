library(ggplot2)
library(gridExtra)
library(dplyr)
dataDir<-"C://Users//wangc//Results//WhereIsOutlierScala//PostProcess//bioSensor//"
patientID<-12
k<-2
winWidth<-3
R<-4
fn<-sprintf("patient%d_k_%dleap_windowSize_%d",patientID,k,winWidth);
dataFile=paste(dataDir,fn,sep="")

dt<-read.csv(dataFile,sep=" ",header=TRUE)
dt<-dt%>%mutate(new_date=paste(year,"/",month,"/",date,"",sep=""))%>%mutate(nn=as.integer(nn))
regPat1<-"\\d+\\z"
regRus1<-regexpr(regPat1,fn,perl=TRUE)
winSz<-as.integer(regmatches(fn,regRus1));
#browser()
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
dt$new_date<-factor(dt$new_date,levels=unique(dt$new_date));  #change the order of variable levels with factor()
dt<-dt%>%filter(radius==R)
sign<-dt%>%group_by(year,month,date,hour)%>%summarize(new_status=(mean(nn)>k-1));
dt<-dt%>%left_join(sign,by=c("year","month","date","hour"))
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
  labs(x="Date",y="Number of Nearest neighbors")
#objFile=paste(dataDir,"patient6_r6_k2_w3.png",sep="");
#ml <- marrangeGrob(list(cod_cpu,cod_mem), nrow=1, ncol=2)
objFile<-sprintf("patient%d_r%d_k%d_w%d.png",patientID,R,k,winWidth);
objFile=paste(dataDir,objFile,sep="");   #"patient6_r6_k2_w3.png"
ggsave(objFile,height=9,width=12,dpi=100)