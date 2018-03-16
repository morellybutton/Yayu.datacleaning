#plot diurnal micro-climate measures

#library(grid)
#library(stringr)
library(tidyverse)
library(gridExtra)

#setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/MetData/")
setwd("/users/alex/Documents/Research/Africa/ECOLIMITS/Data/Yayu")

ns<-read.csv(paste0(getwd(),"/plotnums.csv"))

#load large metstation
l.rg<-read.csv(paste0(getwd(),"/MetData/Diurnal.avgmonth.largemetstations.csv"))

#load combined csv for monthly averages of all under canopy and usb dataloggers
u.can<-read.csv(paste0(getwd(),"/MetData/Diurnal.avgmonth.allundercanopy.csv"))
usb<-read.csv(paste0(getwd(),"/MetData/Diurnal.avgmonth.usbloggers.csv"))

#add plot level measures to microclimate (TRF or MAF, Canopy > 50% vs < 40% , patcharea)
ns[ns$elevation>1750,"eclass"]<-"MAF"
ns[ns$elevation>1500&ns$elevation<=1750,"eclass"]<-"TRF-Hi"
ns[ns$elevation<=1500,"eclass"]<-"TRF-Low"
ns[ns$name=="FC1","eclass"]<-"Forest-Hi"
ns[ns$name=="FC2","eclass"]<-"Forest-Low"
ns$eclass2<-paste(ns$eclass,ns$PatchSize,sep="-")
ns[ns$name=="FC1","eclass2"]<-"Forest-Hi"
ns[ns$name=="FC2","eclass2"]<-"Forest-Low"

u.can$eclass<-ns[match(u.can$Plot,ns$name),"eclass"]
usb$eclass<-ns[match(usb$Plot,ns$name),"eclass"]

u.can$eclass1<-ns[match(u.can$Plot,ns$name),"eclass2"]
u.can1 <- u.can %>% filter(Plot!="B15"&Plot!="W3")

#plot monthly by eclass
g1<-ggplot(u.can1 %>% filter(as.Date(month)>"2014-12-01"&as.Date(month)<"2015-04-01"),aes(hour,Tmin,group=eclass1)) + geom_line(aes(color=eclass1)) + 
  scale_color_discrete(name="Elevation Class")+ facet_wrap(~month)+
  geom_ribbon(aes(ymin=Tmin-Tmin.ci,ymax=Tmin+Tmin.ci,fill=eclass1),linetype=2,alpha=0.1)+ 
  xlab("Hour")+ylab("Minimum Temperature (C)")+ scale_fill_discrete(name="Elevation Class")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14))

g2<-ggplot(u.can1 %>% filter(as.Date(month)>"2015-12-01"&as.Date(month)<"2016-04-01"),aes(hour,Tmin,group=eclass1)) + geom_line(aes(color=eclass1)) + 
  scale_color_discrete(name="Elevation Class")+ facet_wrap(~month)+
  geom_ribbon(aes(ymin=Tmin-Tmin.ci,ymax=Tmin+Tmin.ci,fill=eclass1),linetype=2,alpha=0.1)+ 
  xlab("Hour")+ylab("Minimum Temperature (C)")+ scale_fill_discrete(name="Elevation Class")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14))

g3<-grid.arrange(g1,g2,ncol=1)
ggsave(paste0(getwd(),"/MetData/Figures/U.canopy.Tmin.flowering.pdf"),g3,height=8,width=10)


g1<-ggplot(u.can1 %>% filter(as.Date(month)>"2015-06-01"&as.Date(month)<"2015-11-01"),aes(hour,Tmin,group=eclass1)) + geom_line(aes(color=eclass1)) + 
  scale_color_discrete(name="Elevation Class")+ facet_wrap(~month,ncol=4)+
  geom_ribbon(aes(ymin=Tmin-Tmin.ci,ymax=Tmin+Tmin.ci,fill=eclass1),linetype=2,alpha=0.1)+ 
  xlab("Hour")+ylab("Minimum Temperature (C)")+ scale_fill_discrete(name="Elevation Class")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14))

g2<-ggplot(u.can1 %>% filter(as.Date(month)>"2016-06-01"&as.Date(month)<"2016-11-01"),aes(hour,Tmin,group=eclass1)) + geom_line(aes(color=eclass1)) + 
  scale_color_discrete(name="Elevation Class")+ facet_wrap(~month,ncol=4)+
  geom_ribbon(aes(ymin=Tmin-Tmin.ci,ymax=Tmin+Tmin.ci,fill=eclass1),linetype=2,alpha=0.1)+ 
  xlab("Hour")+ylab("Minimum Temperature (C)")+ scale_fill_discrete(name="Elevation Class")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14))

g3<-grid.arrange(g1,g2,ncol=1)
ggsave(paste0(getwd(),"/MetData/Figures/U.canopy.Tmin.fruiting.pdf"),g3,height=8,width=10)

#absolute humidity
u.can2<- u.can1 %>% filter(Plot!="FC1")
g1<-ggplot(u.can2 %>% filter(as.Date(month)>"2014-12-01"&as.Date(month)<"2015-04-01"),aes(hour,AH,group=eclass1)) + geom_line(aes(color=eclass1)) + 
  scale_color_discrete(name="Elevation Class")+ facet_wrap(~month)+
  geom_ribbon(aes(ymin=AH-AH.ci,ymax=AH+AH.ci,fill=eclass1),linetype=2,alpha=0.1)+ 
  xlab("Hour")+ylab("Absolute Humidity [kg/m3]")+ scale_fill_discrete(name="Elevation Class")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14))

g2<-ggplot(u.can2 %>% filter(as.Date(month)>"2015-12-01"&as.Date(month)<"2016-04-01"),aes(hour,AH,group=eclass1)) + geom_line(aes(color=eclass1)) + 
  scale_color_discrete(name="Elevation Class")+ facet_wrap(~month)+
  geom_ribbon(aes(ymin=AH-AH.ci,ymax=AH+AH.ci,fill=eclass1),linetype=2,alpha=0.1)+ 
  xlab("Hour")+ylab("Absolute Humidity [kg/m3]") + scale_fill_discrete(name="Elevation Class")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14))

g3<-grid.arrange(g1,g2,ncol=1)
ggsave(paste0(getwd(),"/MetData/Figures/U.canopy.AH.flowering.pdf"),g3,height=8,width=10)

g1<-ggplot(u.can2 %>% filter(as.Date(month)>"2015-06-01"&as.Date(month)<"2015-11-01"),aes(hour,AH,group=eclass1)) + geom_line(aes(color=eclass1)) + 
  scale_color_discrete(name="Elevation Class")+ facet_wrap(~month,ncol=4)+
  geom_ribbon(aes(ymin=AH-AH.ci,ymax=AH+AH.ci,fill=eclass1),linetype=2,alpha=0.1)+ 
  xlab("Hour")+ylab("Absolute Humidity [kg/m3]")+ scale_fill_discrete(name="Elevation Class")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14))

g2<-ggplot(u.can2 %>% filter(as.Date(month)>"2016-06-01"&as.Date(month)<"2016-11-01"),aes(hour,AH,group=eclass1)) + geom_line(aes(color=eclass1)) + 
  scale_color_discrete(name="Elevation Class")+ facet_wrap(~month,ncol=4)+
  geom_ribbon(aes(ymin=AH-AH.ci,ymax=AH+AH.ci,fill=eclass1),linetype=2,alpha=0.1)+ 
  xlab("Hour")+ylab("Absolute Humidity [kg/m3]")+ scale_fill_discrete(name="Elevation Class")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14))

g3<-grid.arrange(g1,g2,ncol=1)
ggsave(paste0(getwd(),"/MetData/Figures/U.canopy.AH.fruiting.pdf"),g3,height=8,width=10)

#find frost event in 2016/17
frost <- u.can %>% filter(as.Date(month)>"2016-09-01")

ggplot(frost %>% filter(!is.na(Tmin)&month=="2016-11-01"),aes(hour,Tmin,group=eclass2) ) + geom_line(aes(color=eclass2))

#disease incidence
df<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset.csv"))

ggplot(df %>% filter(ah.fruit>10),aes(ah.fruit,propCLR,group=factor(year))) + geom_point(aes(color=factor(year))) + 
  xlab("Absolute Humidity During Berry Development [kg/m3]")+ylab("Proportion of Leaves with CLR")+
  stat_smooth(method="lm",aes(color=factor(year))) + scale_color_discrete(name="Year")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14))
ggsave(paste0(getwd(),"/MetData/Figures/AH.fruitingvs.propCLR.pdf"))

