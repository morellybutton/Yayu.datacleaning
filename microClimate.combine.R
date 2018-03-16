#create figures of microclimate data
library(gdata)
library(stringr)
library(gridExtra)
library(Rmisc)
library(ggplot2)

#setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/")
setwd("/users/alex/Documents/Research/Africa/ECOLIMITS/Data/Yayu/")

names<-data.frame(read.csv(paste0(getwd(),"/MetData/MS_names.csv")), stringsAsFactors=FALSE)
ns<-read.csv(paste0(getwd(),"/plotnums.csv"))
dnames<-data.frame(read.csv(paste0(getwd(),"/MetData/DL_names.csv")), stringsAsFactors=FALSE)

#canopy measures
canopy<-read.csv(paste0(getwd(),"/NPP/LAI/Analysis/Yayu_gap_analysis.csv"))
#remove NAs
#canopy<-canopy[!is.na(canopy$Plot),]

#create monthly sums and averages
#figure ppt
mS<-as.character(names[grep("Met_",as.character(names$Plot)),"DataLogger"])
grid1<-list()
for(j in 1:length(mS)){
  summ<-read.csv(paste0(getwd(),"/MetData/",mS[j],"_summary.csv"))
  #remove NAs
  summ<-summ[!is.na(summ$day),]
  #organize by month
  summ$month<-as.Date(cut(as.Date(summ$day),breaks="month"))
  #check for missing dates
  daycount <- table(summ$day) 
  
  # generate vector of all dates[taken from http://www.r-bloggers.com/fix-missing-dates-with-r/]
  alldays <- seq(as.Date(min(summ$month,na.rm=T)),length=as.numeric(as.Date(summ[!is.na(summ$day),"day"][length(summ[!is.na(summ$day),1])])-as.Date(min(summ$month,na.rm=T))+1),by="+1 day")  
  allcount <- table(alldays) # create table object from alldays.
  actindex <- match(names(allcount),names(daycount),nomatch = 0)  
  # create "active" index: vector of length(allcount), i.e. all days. 
  # on days with no activity (i.e. a missing day in daycount), this has value 0 (nomatch = 0). 
  # For days with activity, actindex holds the index of the matching position in daycount.
  # function to get entries of daycount corresponding to actindex
  # indexing is a bit tricky. i loops over all days. get correct date by
  # substracting all "zero-activity" days accumulated so far.
  days <- function(actindex,daycount){
    n <- length(actindex)
    x <- rep(NA,times=n)
    zero <- 0
    for (i in 1:n){
      if (actindex[i]==0) {
        zero <- zero +1
        x[i] <- 0
      } else {
        x[i] <- daycount[i-zero]
      }    	
    }
    return(x)
  }
  
  alldaycount <- data.frame(days(actindex,daycount))   # construct vector with number of hits per day
  alldaycount[,2] <- names(allcount)           # name entries by consecutive dates.
  
  #for all days where ppt data is missing put daily average for month
  ds<-data.frame(alldaycount[alldaycount[,1]==0,2])
  colnames(ds)<-"day"
  #add month
  ds$month<-as.Date(cut(as.Date(ds[,1]),breaks="month"))
  m<-unique(ds$month)
  for(i in 1:length(m)){
    ds[ds$month==m[i],"avg"]<-mean(summ[summ$month==m[i],"Tppt"],na.rm=T)
  }
  #add average ppt for missing points with daily ppt measures
  alldaycount[match(as.Date(summ$day),as.Date(alldaycount$V2),nomatch =0),"Tppt"]<-summ$Tppt
  alldaycount[match(ds$day,alldaycount$V2),"Tppt"]<-ds$avg
  rm(ds)
  #take monthly sums
  alldaycount$month<-as.Date(cut(as.Date(alldaycount$V2),breaks="month"))
  #m<-data.frame(unique(alldaycount$month))
  
  m<-data.frame(aggregate(alldaycount$Tppt,list(alldaycount$month),FUN="sum"))
  colnames(m)<-c("month","Tppt")
  n<-data.frame(aggregate(summ[,2:68],list(summ$month),FUN="mean",na.action="na.omit"))
  #m[,3:69]<-n[,2:68]
  #create figure of monthly ppt
  #ggplot(m,aes(m$month,m$Tppt))+geom_bar(stat="identity")+xlab("Month")+ylab("Precipitation (cm)")+ggtitle("Monthly Precipitation")
  ggplot(m,aes(m$month,m$Tppt))+geom_line()+xlab("Month")+ylab("Monthly Precipitation (cm)")+ggtitle("Monthly Precipitation")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,axis.text.x=element_text(angle = 45,hjust=1))
  
  ggsave(paste0(getwd(),"/MetData/Figures/",mS[j],"_ppt.pdf"), width = 12, height = 8)
#save ppt values
write.csv(m,paste0(getwd(),"/MetData/summary/",mS[j],"_monthlyppt.csv"))
}

#create dataframes for each variable
AH<-list() #absolute humidity
VPD<-list() #vapour pressure deficit
mVPD<-list() #max vapour pressure deficit
VWC<-list() #volumetric water content, soil
ST<-list() #soil temperature
mxST<-list() #max soil temperature
mnST<-list() #min soil temperature
TMAX<-list() #max temp
TMIN<-list() #min temp
TAVG<-list() #avg temp

#for metstations
#remove ECO_7 due to disturbance
names<-names[names$DataLogger!="ECO_7",]
names2<-names[grep("Met_",names$Plot,invert=T),]
for(i in 1:(length(names2[,1]))){
  dF<-read.csv(paste0(getwd(),"/MetData/",names2[i,"DataLogger"],"_summary.csv"))
  #remove NAs
  dF<-dF[!is.na(dF$day),]
  #only take day averages (t3,t4,t5,t6)
  #check if have t2soilT.1
  if(length(dF[,grep("t2soilT.1",colnames(dF))])>0) dF.2<-cbind(dF$Tmax,dF$Tmin,dF$SoilTmax,dF$SoilTmin,dF$VPDmax,dF$day,dF[,grep("t3",colnames(dF))],dF[,grep("t2soilT.1",colnames(dF))],dF[,grep("t4",colnames(dF))],dF[,grep("t5",colnames(dF))],dF[,grep("t6",colnames(dF))]) else dF.2<-cbind(dF$Tmax,dF$Tmin,dF$Soiltmax,dF$Soiltmin,dF$VPDmax,dF$day,dF[,grep("t3",colnames(dF))],dF[,grep("t4",colnames(dF))],dF[,grep("t5",colnames(dF))],dF[,grep("t6",colnames(dF))])
  #add month
  dF.2$month<-as.Date(cut(as.Date(dF.2[,6]),breaks="month"))
    
  dF.3<-aggregate(dF.2[,grep("ah",colnames(dF.2))],list(dF.2$month),FUN="mean")
  dF.3$avg<-rowMeans(dF.3[,2:5],na.rm=T)
  dF.3$name<-names2[i,"DataLogger"]
  dF.3$elevation<-ns[ns$name==as.character(names2[i,"Plot"]),"elevation"] 
  dF.3$patchArea<-ns[ns$name==as.character(names2[i,"Plot"]),"PatchArea"]
  dF.3$kebele<-ns[ns$name==as.character(names2[i,"Plot"]),"Kebele"]
  AH[[i]]<-cbind(dF.3$Group.1,dF.3[,6:ncol(dF.3)])
  rm(dF.3)
  
  dF.3<-aggregate(dF.2[,grep("vpd",colnames(dF.2))],list(dF.2$month),FUN="mean")
  dF.3$avg<-rowMeans(dF.3[,2:5],na.rm=T)
  dF.3$name<-names2[i,"DataLogger"]
  dF.3$elevation<-ns[ns$name==as.character(names2[i,"Plot"]),"elevation"] 
  dF.3$patchArea<-ns[ns$name==as.character(names2[i,"Plot"]),"PatchArea"]
  dF.3$kebele<-ns[ns$name==as.character(names2[i,"Plot"]),"Kebele"]
  VPD[[i]]<-cbind(dF.3$Group.1,dF.3[,6:ncol(dF.3)])
  rm(dF.3)
  
  dF.3<-aggregate(dF.2[,5],list(dF.2$month),FUN="max")
  dF.3$name<-names2[i,"DataLogger"]
  dF.3$elevation<-ns[ns$name==as.character(names2[i,"Plot"]),"elevation"] 
  dF.3$patchArea<-ns[ns$name==as.character(names2[i,"Plot"]),"PatchArea"]
  dF.3$kebele<-ns[ns$name==as.character(names2[i,"Plot"]),"Kebele"]
  mVPD[[i]]<-dF.3
  
  dF.3<-aggregate(dF.2[,grep("vwc",colnames(dF.2))],list(dF.2$month),FUN="mean")
  dF.3$avg<-rowMeans(dF.3[,2:5],na.rm=T)
  dF.3$name<-names2[i,"DataLogger"]
  dF.3$elevation<-ns[ns$name==as.character(names2[i,"Plot"]),"elevation"] 
  dF.3$patchArea<-ns[ns$name==as.character(names2[i,"Plot"]),"PatchArea"]
  dF.3$kebele<-ns[ns$name==as.character(names2[i,"Plot"]),"Kebele"]
  VWC[[i]]<-cbind(dF.3$Group.1,dF.3[,6:ncol(dF.3)])
  rm(dF.3)
  
  dF.3<-aggregate(dF.2[,grep("soilT",colnames(dF.2))],list(dF.2$month),FUN="mean")
  dF.3$avg<-rowMeans(dF.3[,2:5],na.rm=T)
  dF.3$name<-names2[i,"DataLogger"]
  dF.3$elevation<-ns[ns$name==as.character(names2[i,"Plot"]),"elevation"] 
  dF.3$patchArea<-ns[ns$name==as.character(names2[i,"Plot"]),"PatchArea"]
  dF.3$kebele<-ns[ns$name==as.character(names2[i,"Plot"]),"Kebele"]
  ST[[i]]<-cbind(dF.3$Group.1,dF.3[,6:ncol(dF.3)])   
  rm(dF.3)
  
  dF.3<-aggregate(dF.2[,3],list(dF.2$month),FUN="max")
  dF.3$name<-names2[i,"DataLogger"]
  dF.3$elevation<-ns[ns$name==as.character(names2[i,"Plot"]),"elevation"] 
  dF.3$patchArea<-ns[ns$name==as.character(names2[i,"Plot"]),"PatchArea"]
  dF.3$kebele<-ns[ns$name==as.character(names2[i,"Plot"]),"Kebele"]
  mxST[[i]]<-dF.3
  rm(dF.3)
  
  dF.3<-aggregate(dF.2[,4],list(dF.2$month),FUN="min")
  dF.3$name<-names2[i,"DataLogger"]
  dF.3$elevation<-ns[ns$name==as.character(names2[i,"Plot"]),"elevation"] 
  dF.3$patchArea<-ns[ns$name==as.character(names2[i,"Plot"]),"PatchArea"]
  dF.3$kebele<-ns[ns$name==as.character(names2[i,"Plot"]),"Kebele"]
  mnST[[i]]<-dF.3
  rm(dF.3)
  
  dF.3<-aggregate(dF.2[,grep("Tmax",colnames(dF.2))],list(dF.2$month),FUN="max")
  dF.3$name<-names2[i,"DataLogger"]
  dF.3$elevation<-ns[ns$name==as.character(names2[i,"Plot"]),"elevation"] 
  dF.3$patchArea<-ns[ns$name==as.character(names2[i,"Plot"]),"PatchArea"]
  dF.3$kebele<-ns[ns$name==as.character(names2[i,"Plot"]),"Kebele"]
  TMAX[[i]]<-cbind(dF.3$Group.1,dF.3[,2:ncol(dF.3)])
  rm(dF.3)
  
  dF.3<-aggregate(dF.2[,grep("Tmin",colnames(dF.2))],list(dF.2$month),FUN="min")
  dF.3$name<-names2[i,"DataLogger"]
  dF.3$elevation<-ns[ns$name==as.character(names2[i,"Plot"]),"elevation"] 
  dF.3$patchArea<-ns[ns$name==as.character(names2[i,"Plot"]),"PatchArea"]
  dF.3$kebele<-ns[ns$name==as.character(names2[i,"Plot"]),"Kebele"]
  TMIN[[i]]<-cbind(dF.3$Group.1,dF.3[,2:ncol(dF.3)])
  rm(dF.3)
  
  dF.3<-aggregate(dF.2[,grep("Tavg",colnames(dF.2))],list(dF.2$month),FUN="mean")
  dF.3$avg<-rowMeans(dF.3[,2:5],na.rm=T)
  dF.3$name<-names2[i,"DataLogger"]
  dF.3$elevation<-ns[ns$name==as.character(names2[i,"Plot"]),"elevation"] 
  dF.3$patchArea<-ns[ns$name==as.character(names2[i,"Plot"]),"PatchArea"]
  dF.3$kebele<-ns[ns$name==as.character(names2[i,"Plot"]),"Kebele"]
  TAVG[[i]]<-cbind(dF.3$Group.1,dF.3[,6:ncol(dF.3)])
  rm(dF.3)
}

#add dataloggers
#remove dataloggers have "no data"
dnames<-dnames[grep("no data",as.character(dnames$Datalogger.id),invert=T),]
for(i in 1:(length(dnames[,1]))){
  if(strsplit(as.character(dnames[i,"Datalogger.id"]),"_")[[1]][2]==50) next
  if(strsplit(as.character(dnames[i,"Datalogger.id"]),"_")[[1]][2]==51) next
  if(dnames[i,"Datalogger.id"]=="ECFF_30") next
  if(dnames[i,"Datalogger.id"]=="NCRC_21") next
  if(dnames[i,"Datalogger.id"]=="ECFF_44") next
  dF<-read.csv(paste0(getwd(),"/MetData/summary/",dnames[i,"Datalogger.id"],"_summary.csv"))
  #remove NAs
  dF<-dF[!is.na(dF$day),]
  #only take day averages (t3,t4,t5,t6)
  #check if have t2soilT.1
  #if(length(dF[,grep("t2soilT.1",colnames(dF))])>0) dF.2<-cbind(dF$Tmax,dF$Tmin,dF$day,dF[,grep("t3",colnames(dF))],dF[,grep("t2soilT.1",colnames(dF))],dF[,grep("t4",colnames(dF))],dF[,grep("t5",colnames(dF))],dF[,grep("t6",colnames(dF))]) else 
  dF.2<-cbind(dF$Tmax,dF$Tmin,dF$VPDmax,dF$day,dF[,grep("t3",colnames(dF))],dF[,grep("t4",colnames(dF))],dF[,grep("t5",colnames(dF))],dF[,grep("t6",colnames(dF))])
    
  #add month
  dF.2$month<-as.Date(cut(as.Date(dF.2[,4]),breaks="month"))
    
  dF.3<-aggregate(dF.2[,grep("ah",colnames(dF.2))],list(dF.2$month),FUN="mean")
  dF.3$avg<-rowMeans(dF.3[,2:5],na.rm=T)
  dF.3$name<-dnames[i,"Datalogger.id"]
  dF.3$elevation<-ns[ns$name==as.character(dnames[i,"Plot"]),"elevation"] 
  dF.3$patchArea<-ns[ns$name==as.character(dnames[i,"Plot"]),"PatchArea"]
  dF.3$kebele<-ns[ns$name==as.character(dnames[i,"Plot"]),"Kebele"]
  AH[[i+10]]<-cbind(dF.3$Group.1,dF.3[,6:ncol(dF.3)]) 
  rm(dF.3)
  
  dF.3<-aggregate(dF.2[,grep("vpd",colnames(dF.2))],list(dF.2$month),FUN="mean")
  dF.3$avg<-rowMeans(dF.3[,2:5],na.rm=T)
  dF.3$name<-dnames[i,"Datalogger.id"]
  dF.3$elevation<-ns[ns$name==as.character(dnames[i,"Plot"]),"elevation"] 
  dF.3$patchArea<-ns[ns$name==as.character(dnames[i,"Plot"]),"PatchArea"]
  dF.3$kebele<-ns[ns$name==as.character(dnames[i,"Plot"]),"Kebele"]
  VPD[[i+10]]<-cbind(dF.3$Group.1,dF.3[,6:ncol(dF.3)])  
  rm(dF.3)
  
  dF.3<-aggregate(dF.2[,3],list(dF.2$month),FUN="max")
  dF.3$name<-dnames[i,"Datalogger.id"]
  dF.3$elevation<-ns[ns$name==as.character(dnames[i,"Plot"]),"elevation"] 
  dF.3$patchArea<-ns[ns$name==as.character(dnames[i,"Plot"]),"PatchArea"]
  dF.3$kebele<-ns[ns$name==as.character(dnames[i,"Plot"]),"Kebele"]
  mVPD[[i+10]]<-dF.3
  rm(dF.3)
  
  dF.3<-aggregate(dF.2[,grep("Tmax",colnames(dF.2))],list(dF.2$month),FUN="max")
  dF.3$name<-dnames[i,"Datalogger.id"]
  dF.3$elevation<-ns[ns$name==as.character(dnames[i,"Plot"]),"elevation"] 
  dF.3$patchArea<-ns[ns$name==as.character(dnames[i,"Plot"]),"PatchArea"]
  dF.3$kebele<-ns[ns$name==as.character(dnames[i,"Plot"]),"Kebele"]
  TMAX[[i+10]]<-cbind(dF.3$Group.1,dF.3[,2:ncol(dF.3)])  
  rm(dF.3)
  
  dF.3<-aggregate(dF.2[,grep("Tmin",colnames(dF.2))],list(dF.2$month),FUN="min")
  dF.3$name<-dnames[i,"Datalogger.id"]
  dF.3$elevation<-ns[ns$name==as.character(dnames[i,"Plot"]),"elevation"] 
  dF.3$patchArea<-ns[ns$name==as.character(dnames[i,"Plot"]),"PatchArea"]
  dF.3$kebele<-ns[ns$name==as.character(dnames[i,"Plot"]),"Kebele"]
  TMIN[[i+10]]<-cbind(dF.3$Group.1,dF.3[,2:ncol(dF.3)])  
  rm(dF.3)
  
  dF.3<-aggregate(dF.2[,grep("Tavg",colnames(dF.2))],list(dF.2$month),FUN="mean")
  dF.3$avg<-rowMeans(dF.3[,2:5],na.rm=T)
  dF.3$name<-dnames[i,"Datalogger.id"]
  dF.3$elevation<-ns[ns$name==as.character(dnames[i,"Plot"]),"elevation"] 
  dF.3$patchArea<-ns[ns$name==as.character(dnames[i,"Plot"]),"PatchArea"]
  dF.3$kebele<-ns[ns$name==as.character(dnames[i,"Plot"]),"Kebele"]
  TAVG[[i+10]]<-cbind(dF.3$Group.1,dF.3[,6:ncol(dF.3)])    
  rm(dF,dF.2,dF.3)
}

#add elevation classes
ah<-do.call(rbind.data.frame,AH)
#remove months before July 2014
ah<-ah[ah[,1]>="2014-07-01",]
ah[ah$elevation>1750,"eclass"]<-"MAF"
ah[ah$elevation>1500&ah$elevation<=1750,"eclass"]<-"TRF-Hi"
ah[ah$elevation<=1500,"eclass"]<-"TRF-Low"
ah[ah$name=="ECO_1","eclass"]<-"Forest-Hi"
ah[ah$name=="ECO_6","eclass"]<-"Forest-Low"

#ah[ah$elevation<1500,6]<-"Low"
colnames(ah)<-c("month","avg","name","elevation","kebele","eclass")
#ah$psize<-as.character(ah$psize)
#ah[ah$eclass=="Forest-Hi"|ah$eclass=="Forest-Low","psize"]<-"Forest"
#ah[ah$psize=="Mid","psize"]<-"Small"
#ahc <- summarySE(ah, measurevar="avg", groupvars=c("month","eclass"),na.rm=T)
#ahp <- summarySE(ah, measurevar="avg", groupvars=c("month","psize"),na.rm=T)
#ahk <-summarySE(ah, measurevar="avg", groupvars=c("month","kebele"),na.rm=T)
#ahc$var<-"AH"

vpd<-data.frame(do.call(rbind.data.frame,mVPD),stringsAsFactors = F)
#mvpd<-do.call(rbind.data.frame,mVPD)
colnames(vpd)<-c("month","max","name","elevation","patcharea","kebele")
#remove -Inf
#vpd[vpd$max==-Inf&!is.na(vpd$max),"max"]<-NA
#vpc <- summarySE(mvpd, measurevar="max", groupvars=c("month","dist"),na.rm=T)
#vpc$var<-"VPD"

#remove months before July 2014
vpd<-vpd[vpd[,1]>="2014-07-01",]
vpd[vpd$elevation>1750,"eclass"]<-"MAF"
vpd[vpd$elevation>1500&vpd$elevation<=1750,"eclass"]<-"TRF-Hi"
vpd[vpd$elevation<=1500,"eclass"]<-"TRF-Low"
#vpd[vpd$elevation<1500,6]<-"Low"
vpd[vpd$name=="ECO_1","eclass"]<-"Forest-Hi"
vpd[vpd$name=="ECO_6","eclass"]<-"Forest-Low"

#colnames(vpd)<-c("month","max","name","elevation","psize","kebele","eclass")
#vpd$psize<-as.character(vpd$psize)
#vpd[vpd$eclass=="Forest-Hi"|vpd$eclass=="Forest-Low","psize"]<-"Forest"
#vpd[vpd$psize=="Mid","psize"]<-"Small"

#vpc <- summarySE(vpd, measurevar="max", groupvars=c("month","eclass"),na.rm=T)
#vpp <- summarySE(vpd, measurevar="max", groupvars=c("month","psize"),na.rm=T)

#vpc$var<-"VPD"
vwc<-do.call(rbind.data.frame,VWC)
colnames(vwc)<-c("month","max","name","elevation","patcharea","kebele")

#remove months before July 2014
vwc<-vwc[vwc[,1]>="2014-07-01",]
vwc[vwc$elevation>1750,"eclass"]<-"MAF"
vwc[vwc$elevation>1500&vwc$elevation<=1750,"eclass"]<-"TRF-Hi"
vwc[vwc$elevation<=1500,"eclass"]<-"TRF-Low"
#vwc[vwc$elevation<1500,6]<-"Low"
vwc[vwc$name=="ECO_1","eclass"]<-"Forest-Hi"
vwc[vwc$name=="ECO_6","eclass"]<-"Forest-Low"

#colnames(vwc)<-c("month","avg","name","elevation","psize","kebele","eclass")
#vwc$psize<-as.character(vwc$psize)
#vwc[vwc$eclass=="Forest-Low"|vwc$eclass=="Forest-Hi","psize"]<-"Forest"
#vwc[vwc$psize=="Mid","psize"]<-"Small"

#vwcc <- summarySE(vwc, measurevar="avg", groupvars=c("month","eclass"),na.rm=T)
#vwcp <- summarySE(vwc, measurevar="avg", groupvars=c("month","psize"),na.rm=T)

#ahc$ElevationClass<-factor(ahc$eclass)
#grid2<-ggplot(ahc[ahc$eclass!="Forest-Hi",], aes(x=month, y=avg, colour=ElevationClass)) + 
#geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1) +
#geom_line() +
#geom_point()+xlab("Month")+ylab("Absolute Humidity (g/m3)")+ggtitle("Monthly Absolute Humidity \nby Elevation Class")+theme(
#plot.background = element_blank()
#,panel.background = element_blank()
#,panel.grid.major = element_blank()
#,panel.grid.minor = element_blank()
#,panel.border = element_blank()
#,axis.line.x = element_line(color = 'black')
#,axis.line.y = element_line(color = 'black')
#,text = element_text(size = 14)
#,axis.text.x=element_text(angle = 45,hjust=1)
#,legend.key = element_rect(colour = "white", fill = NA))
#ggsave(paste0(getwd(),"/Figures/AbsoluteHumidity.pdf"),grid2)
#scale_x_date(limits = as.Date(c('2014-06-01','2015-08-01')))

#vpc$ElevationClass<-factor(vpc$eclass)
#grid3<-ggplot(vpc[vpc$eclass!="Forest-Hi",], aes(x=month, y=max, colour=ElevationClass)) + geom_point()+
#geom_errorbar(aes(ymin=max-se, ymax=max+se), width=.1) +
#geom_line() +xlab("Month")+ylab("Vapour Pressure Deficit (hPa)")+ggtitle("Monthly Max Vapour Pressure Deficit \nby Elevation Class")+
#theme(
#plot.background = element_blank()
#,panel.background = element_blank()
#,panel.grid.major = element_blank()
#,panel.grid.minor = element_blank()
#,panel.border = element_blank()
#,axis.line.x = element_line(color = 'black')
#,axis.line.y = element_line(color = 'black')
#,text = element_text(size = 14)
#,axis.text.x=element_text(angle = 45,hjust=1)
#,legend.key = element_rect(colour = "white", fill = NA))
#ggsave(paste0(getwd(),"/Figures/VapourPressureDeficit.pdf"),grid3)

#scale_x_date(limits = as.Date(c('2014-06-01','2015-08-01')))
#vwcc$ElevationClass<-factor(vwcc$eclass)
#grid4<-ggplot(vwcc, aes(x=month, y=avg, colour=ElevationClass)) + 
#geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1) +
#geom_line() +
#geom_point()+xlab("Month")+ylab("Volumetric Water Content (Fraction)")+ggtitle(paste0("Monthly Volumetric Water Content \nby Elevation Class"))+
#theme(
#plot.background = element_blank()
#,panel.background = element_blank()
#,panel.grid.major = element_blank()
#,panel.grid.minor = element_blank()
#,panel.border = element_blank()
#,axis.line.x = element_line(color = 'black')
#,axis.line.y = element_line(color = 'black')
#,text = element_text(size = 14)
#,axis.text.x=element_text(angle = 45,hjust=1)
#,legend.key = element_rect(colour = "white", fill = NA))
#ggsave(paste0(getwd(),"/Figures/VolumetricWaterContent.pdf"),grid4)

#VPD vs psize
#ggplot(vpp, aes(x=month, y=max, colour=factor(psize))) + 
#geom_errorbar(aes(ymin=max-se, ymax=max+se), width=.1) +
#geom_line() +
#geom_point()+xlab("Month")+ylab("Vapour Pressure Deficit (hPa)")+ggtitle("Monthly Max Vapour Pressure Deficit \nby Patch Size")+theme(
#plot.background = element_blank()
#,panel.background = element_blank()
#,panel.grid.major = element_blank()
#,panel.grid.minor = element_blank()
#,panel.border = element_blank()
#,axis.line.x = element_line(color = 'black')
#,axis.line.y = element_line(color = 'black')
#,text = element_text(size = 14)
#,axis.text.x=element_text(angle = 45,hjust=1)
#,legend.key = element_rect(colour = "white", fill = NA))
#ggsave(paste0(getwd(),"/Figures/VPD_comparison_psize.pdf"))


tmax<-do.call(rbind.data.frame,TMAX)
#remove months before July 2014
tmax<-tmax[tmax[,1]>="2014-07-01",]
tmax[tmax$elevation>1750,"eclass"]<-"MAF"
tmax[tmax$elevation>1500&tmax$elevation<=1750,"eclass"]<-"TRF-Hi"
tmax[tmax$elevation<=1500,"eclass"]<-"TRF-Low"
#tmax[tmax$elevation<1500,6]<-"Low"
tmax[tmax$name=="ECO_1","eclass"]<-"Forest-Hi"
tmax[tmax$name=="ECO_6","eclass"]<-"Forest-Low"
colnames(tmax)<-c("month","avg","name","elevation","psize","kebele","eclass")
#tmax$psize<-as.character(tmax$psize)
#tmax[tmax$eclass=="Forest-Hi"|tmax$eclass=="Forest-Low","psize"]<-"Forest"
#tmax[tmax$psize=="Mid","psize"]<-"Small"

#remove -Inf values
tmax[tmax$avg=="-Inf"&!is.na(tmax$avg),"avg"]<-NA

#txc <- summarySE(tmax, measurevar="avg", groupvars=c("month","eclass"),na.rm=T)
#txp <- summarySE(tmax, measurevar="avg", groupvars=c("month","psize"),na.rm=T)

tmin<-do.call(rbind.data.frame,TMIN)
#remove months before July 2014
tmin<-tmin[tmin[,1]>="2014-07-01",]
tmin[tmin$elevation>1750,"eclass"]<-"MAF"
tmin[tmin$elevation>1500&tmin$elevation<=1750,"eclass"]<-"TRF-Hi"
tmin[tmin$elevation<=1500,"eclass"]<-"TRF-Low"
#tmin[tmin$elevation<1500,6]<-"Low"
tmin[tmin$name=="ECO_1","eclass"]<-"Forest-Hi"
tmin[tmin$name=="ECO_6","eclass"]<-"Forest-Low"
colnames(tmin)<-c("month","avg","name","elevation","psize","kebele","eclass")

#remove Inf values
tmin[tmin$avg=="Inf"&!is.na(tmin$avg),"avg"]<-NA

#tmc <- summarySE(tmin, measurevar="avg", groupvars=c("month","eclass"),na.rm=T)

tavg<-do.call(rbind.data.frame,TAVG)
#remove months before July 2014
tavg<-tavg[tavg[,1]>="2014-07-01",]
tavg[tavg$elevation>1750,"eclass"]<-"MAF"
tavg[tavg$elevation>1500&tavg$elevation<=1750,"eclass"]<-"TRF-Hi"
tavg[tavg$elevation<=1500,"eclass"]<-"TRF-Low"
#tavg[tavg$elevation<1500,6]<-"Low"
tavg[tavg$name=="ECO_1","eclass"]<-"Forest-Hi"
tavg[tavg$name=="ECO_6","eclass"]<-"Forest-Low"
colnames(tavg)<-c("month","avg","name","elevation","psize","kebele","eclass")
#tac <- summarySE(tavg, measurevar="avg", groupvars=c("month","eclass"),na.rm=T)

#txc$ElevationClass<-factor(txc$eclass)
#grid1<-ggplot(txc, aes(x=month, y=avg, colour=ElevationClass)) + 
#geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1) +
#geom_line() +
#geom_point()+xlab("Month")+ylab("Maximum Temperature (C)")+ggtitle("Monthly Maximum Temperature by Elevation Class")+
#theme(
#plot.background = element_blank()
#,panel.background = element_blank()
#,panel.grid.major = element_blank()
#,panel.grid.minor = element_blank()
#,panel.border = element_blank()
#,axis.line.x = element_line(color = 'black')
#,axis.line.y = element_line(color = 'black')
#,text = element_text(size = 14)
#,axis.text.x=element_text(angle = 45,hjust=1)
#,legend.key = element_rect(colour = "white", fill = NA))
  #scale_x_date(limits = as.Date(c('2014-06-01','2015-08-01')))
#ggsave(paste0(getwd(),"/Figures/MaxTemperature.pdf"),grid1)

#ggplot(txp, aes(x=month, y=avg, colour=factor(psize))) + 
#geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1) +
#geom_line() +
#geom_point()+xlab("Month")+ylab("Maximum Temperature (C)")+ggtitle("Monthly Maximum Temperature by Patch Size")+theme(
#plot.background = element_blank()
#,panel.background = element_blank()
#,panel.grid.major = element_blank()
#,panel.grid.minor = element_blank()
#,panel.border = element_blank()
#,axis.line.x = element_line(color = 'black')
#,axis.line.y = element_line(color = 'black')
#,text = element_text(size = 14)
#,axis.text.x=element_text(angle = 45,hjust=1)
#,legend.key = element_rect(colour = "white", fill = NA))
#ggsave("/Users/alex/Documents/Research/Africa/ECOLIMITS/Data/Yayu/MetData/Figures/Tmax_comparison_psize.pdf")

#grid2<-ggplot(tmc, aes(x=month, y=avg, colour=factor(eclass))) + 
#geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1) +
#geom_line() +
#geom_point()+xlab("Month")+ylab("Minimum Temperature (C)")+ggtitle("Monthly Minimum Temperature by Elevation Class")+
#theme(
#plot.background = element_blank()
#,panel.background = element_blank()
#,panel.grid.major = element_blank()
#,panel.grid.minor = element_blank()
#,panel.border = element_blank()
#,axis.line.x = element_line(color = 'black')
#,axis.line.y = element_line(color = 'black')
#,text = element_text(size = 14)
#,axis.text.x=element_text(angle = 45,hjust=1)
#,legend.key = element_rect(colour = "white", fill = NA))
#ggsave(paste0(getwd(),"/Figures/MinTemperature.pdf"),grid2)

#tac$ElevationClass<-factor(tac$eclass)
#grid3<-ggplot(tac, aes(x=month, y=avg, colour=ElevationClass)) + 
#geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1) +
#geom_line() +
#geom_point()+xlab("Month")+ylab("Average Temperature (C)")+ggtitle(paste0("Monthly Average Temperature by Elevation Class"))+
#theme(
#plot.background = element_blank()
#,panel.background = element_blank()
#,panel.grid.major = element_blank()
#,panel.grid.minor = element_blank()
#,panel.border = element_blank()
#,axis.line.x = element_line(color = 'black')
#,axis.line.y = element_line(color = 'black')
#,text = element_text(size = 14)
#,axis.text.x=element_text(angle = 45,hjust=1)
#,legend.key = element_rect(colour = "white", fill = NA))
#ggsave(paste0(getwd(),"/Figures/AverageTemperature.pdf"),grid3)
#scale_x_date(limits = as.Date(c('2014-06-01','2015-08-01')))

#**************

#combine metdata measures
dF<-cbind(ah,tavg$avg,tmax$avg,tmin$avg,vpd$max)
#canopy gap and microclimate measures
m<-as.character(unique(ah[,3]))

for(i in 1:length(m)){
  #find plot number
  if(length(grep("ECO_",m[i]))>0)  p<-as.character(names[names$DataLogger==m[i],"Plot"]) else p<-as.character(dnames[dnames$Datalogger.id==m[i],"Plot"])
  #assign "5" for forest measures
  if(length(grep("FC",p))>0) dF[dF$name==m[i],"Cavg"]<-5
  dF[dF$name==m[i],"Plot"]<-p
  if(length(grep("FC",p))>0) next
  #find canopy measures
  can<-canopy[canopy$Plot==p,"Synth"]
  if(length(can)>1) dF[dF$name==m[i],"Nov"]<-can[1] else dF[dF$name==m[i],strsplit(as.character(canopy[canopy$Plot==p,"Month"]),"-")[[1]][1]]<-can
  if(length(can)>1) dF[dF$name==m[i],"Jul"]<-can[2]
  dF[dF$name==m[i],"Cavg"]<-mean(can)
  
  
}
colnames(dF)<-c("month","ah","name","elevation","psize","kebele","eclass","tavg","tmax","tmin","vpd","Cavg","Plot","Nov","Jul")
write.csv(dF,paste0(getwd(),"/MetData/Monthly_metdata_withcanopygap.csv"))
