#calculating daily VPD for each plot from Campbell Sci and USB dataloggers
library(lubridate)
library(ggplot2)
library(plyr)
library(grid)
library(stringr)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/MetData/")

names<-data.frame(read.csv(paste0(getwd(),"/MS_names.csv")), stringsAsFactors=FALSE)
ns<-read.csv("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/plotnums.csv")
#load reference for T and VPD
#svp<-data.frame(read.csv(paste0(getwd(),"/T_SVP.csv")), stringsAsFactors=FALSE)
#based on equation SVP (Pa)= A*10^(mT/(T+Tn))
#A=6.116441, m=7.591386,Tn=240.7263
#VPD equation = (100-RH)/100*SVP
#AH equation = 2.16679*SVP/T(in K)
#time periods to average over:
brks<-data.frame(x1=0:2,x2=3:5,x3=6:8,x4=9:11,x5=12:14,x6=15:17,x7=18:20,x8=21:23)

root="ECO_"
nums=c(1:10)

for (i in 1:length(nums)){
  if(i == 7) next
  dat<-data.frame(read.csv(paste0(getwd(),"/",root,nums[i],".csv"), stringsAsFactors=FALSE))
  
  #remove 0 values
  #dat[dat$rh==0,"rh"]<-NA
  
  #calculate VPD
  dat$VPD=(100-as.numeric(dat[,5]))/100*6.116441*10^(7.591386*as.numeric(dat[,4])/(240.7263+as.numeric(dat[,4])))
  #calculate AH
  dat$AH=2.16679*6.116441*10^(7.591386*as.numeric(dat[,4])/(240.7263+as.numeric(dat[,4])))/(as.numeric(dat[,4])+273.15)*as.numeric(dat[,5])
  #find LWA values >500 kohms
  dat[,12]<-as.numeric(dat[,12])
  dat[is.na(dat[,12]),12]=0
  dat[as.numeric(dat[,12])>500,"LWA"]<-1
  dat[is.na(dat$LWA),"LWA"]<-0
  
  #generate dates
  dys<-as.POSIXct(dat[,1],format="%d/%m/%Y %H:%M")
  if(length(dys[is.na(dys)])==nrow(dat)|year(dys[5])<2014) dat[,1]<-as.POSIXct(dat[,1],format="%d/%m/%y %H:%M") else dat[,1]<-dys
  
  #seq1<-seq(as.POSIXct("2014-05-15 00:00:00"), as.POSIXct("2014-11-30 00:00:00"), by="day")
  #d <- which(outer(as.Date(seq1),as.Date(dat[,2]),"=="),arr.ind = TRUE)
  colnames(dat)<-c("date","no","voltage","temp","rh","vwc","ec","t","p","pa","vr","lw","vpd","ah","lwa")
  #remove extraneous rows
  dat<-na.omit(dat)
  
  #identify days
  dat$day<-as.Date(cut(dat[,1],breaks="day"))
  dates<-unique(dat$day)
  dat$month<-as.Date(cut(dat[,1],breaks="month"))
  #remove factors
  #dat<-data.frame(dat, stringsAsFactors=FALSE)
  #addNA(dat$vwc)
  #remove odd values
  dat$vwc[as.character(dat$vwc)>1]=NA
  dat$temp[as.numeric(dat$temp)>100]=NA
  dat$temp[as.numeric(dat$temp)< -50]=NA
  dat$vpd[as.numeric(dat$vpd)>100]=NA
  dat$ah[as.numeric(dat$ah)>100]=NA
  dat$rh[as.numeric(dat$rh)>100]=100
  #take 3 hour averages of all measures, for each day including diurnal Tmax and Tmin
  y<-dat[dat$day==dates[1],]
  y4<-y[hour(y[,1])>6&hour(y[,1])<=18,]
  d<-list()
  d1<-data.frame(cbind(max(y4$temp),min(y4$temp),max(y4$t),min(y4$t),max(y4$vpd),mean(as.numeric(y4$rh))))
  rm(y4)
  d2<-list()
  for(j in 1:ncol(brks)){
    y2<-data.frame(cbind(y,findInterval(hour(y[,1]),brks[,j])),stringsAsFactors=FALSE)
    y3<-data.frame(rbind(y2[y2[,ncol(y2)]==1,],y2[y2[,ncol(y2)]==2,]),stringsAsFactors=FALSE)
    y3<-data.frame(rbind(y3,y2[hour(y2[,1])==brks[3,j],]),stringsAsFactors=FALSE)
    y2<-lapply(y3[,4:15], as.numeric)
    y1<-data.frame(t(unlist(lapply(y2[1:(length(y2)-1)],mean,simplify=FALSE))),stringsAsFactors=FALSE)
    y1$lwa<-as.numeric(sum(y2$lwa)/2)
    d2[[j]]<-cbind(y1$temp,y1$vwc,y1$t,y1$vpd,y1$ah,y1$lwa)
    rm(y1,y2,y3)
  }
  d1[,7:(length(d2)*6+6)]<-data.frame(rbind(unlist(d2)))
  colnames(d1)<-c("Tmax","Tmin","Soiltmax","Soiltmin","VPDmax","RHmean","t1Tavg","t1vwc","t1soilT","t1vpd","t1ah","t1lwa","t2Tavg","t2vwc","t2soilT","t2vpd","t2ah","t2lwa","t3Tavg","t3vwc","t3soilT","t3vpd","t3ah","t3lwa","t4Tavg","t4vwc","t4soilT","t4vpd","t4ah","t4lwa","t5Tavg","t5vwc","t5soilT","t5vpd","t5ah","t5lwa","t6Tavg","t6vwc","t6soilT","t6vpd","t6ah","t6lwa","t7Tavg","t7vwc","t7soilT","t7vpd","t7ah","t7lwa","t8Tavg","t8vwc","t8soilT","t8vpd","t8ah","t8lwa")
  d1$day<-dates[1]
  d[[1]]<-d1
  rm(y,d2,d1)
  for(k in 2:length(dates)){
    y<-dat[dat$day==dates[k],]
    #get Tmin and Tmax
    y4<-y[hour(y[,1])>6&hour(y[,1])<=18,]
    d1<-data.frame(cbind(max(y4$temp),min(y4$temp),max(y4$t),min(y4$t),max(y4$vpd),mean(as.numeric(y4$rh))))
    d2<-list()
    rm(y4)
    for(j in 1:ncol(brks)){
      y2<-data.frame(cbind(y,findInterval(hour(y[,1]),brks[,j])),stringsAsFactors=FALSE)
      y3<-data.frame(rbind(y2[y2[,ncol(y2)]==1,],y2[y2[,ncol(y2)]==2,]),stringsAsFactors=FALSE)
      y3<-data.frame(rbind(y3,y2[hour(y2[,1])==brks[3,j],]),stringsAsFactors=FALSE)
      y2<-lapply(y3[,4:15], as.numeric)
      y1<-data.frame(t(unlist(lapply(y2[1:(length(y2)-1)],mean,simplify=FALSE))),stringsAsFactors=FALSE)
      y1$lwa<-as.numeric(sum(y2$lwa)/2)
      d2[[j]]<-cbind(y1$temp,y1$vwc,y1$t,y1$vpd,y1$ah,y1$lwa)
      rm(y1,y2,y3)
    }
    d1[,7:(length(d2)*6+6)]<-data.frame(rbind(unlist(d2)))
    colnames(d1)<-c("Tmax","Tmin","Soiltmax","Soiltmin","VPDmax","RHmean","t1Tavg","t1vwc","t1soilT","t1vpd","t1ah","t1lwa","t2Tavg","t2vwc","t2soilT","t2vpd","t2ah","t2lwa","t3Tavg","t3vwc","t3soilT","t3vpd","t3ah","t3lwa","t4Tavg","t4vwc","t4soilT","t4vpd","t4ah","t4lwa","t5Tavg","t5vwc","t5soilT","t5vpd","t5ah","t5lwa","t6Tavg","t6vwc","t6soilT","t6vpd","t6ah","t6lwa","t7Tavg","t7vwc","t7soilT","t7vpd","t7ah","t7lwa","t8Tavg","t8vwc","t8soilT","t8vpd","t8ah","t8lwa")
    d1$day<-dates[k]
    d[[k]]<-d1
    rm(y,d2,d1)
  }
  final<-do.call(rbind.data.frame,d)
  write.csv(final,paste0(getwd(),"/",root,nums[i],"_summary.csv"))
}

#based on equation SVP (Pa)= A*10^(mT/(T+Tn))
#A=6.116441, m=7.591386,Tn=240.7263
#VPD equation = (100-RH)/100*SVP
for (i in 11:12){
  met<-data.frame(read.csv(paste0(getwd(),"/",root,i,".csv"), stringsAsFactors=FALSE))
  #calculate VPD
  met$VPD=(100-as.numeric(met[,6]))/100*6.116441*10^(7.591386*as.numeric(met[,5])/(240.7263+as.numeric(met[,5])))
  #calculate AH
  met$AH=2.16679*6.116441*10^(7.591386*as.numeric(met[,5])/(240.7263+as.numeric(met[,5])))/(as.numeric(met[,5])+273.15)*as.numeric(met[,6])
  
  #remove odd values
  dat$temp[as.numeric(dat$temp)>100]=NA
  dat$temp[as.numeric(dat$temp)< -50]=NA
  dat$vpd[as.numeric(dat$vpd)>100]=NA
  dat$ah[as.numeric(dat$ah)>100]=NA
  dat$rh[as.numeric(dat$rh)>100]=100
  
  #generate dates
  dys<-as.POSIXct(met[,1],format="%d/%m/%Y %H:%M")
  if(length(dys[is.na(dys)])==nrow(met)) met[,1]<-as.POSIXct(met[,1],format="%d/%m/%y %H:%M") else met[,1]<-dys
  
  #seq1<-seq(as.POSIXct("2014-05-15 00:00:00"), as.POSIXct("2014-11-30 00:00:00"), by="day")
  #d <- which(outer(as.Date(seq1),as.Date(dat[,2]),"=="),arr.ind = TRUE)
  colnames(met)<-c("date","no","voltage","ptemp","temp","rh","slrkw","slrmj","wsms","wind.dir","ppt","vpd","ah")
  
  #identify days
  met$day<-as.Date(cut(met[,1],breaks="day"))
  dates<-unique(met$day)
  met$month<-as.Date(cut(met[,1],breaks="month"))
  #remove extraneous rows
  met<-na.omit(met)
  
  #take 3 hour averages of all measures, for each day including Tmax and Tmin
  y<-met[met$day==dates[1],]
  y4<-y[hour(y[,1])>6&hour(y[,1])<=18,]
  d<-list()
  d1<-data.frame(cbind(max(y4$temp),min(y4$temp),max(y4$vpd),mean(as.numeric(y4$rh)),sum(as.numeric(y$ppt))))
  d2<-list()
  rm(y4)
  for(j in 1:ncol(brks)){
    y2<-data.frame(cbind(y,findInterval(hour(y[,1]),brks[,j])),stringsAsFactors=FALSE)
    y3<-data.frame(rbind(y2[y2[,ncol(y2)]==1,],y2[y2[,ncol(y2)]==2,]),stringsAsFactors=FALSE)
    y3<-data.frame(rbind(y3,y2[hour(y2[,1])==brks[3,j],]),stringsAsFactors=FALSE)
    y2<-lapply(y3[,5:13], as.numeric)
    y1<-data.frame(t(unlist(lapply(y2[1:4],mean,simplify=FALSE))),stringsAsFactors=FALSE)
    y1$wsmw<-mean(na.omit(unlist(y2[5])))
    y1$wind.dir<-max(unlist(y2[6]))
    y1$ppt<-as.numeric(sum(y2$ppt))
    y1$vpd<-mean(y2$vpd)
    y1$ah<-mean(y2$ah)
    d2[[j]]<-cbind(y1$temp,y1$slrkw,y1$slrmj,y1$wsmw,y1$wind.dir,y1$ppt,y1$vpd,y1$ah)
    rm(y1,y2,y3)
  }
  d1[,6:(length(d2)*8+5)]<-data.frame(rbind(unlist(d2)))
  colnames(d1)<-c("Tmax","Tmin","VPDmax","RHmean","Tppt","t1Tavg","t1slrkw","t1slrmj","t1wsmw","t1wind.dir","t1ppt","t1vpd","t1ah","t2Tavg","t2slrkw","t2slrmj","t2wsmw","t2wind.dir","t2ppt","t2vpd","t2ah","t3Tavg","t3slrkw","t3slrmj","t3wsmw","t3wind.dir","t3ppt","t3vpd","t3ah","t4Tavg","t4slrkw","t4slrmj","t4wsmw","t4wind.dir","t4ppt","t4vpd","t4ah","t5Tavg","t5slrkw","t5slrmj","t5wsmw","t5wind.dir","t5ppt","t5vpd","t5ah","t6Tavg","t6slrkw","t6slrmj","t6wsmw","t6wind.dir","t6ppt","t6vpd","t6ah","t7Tavg","t7slrkw","t7slrmj","t7wsmw","t7wind.dir","t7ppt","t7vpd","t7ah","t8Tavg","t8slrkw","t8slrmj","t8wsmw","t8wind.dir","t8ppt","t8vpd","t8ah")
  d1$day<-dates[1]
  d[[1]]<-d1
  rm(y,d2,d1)
  for(k in 2:length(dates)){
    y<-met[met$day==dates[k],]
    y4<-y[hour(y[,1])>6&hour(y[,1])<=18,]
    #get Tmin and Tmax
    d1<-data.frame(cbind(max(y4$temp),min(y4$temp),max(y4$vpd),mean(as.numeric(y4$rh)),sum(as.numeric(y$ppt))))
    d2<-list()
    rm(y4)
    for(j in 1:ncol(brks)){
      y2<-data.frame(cbind(y,findInterval(hour(y[,1]),brks[,j])),stringsAsFactors=FALSE)
      y3<-data.frame(rbind(y2[y2[,ncol(y2)]==1,],y2[y2[,ncol(y2)]==2,]),stringsAsFactors=FALSE)
      y3<-data.frame(rbind(y3,y2[hour(y2[,1])==brks[3,j],]),stringsAsFactors=FALSE)
      y2<-lapply(y3[,5:13], as.numeric)
      y1<-data.frame(t(unlist(lapply(y2[1:4],mean,simplify=FALSE))),stringsAsFactors=FALSE)
      y1$wsmw<-mean(na.omit(unlist(y2[5])))
      y1$wind.dir<-max(unlist(y2[6]))
      y1$ppt<-as.numeric(sum(y2$ppt))
      y1$vpd<-mean(y2$vpd)
      y1$ah<-mean(y2$ah)
      d2[[j]]<-cbind(y1$temp,y1$slrkw,y1$slrmj,y1$wsmw,y1$wind.dir,y1$ppt,y1$vpd,y1$ah)
      rm(y1,y2,y3)
    }
    d1[,6:(length(d2)*8+5)]<-data.frame(rbind(unlist(d2)))
    colnames(d1)<-c("Tmax","Tmin","VPDmax","RHmean","Tppt","t1Tavg","t1slrkw","t1slrmj","t1wsmw","t1wind.dir","t1ppt","t1vpd","t1ah","t2Tavg","t2slrkw","t2slrmj","t2wsmw","t2wind.dir","t2ppt","t2vpd","t2ah","t3Tavg","t3slrkw","t3slrmj","t3wsmw","t3wind.dir","t3ppt","t3vpd","t3ah","t4Tavg","t4slrkw","t4slrmj","t4wsmw","t4wind.dir","t4ppt","t4vpd","t4ah","t5Tavg","t5slrkw","t5slrmj","t5wsmw","t5wind.dir","t5ppt","t5vpd","t5ah","t6Tavg","t6slrkw","t6slrmj","t6wsmw","t6wind.dir","t6ppt","t6vpd","t6ah","t7Tavg","t7slrkw","t7slrmj","t7wsmw","t7wind.dir","t7ppt","t7vpd","t7ah","t8Tavg","t8slrkw","t8slrmj","t8wsmw","t8wind.dir","t8ppt","t8vpd","t8ah")
    
    d1$day<-dates[k]
    d[[k]]<-d1
    rm(y,d2,d1)
  }
  final<-do.call(rbind.data.frame,d)
  write.csv(final,paste0(getwd(),"/",root,i,"_summary.csv"))
}


############USB dataloggers
#root="NCRC_"
#find list of files
names<-list.files(path=paste0(getwd(),"/USB_raw"),pattern=".csv",all.files = FALSE,full.names = TRUE)

#load datalogger names
name<-read.csv(paste0(getwd(),"/DL_names.csv"))
name[is.na(name$Time.offset),"Time.offset"]<-0
#remove "no data" lines
name<-name[name[,4]!="no data",]
#ns<-read.csv("/Users/alex/Documents/Research/Africa/ECOLIMITS/Data/Kakum/plots.csv")
combo<-as.data.frame(matrix(nrow=length(name[,1]),ncol=7),stringsAsFactors =FALSE)
for(i in 1:length(name[,1])){
  p<-grep(as.character(name[i,3]),ns$name)
  if(as.character(name[i,3])=="H1") p<-p[1]
  combo[i,1:3]<-cbind(as.character(name[i,2]),as.character(name[i,3]),as.numeric(name[i,5]))
  combo[i,4]<-cbind(as.character(ns[p,5]))
  combo[i,5:7]<-cbind(ns[p,2],ns[p,3],ns[p,6])
}
rm(p)

for(i in 1:length(names)){
  met<-data.frame(read.csv(names[i]), stringsAsFactors=FALSE)
  #find plot number and start date
  p<-str_split(str_split(names[i],"USB_raw/")[[1]][2],".csv")
  p<-p[[1]][p[[1]]!=""]
  n<-as.Date(name[grep(p,as.character(name$Datalogger.id)),"Date"],format="%d/%m/%Y")+1
  if(p=="NCRC_1") n=n[1]
  
  #identify days
  time<-as.POSIXct(met[,2],format="%d/%m/%Y %H:%M")
  if(is.na(time[1])) time<-as.POSIXct(met[,2],format="%d/%m/%Y %H:%M")
  met$day<-as.Date(cut(time,breaks="day"))
  met$month<-as.Date(cut(time,breaks="month"))
  #extract data collected after start date
  out<-match(n,met$day)
  if(is.na(out)&min(met$day,na.rm=T)>n) out<-match(min(met$day,na.rm=T)+1,met$day)
  if(is.na(out)) next
  
  met<-met[out:length(met[,1]),]
  dates<-unique(met$day)
  
  #calculate VPD
  met$VPD=(100-as.numeric(met[,4]))/100*6.116441*10^(7.591386*as.numeric(met[,3])/(240.7263+as.numeric(met[,3])))
  #calculate AH
  met$AH=2.16679*6.116441*10^(7.591386*as.numeric(met[,3])/(240.7263+as.numeric(met[,3])))/(as.numeric(met[,3])+273.15)*as.numeric(met[,4])
  colnames(met)<-c("no","time","temp","rh","dew","sno","day","month","vpd","ah")
  
  #remove odd values
  dat$temp[as.numeric(dat$temp)>100]=NA
  dat$temp[as.numeric(dat$temp)< -50]=NA
  dat$vpd[as.numeric(dat$vpd)>100]=NA
  dat$ah[as.numeric(dat$ah)>100]=NA
  dat$rh[as.numeric(dat$rh)>100]=100
  
  #take 3 hour averages of all measures, for each day including Tmax and Tmin
  y<-met[met$day==dates[1],]
  y$time<-as.POSIXct(y$time,format="%d/%m/%y %H:%M")
  #remove NA
  y<-y[!is.na(y[,2]),]
  d<-list()
  y4<-y[hour(y$time)>6&hour(y$time)<=18,]
  d1<-data.frame(cbind(max(y4$temp),min(y4$temp),max(y4$vpd),mean(as.numeric(y4$rh))))
  d2<-list()
  rm(y4)
  for(j in 1:ncol(brks)){
    y2<-data.frame(cbind(y,findInterval(hour(y$time),brks[,j])),stringsAsFactors=FALSE)
    #remove NA
    y2<-y2[!is.na(y2[,ncol(y2)]),]
    y3<-data.frame(rbind(y2[y2[,ncol(y2)]==1,],y2[y2[,ncol(y2)]==2,]),stringsAsFactors=FALSE)
    y3<-data.frame(rbind(y3,y2[hour(y2$time)==brks[3,j],]),stringsAsFactors=FALSE)
    y2<-data.frame(cbind(y3$temp,y3$vpd,y3$ah),stringsAsFactors=FALSE)
    y2<-lapply(y2, as.numeric)
    y1<-data.frame(t(unlist(lapply(y2[1:(length(y2))],mean,simplify=FALSE))),stringsAsFactors=FALSE)
    
    d2[[j]]<-cbind(y1$X1,y1$X2,y1$X3)
    rm(y1,y2,y3)
  }
  d1[,5:(length(d2)*3+4)]<-data.frame(rbind(unlist(d2)))
  colnames(d1)<-c("Tmax","Tmin","VPDmax","RHmean","t1Tavg","t1vpd","t1ah","t2Tavg","t2vpd","t2ah","t3Tavg","t3vpd","t3ah","t4Tavg","t4vpd","t4ah","t5Tavg","t5vpd","t5ah","t6Tavg","t6vpd","t6ah","t7Tavg","t7vpd","t7ah","t8Tavg","t8vpd","t8ah")
  d1$day<-dates[1]
  d[[1]]<-d1
  rm(y,d2,d1)
  for(k in 2:length(dates)){
    y<-met[met$day==dates[k],]
    y$time<-as.POSIXct(y$time,format="%d/%m/%Y %H:%M")
    #remove NA
    y<-y[!is.na(y[,2]),]
    y4<-y[hour(y$time)>6&hour(y$time)<=18,]
    #get Tmin and Tmax
    d1<-data.frame(cbind(max(y4$temp),min(y4$temp),max(y4$vpd),mean(as.numeric(y4$rh))))
    d2<-list()
    rm(y4)
    for(j in 1:ncol(brks)){
      y2<-data.frame(cbind(y,findInterval(hour(y$time),brks[,j])),stringsAsFactors=FALSE)
      #remove NA
      y2<-y2[!is.na(y2[,ncol(y2)]),]
      y3<-data.frame(rbind(y2[y2[,ncol(y2)]==1,],y2[y2[,ncol(y2)]==2,]),stringsAsFactors=FALSE)
      y3<-data.frame(rbind(y3,y2[hour(y2$time)==brks[3,j],]),stringsAsFactors=FALSE)
      y2<-data.frame(cbind(y3$temp,y3$vpd,y3$ah),stringsAsFactors=FALSE)
      y2<-lapply(y2, as.numeric)
      y1<-data.frame(t(unlist(lapply(y2[1:(length(y2))],mean,simplify=FALSE))),stringsAsFactors=FALSE)
      
      d2[[j]]<-cbind(y1$X1,y1$X2,y1$X3)
      rm(y1,y2,y3)
    }
    d1[,5:(length(d2)*3+4)]<-data.frame(rbind(unlist(d2)))
    colnames(d1)<-c("Tmax","Tmin","VPDmax","RHmean","t1Tavg","t1vpd","t1ah","t2Tavg","t2vpd","t2ah","t3Tavg","t3vpd","t3ah","t4Tavg","t4vpd","t4ah","t5Tavg","t5vpd","t5ah","t6Tavg","t6vpd","t6ah","t7Tavg","t7vpd","t7ah","t8Tavg","t8vpd","t8ah")
    d1$day<-dates[k]
    d[[k]]<-d1
    rm(y,d2,d1)
  }
  final<-do.call(rbind.data.frame,d)
 
  #g2<-strsplit(g[[1]][2],".csv")
  #as.character(name[name[,3]==paste0(root,g2[[1]][1]),2])
  write.csv(final,paste0(getwd(),"/summary/",p,"_summary.csv"))
  
  #ms<-paste0(root,nums[i])
  #pnum<-as.character(names[names[,2]==ms,1])
  #generate dates
  #dtm=as.POSIXct("2014-05-15 05:00", tz="GMT")+3600*(1:240)
  #seq1<-seq(as.POSIXct("2014-05-15 00:00:00"), as.POSIXct("2014-11-30 00:00:00"), by="day")
  #d <- which(outer(as.Date(seq1),as.Date(data[,2]),"=="),arr.ind = TRUE)
  
}
