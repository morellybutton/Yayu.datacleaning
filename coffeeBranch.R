#combination of monitored branches over year (from flowers to yield)

library(gdata)
library(stringr)
library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")

years<-c("2014","2015","2016")

plts<-read.csv(paste0(getwd(),"/plotnums.csv"))
#remove Met station and Forest plots
plts<-plts[grep("Met_",plts$name,invert=T),]
plts<-plts[grep("FC",plts$name,invert=T),]
p<-as.character(plts$name)
p<-p[p!="H7"]

for(j in 1:length(years)){
  for(i in 1:length(p)){
    #open flower data
    if(years[j]!=2014) dataFs <- data.frame(read.xls(paste0(getwd(),"/Yield/",years[j],"/FruitSet_Combined.xlsx"), sheet=p[i]),stringsAsFactors = F) else dataFs<-data.frame(Plot=as.character(),Coordinate=as.character(),Shrub.id=as.character(),Branch.cm=as.character(),No.of.buds=as.character(),No.of.flower=as.character(),No.of.eberry=as.character(),No.of.leaves=as.character(),NoCLR=as.character(),iCLR=as.character(),NoChl=as.character(),NoHerb=as.character(),SoilMoist=as.character(),Notes=as.character())
    #add column names
    if(nrow(dataFs)!=0) {
      colnames(dataFs)<-c("Plot","Coordinate","Shrub.id","Branch.cm","No.of.buds","No.of.flower","No.of.eberry","No.of.leaves","NoCLR","iCLR","NoChl","NoHerb","SoilMoist","Notes") 
      #find entry days
      d1 <- data.frame(grep("DATE",dataFs[,4]))
      d1[(nrow(d1)+1),1]<-nrow(dataFs)
      d1$date<-as.Date(dataFs[d1[,1],5])
      dataFs$Date<-NA
      for(x in 1:(nrow(d1)-1)){
        if(x < (nrow(d1)-1)) dataFs[(d1[x,1]+2):(d1[(x+1),1]-1),"Date"]<-d1[x,2] else dataFs[(d1[x,1]+2):(d1[(x+1),1]),"Date"]<-d1[x,2]
      }
      dataFs <- dataFs[!is.na(dataFs$Date),]
      dataFs$Date<-as.Date(dataFs$Date,origin = "1970-01-01")
      dataFs<-data.frame(sapply(dataFs,as.character),stringsAsFactors = F)
    }
    
    #open disease data
    dataBr <- data.frame(read.xls(paste0(getwd(),"/Disease/",years[j],"/Disease Survey_Combined.xlsx"), sheet=paste0(p[i]," (branches)")),stringsAsFactors = F)
    dataBr <- data.frame(lapply(dataBr, as.character), stringsAsFactors=FALSE)
    dataBr<-dataBr[,!is.na(dataBr[1,])]
    #add column names
    colnames(dataBr)<-c("Plot","Coordinate","Shrub.id","Branch.cm","No.of.fruits","NoCBB","NoCBD","No.of.leaves","NoLM","NoCLR","iCLR","NoChl","NoWilt","NoHerb","Notes")
    #find entry days
    d1 <- data.frame(grep("DATE",dataBr[,4]))
    d1[(nrow(d1)+1),1]<-nrow(dataBr)
    d1$date<-as.Date(dataBr[d1[,1],5])
    for(x in 1:(nrow(d1)-1)){
      if(x < (nrow(d1)-1)) dataBr[(d1[x,1]+2):(d1[(x+1),1]-1),"Date"]<-d1[x,2] else dataBr[(d1[x,1]+2):(d1[(x+1),1]),"Date"]<-d1[x,2]
    }
    dataBr$Date<-as.Date(dataBr$Date,origin = "1970-01-01")
    dataBr <- dataBr[!is.na(dataBr$Date),]
    dataBr <- dataBr[!is.na(dataBr$Plot),]
    dataBr<-data.frame(sapply(dataBr,as.character),stringsAsFactors = F)
    
    branch<-data_frame(plot=c(dataFs$Plot,dataBr$Plot),
               Shrub.id=c(dataFs$Shrub.id,dataBr$Shrub.id),
               Branch.cm=c(dataFs$Branch.cm,dataBr$Branch.cm),
               No.of.buds=c(dataFs$No.of.buds,rep(0,nrow(dataBr))),
               No.of.flower=c(dataFs$No.of.flower,rep(0,nrow(dataBr))),
               No.of.eberry=c(dataFs$No.of.eberry,rep(0,nrow(dataBr))),
               No.of.fruits=c(rep(0,nrow(dataFs)),dataBr$No.of.fruits),
               NoCBB=c(rep(0,nrow(dataFs)),dataBr$NoCBB),
               NoCBD=c(rep(0,nrow(dataFs)),dataBr$NoCBD),
               No.of.leaves=c(dataFs$No.of.leaves,dataBr$No.of.leaves),
               NoLM=c(rep(0,nrow(dataFs)),dataBr$NoLM),
               NoCLR=c(dataFs$NoCLR,dataBr$NoCLR),
               iCLR=c(dataFs$iCLR,dataBr$iCLR),
               NoChl=c(dataFs$NoChl,dataBr$NoChl),
               NoWilt=c(rep(0,nrow(dataFs)),dataBr$NoWilt),
               NoHerb=c(dataFs$NoHerb,dataBr$NoHerb),
               Notes=c(dataFs$Notes,dataBr$Notes),
               Date=c(dataFs$Date,dataBr$Date))
    
    tmp<- branch %>% arrange(Shrub.id,Branch.cm)
    write.csv(tmp,paste0(getwd(),"/Yield/",years[j],"/Branchdata_",p[i],".csv"))
  }
}
