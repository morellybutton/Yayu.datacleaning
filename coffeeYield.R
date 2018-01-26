#code for collecting/cleaning yield data and calculating per ha values

library(gdata)
library(stringr)
library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/")

#dates<-"nov14"
years<-c("2014","2015","2016")


#average berry weights by color (g)
red<-1.211
green<-0.987
other<-0.374

plts<-read.csv(paste0(getwd(),"/plotnums.csv"))
#remove Met station and Forest plots
plts<-plts[grep("Met_",plts$name,invert=T),]
plts<-plts[grep("FC",plts$name,invert=T),]

p<-as.character(plts$name)

#open coffee density measures
c.dens<-read.csv(paste0(getwd(),"/Yield/Coffee.density_2015_cleaned.csv"))


for(j in 1:length(years)){
  results<-list()
  y.ld<-list()
  y.ld.shr<-list()
  #open shrub DBH measures
  if(years[j]==2016) dataSh<-read.csv(paste0(getwd(),"/Yield/Shrub.data_2015_cleaned.csv")) else dataSh<-read.csv(paste0(getwd(),"/Yield/Shrub.data_",years[j],"_cleaned.csv"))
  dataSh<-data_frame(Plot=as.character(dataSh$Plot), Shrub.id=as.character(dataSh$ShrubId),DBH1.3=as.character(dataSh$DBH1.3),DBH1.3x=dataSh$DBH1.3x,DBH40=dataSh$DBH40,DBH40x=dataSh$DBH40x)
  ########count number of shoots on shrub from DBH 1.3
  dataSh <- dataSh %>% group_by(Plot,Shrub.id) %>% mutate(No.shr=gregexpr(",",DBH1.3[1])[[1]][1])
  dataSh <- dataSh %>% group_by(Plot,Shrub.id) %>% mutate(No.shr=replace(No.shr,No.shr!=-1,length(gregexpr(",",DBH1.3[1])[[1]]))+1) %>%
    mutate(No.shr=replace(No.shr,No.shr==0,1)) %>% ungroup()
  
  for(i in 1:length(p)){
    if(p[i]=="H7") next
    if(p[i]=="H11") next
    
    #open yield files
    dataY<-data.frame(read.xls(paste0(getwd(),"/Yield/",years[j],"/Yield_Combined.xlsx"), sheet=p[i]),stringsAsFactors = F)
    dataY<-dataY[,!is.na(dataY[1,])]
    
    if(years[j]==2014) colnames(dataY)<-c("Shrub.id","Branch","Redwt.g","Greenwt.g","Otherwt.g") else colnames(dataY)<-c("Shrub.id","Branch","Redwt.g","Greenwt.g","Otherwt.g","NoCBB")
    dataY$Shrub.id<-gsub("\\*","",dataY$Shrub.id)
    #find start of data
    s<-grep("Shrub id",dataY[,1])
    #pull out actual data rows
    y<-dataY[(s+1):length(dataY[,1]),]
    y<-data.frame(sapply(y,as.character),stringsAsFactors = F)
    
    #create empty dataframe for results
    if(years[j]==2014) yield<-data.frame(Plot=character(),ShrubId=character(),Branch.cm=character(),Redwt.g=numeric(),Greenwt.g=numeric(),Otherwt.g=numeric(),Berry.no=numeric(),stringsAsFactors=FALSE) else yield<-data.frame(Plot=character(),ShrubId=character(),Branch.cm=character(),Redwt.g=numeric(),Greenwt.g=numeric(),Otherwt.g=numeric(),Berry.no=numeric(),NoCBB=numeric(),stringsAsFactors=FALSE)
    shr<-as.character(unique(y$Shrub.id)) 
    #remove blanks and "Total" from shrub id
    shr<-shr[shr!=""&shr!="Total"]
    
    if(length(shr)>7) break
    d.f.s<-list()
    shr<-gsub("\\*","",shr)
    for(k in 1:length(shr)){
      b<-y[y$Shrub.id==shr[k],]
      for(m in 1:nrow(b)){
        yield[4*(k-1)+m,1]<-p[i]
        yield[4*(k-1)+m,2]<-shr[k]
        yield[4*(k-1)+m,3]<-gsub("cm","",as.character(b[m,2]))
        if(length(grep("\\)",as.character(b[m,3])))>0) yield[4*(k-1)+m,4]<-as.numeric(gsub("\\)","",as.character(b[m,3])))*red else {yield[4*(k-1)+m,4]<-as.numeric(as.character(b[m,3])); b[m,3]<-floor(as.numeric(as.character(b[m,3]))/red)}
        if(length(grep("\\)",as.character(b[m,4])))>0) yield[4*(k-1)+m,5]<-as.numeric(gsub("\\)","",as.character(b[m,4])))*green else {yield[4*(k-1)+m,5]<-as.numeric(as.character(b[m,4])); b[m,4]<-floor(as.numeric(as.character(b[m,4]))/green)}
        if(length(grep("\\)",as.character(b[m,5])))>0) yield[4*(k-1)+m,6]<-as.numeric(gsub("\\)","",as.character(b[m,5])))*other else yield[4*(k-1)+m,6]<-as.numeric(as.character(b[m,5]))
        #count number of berries
        if(m<4) yield[4*(k-1)+m,7]<-sum(c(as.numeric(gsub("\\)","",as.character(b[m,3]))),as.numeric(gsub("\\)","",as.character(b[m,4])))))
        if(years[j]!=2014) yield[4*(k-1)+m,8]<-as.numeric(as.character(b[m,6]))
        }
      }
    #save berry counts for comparison with branch measures
    results[[i]]<-yield
    
    #calculate hectare estimates
    yield<-data_frame(Plot=yield$Plot,Shrub.id=yield$ShrubId, Branch=yield$Branch.cm, Redwt=yield$Redwt.g, Greenwt=yield$Greenwt.g)
    #estimate total production of shrub (multiply total by number of shrubs)
    total<- yield %>% filter(Branch=="Whole tree") %>% group_by(Shrub.id) %>% mutate(wt=sum(Redwt,Greenwt))
    y.ld.shr[[i]]<-total
    #multiply shoot yield estimate by number of shoots taken from survey
    #total <- left_join(total, dataSh %>% select(Plot,Shrub.id,No.shr),by=c("Plot","Shrub.id"))
    #insert average number of shoots per shrub for missing values
    #g<-floor(mean(total$No.shr,na.rm=T))
    #total <- total %>% mutate(No.shr = replace(No.shr,is.na(No.shr),g)) %>% group_by(Shrub.id) %>%
    #mutate(Tot.shr.kg=wt*No.shr/1000)
    
    #take median per shoot production
    kg<-median(total$wt/1000,na.rm=T)
    
    #calculate Ha estimate using coffee density (multiply by number of shoots per shrub divide by area)
    d<-c.dens[c.dens$Plot == p[i],]
    #create tibble
    #d<-data_frame(Plot=as.character(d$Plot),SPlot=as.character(d$SPlotNo),DBH1.3=as.character(d$DBH1.3),TotShoots=d$TotShoots)
    #area (in ha) over which coffee was monitored
    area=length(unique(d$SPlot))*0.2*0.2
    ha.kg<-data_frame(Plot=p[i], area=area, shoot.kg=kg, no.sht=sum(d$TotShoots,na.rm=T), Total.kg=sum(d$TotShoots,na.rm=T)*kg/area)
    y.ld[[i]]<-ha.kg
  }
  final<-do.call(rbind.data.frame,results)
  write.csv(final,paste0(getwd(),"/Yield/Final.BerryCounts_",years[j],".csv"))
  yields<-do.call(rbind.data.frame,y.ld)
  write.csv(yields,paste0(getwd(),"/Yield/Plot.ha.yields_",years[j],".csv"))
  yields.shrub<-do.call(rbind.data.frame,y.ld.shr)
  write.csv(yields.shrub,paste0(getwd(),"/Yield/Shoot.ha.yields_",years[j],".csv"))
}


