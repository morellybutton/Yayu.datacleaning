#combining shrub data for monitored plots, producing "average shrub" per plot and calculating coffee density per plot

library(gdata)
library(stringr)
#library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")

years<-c("2014","2015")

plts<-read.csv(paste0(getwd(),"/plotnums.csv"))
#remove Met station and Forest plots
plts<-plts[grep("Met_",plts$name,invert=T),]
plts<-plts[grep("FC",plts$name,invert=T),]
p<-as.character(plts$name)
p<-p[p!="H7"]


for(j in 1:length(years)){
  result.1<-list()
  for(i in 1:length(p)){
    if(p[i]=="H11") next
    
    dataSh <- data.frame(read.xls(paste0(getwd(),"/Disease/",years[j],"/Disease Survey_Combined.xlsx"), sheet=paste0(p[i]," (shrub)")),stringsAsFactors = F)
    #find start of data
    s1 <- grep("Plot",dataSh[,1])+1
    dataSh <- dataSh[s1:nrow(dataSh),]
    if(years[j]!=2014) colnames(dataSh)<-c("Plot","Coordinate","Shrub.id","Wilt","Height","DBH.1.3","DBH.1.3x","DBH40","DBH40x","CDiameter.x","CDiameter.y","Soil.moisture","Totshoots","Deadshoots","Missingshoots","Notes") else colnames(dataSh)<-c("Plot","Coordinate","Shrub.id","Wilt","Height","DBH.1.3","DBH40","CDiameter.x","CDiameter.y","Soil.moisture","Totshoots","Deadshoots","Missingshoots")
    rm(s1)
    #create empty dataframe for results
    result<-data.frame(Plot=character(),ShrubNo=character(),ShrubId=character(),Height=numeric(),DBH1.3=character(),DBH1.3x=character(),DBH40=character(),DBH40x=character(),Xcrown=numeric(),Ycrown=numeric(),Soil.moisture=numeric(),TotShoots=numeric(),DeadShoots=numeric(),MissingShoots=numeric(),stringsAsFactors=FALSE)
    
    for(k in 1:nrow(dataSh)){
      shrub<-dataSh[k,]
      result[k,1:3]<-data.frame(lapply(shrub[,1:3], as.character), stringsAsFactors=FALSE)
      result$Height[k]<-as.character(shrub$Height)
      #find number of shoots per shrub
      dbh<-as.character(shrub$DBH.1.3)
      ind<-gregexpr("/",dbh)
      if(ind[[1]][1]==-1) br=dbh else br<-substr(dbh, 1, ind[[1]][1]-1)
      ind2<-gregexpr(",",br)
      one<-as.numeric(substr(br, 1, ind2[[1]][1]-1))
      two<-as.numeric(substr(br, ind2[[1]][1]+1,nchar(br)))
      if(is.na(one)|is.na(two)) DBH<-sum(one,two,na.rm=T) else DBH <- mean(one,two,na.rm=T)
        
      for(m in 1:length(ind[[1]])){
        {if(ind[[1]][1]==-1) next();}
        br<-substr(dbh, ind[[1]][m]+1, ind[[1]][m+1]-1)
        if(m==length(ind[[1]])) br<-substr(dbh, ind[[1]][m]+1,nchar(dbh))
        #test[m]<-br
        ind2<-gregexpr(",",br)
        if(ind2[[1]][1]==-1) dbh1=br else {
          one<-as.numeric(substr(br, 1, ind2[[1]][1]-1))
          two<-as.numeric(substr(br, ind2[[1]][1]+1,nchar(br)))
          dbh1<-sum(one,two)/2
          }
        DBH<-paste(DBH,dbh1,sep=",")
        }
      rm(m,one,two,br,ind,ind2)
      if(is.na(DBH)) result[k,5]<-"not collected" else result[k,5]<-DBH
      rm(DBH,dbh1,dbh)
      
      dbh<-as.character(shrub$DBH.1.3x)
      if(nrow(shrub)==0) DBH<-signif(mean(unlist(lapply(str_split(result$DBH1.3[k],","),as.numeric)),na.rm=T),3) else {
        if(years[j]!=2014){
          ind2<-gregexpr(",",dbh)
          one<-as.numeric(substr(dbh, 1, ind2[[1]][1]-1))
          two<-as.numeric(substr(dbh, ind2[[1]][1]+1,nchar(dbh)))
          DBH<-sum(one,two)/2
        } else
          DBH<-mean(as.numeric(str_split(result[k,5],",")[[1]]),na.rm=T)
      }
      result[k,6]<-DBH
      rm(DBH,dbh)
      
      dbh<-as.character(shrub$DBH40)
      if(nrow(shrub)==0) DBH<-paste(rep(signif(mean(unlist(lapply(str_split(result$DBH40,","),as.numeric)),na.rm=T),3),round(mean(unlist(lapply(lapply(str_split(result$DBH40,","),as.numeric),length)),na.rm=T))),collapse=",") else {
        ind<-gregexpr("/",dbh)
        #{if(length(ind)==0) next(); 
        if(ind[[1]][1]==-1) br<-dbh else br<-substr(dbh, 1, ind[[1]][1]-1)
        #DBH<-c()
        ind2<-gregexpr(",",br)
        if(ind2[[1]][1]==-1) one<-as.numeric(br) else one<-as.numeric(substr(br, 1, ind2[[1]][1]-1))
        if(ind2[[1]][1]==-1) two<-as.numeric(br) else two<-as.numeric(substr(br, ind2[[1]][1]+1,nchar(br)))
        DBH<-sum(one,two)/2
        rm(one,two)
        for(m in 1:length(ind[[1]][1])){
          {if(ind[[1]][1]==-1) next();}
          br<-substr(dbh, ind[[1]][m]+1, ind[[1]][m+1]-1)
          if(m==length(ind[[1]])) br<-substr(dbh, ind[[1]][m]+1,nchar(dbh))
          #test[m]<-br
          ind2<-gregexpr(",",br)
          if(ind2[[1]][1]==-1) one<-as.numeric(br) else one<-as.numeric(substr(br, 1, ind2[[1]][1]-1))
          if(ind2[[1]][1]==-1) two<-as.numeric(br) else two<-as.numeric(substr(br, ind2[[1]][1]+1,nchar(br)))
          dbh1<-sum(one,two)/2
          DBH<-paste(DBH,dbh1,sep=",")
        }
        rm(m,one,two,br,ind,ind2)
      }
      if(is.na(DBH)) result[k,7]<-"not collected" else result[k,7]<-DBH
      
      dbh<-as.character(shrub$DBH40x)
      if(nrow(shrub)==0) DBH<-signif(mean(unlist(lapply(str_split(result$DBH40[k],","),as.numeric)),na.rm=T),3) else {
        if(years[j]!=2014){
          ind2<-gregexpr(",",dbh)
          one<-as.numeric(substr(dbh, 1, ind2[[1]][1]-1))
          two<-as.numeric(substr(dbh, ind2[[1]][1]+1,nchar(dbh)))
          DBH<-sum(one,two)/2
        } else 
          DBH<-mean(as.numeric(str_split(result[k,7],",")[[1]]),na.rm=T)
      }
      result[k,8]<-DBH
      
      if(years[j]!=2014&nrow(shrub)>0) result[k,9:14]<-data.frame(lapply(shrub[,10:15], as.character), stringsAsFactors=FALSE) 
      if(years[j]==2014&nrow(shrub)>0) result[k,9:14]<-data.frame(lapply(shrub[,8:13], as.character), stringsAsFactors=FALSE)
      if(years[j]==2014&nrow(shrub)==0) result[k,9:14]<-c(mean(as.numeric(result$Xcrown)),mean(as.numeric(result$Ycrown)),NA,NA,NA,NA)
    }
   result.1[[i]]<-result
  }
  result1<-do.call(rbind.data.frame,result.1)
  write.csv(result1,paste0(getwd(),"/Yield/Shrub.data_",years[j],"_cleaned.csv"))
}


#do again for Coffee Density (only collected in 2015)
for(i in 1:length(p)){
  #open coffee density measures
  dataCD <- data.frame(read.xls(paste0(getwd(),"/Yield/2015/Coffee density data final.xlsx"), sheet=p[i]),stringsAsFactors = F)
  dataCD<-dataCD[,!is.na(dataCD[1,])]
  #find start of data
  s1 <- grep("Plot",dataCD[,1])+1
  colnames(dataCD)<-c("Plot","Subplot","Shrub.id","Height","DBH.1.3","DBH40","CDiameter.x","CDiameter.y","Notes")
  dataCD <- dataCD[s1:nrow(dataCD),]
  
  #create empty dataframe for results
  result<-data.frame(Plot=character(),SPlotNo=character(),ShrubId=character(),Height=numeric(),DBH1.3=character(),DBH40=character(),Xcrown=numeric(),Ycrown=numeric(),TotShoots=numeric(),stringsAsFactors=FALSE) 
  for(j in 1:nrow(dataCD)){
    result[j,1:4]<-data.frame(lapply(dataCD[j,1:4], as.character), stringsAsFactors=FALSE)
    #find number of shoots per shrub
    dbh<-as.character(dataCD[j,5])
    ind<-gregexpr("/",dbh)
    if(ind[[1]][1]==-1) br=dbh else br<-substr(dbh, 1, ind[[1]][1]-1)
    ind2<-gregexpr(",",br)
    one<-as.numeric(substr(br, 1, ind2[[1]][1]-1))
    two<-as.numeric(substr(br, ind2[[1]][1]+1,nchar(br)))
    DBH<-sum(one,two)/2
    for(m in 1:length(ind[[1]])){
      {if(ind[[1]][1]==-1) next();}
      br<-substr(dbh, ind[[1]][m]+1, ind[[1]][m+1]-1)
      if(m==length(ind[[1]])) br<-substr(dbh, ind[[1]][m]+1,nchar(dbh))
      #test[m]<-br
      ind2<-gregexpr(",",br)
      one<-as.numeric(substr(br, 1, ind2[[1]][1]-1))
      two<-as.numeric(substr(br, ind2[[1]][1]+1,nchar(br)))
      dbh1<-sum(one,two)/2
      DBH<-paste(DBH,dbh1,sep=",")
    }
    rm(m)
    if(is.na(DBH)) result[j,5]<-"not collected" else result[j,5]<-DBH
    
    dbh<-as.character(dataCD[j,6])
    ind<-gregexpr("/",dbh)
    #{if(length(ind)==0) next(); 
    if(ind[[1]][1]==-1) br<-dbh else br<-substr(dbh, 1, ind[[1]][1]-1)
    #DBH<-c()
    ind2<-gregexpr(",",br)
    one<-as.numeric(substr(br, 1, ind2[[1]][1]-1))
    two<-as.numeric(substr(br, ind2[[1]][1]+1,nchar(br)))
    DBH<-sum(one,two)/2
    rm(one,two)
    for(m in 1:length(ind[[1]][1])){
      {if(ind[[1]][1]==-1) next();}
      br<-substr(dbh, ind[[1]][m]+1, ind[[1]][m+1]-1)
      if(m==length(ind[[1]])) br<-substr(dbh, ind[[1]][m]+1,nchar(dbh))
      #test[m]<-br
      ind2<-gregexpr(",",br)
      one<-as.numeric(substr(br, 1, ind2[[1]][1]-1))
      two<-as.numeric(substr(br, ind2[[1]][1]+1,nchar(br)))
      dbh1<-sum(one,two)/2
      DBH<-paste(DBH,dbh1,sep=",")
    }
    rm(ind)
    if(is.na(DBH)) result[j,6]<-"not collected" else result[j,6]<-DBH
    result[j,7:8] <- data.frame(lapply(dataCD[j,7:8], as.character), stringsAsFactors=FALSE)
    
    #count number of shoots from 1.3 m
    result[j,9] <- length(gregexpr(",",result[j,5])[[1]])
  }
  result.1[[i]]<-result
}

result1<-do.call(rbind.data.frame,result.1)
write.csv(result1,paste0(getwd(),"/Yield/Coffee.density_2015_cleaned.csv"))

