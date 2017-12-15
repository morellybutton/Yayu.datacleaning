#code for collecting/cleaning yield data and calculating per ha values
library(gdata)
library(stringr)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/")

site="Yayu"
#dates<-"nov14"
year<-"2016"

#average berry weights by color (g)
red<-1.211
green<-0.987
other<-0.374

plts<-read.csv(paste0(getwd(),"/",site,"/plotnums.csv"))
#remove Met station and Forest plots
plts<-plts[grep("Met_",plts$name,invert=T),]
plts<-plts[grep("FC",plts$name,invert=T),]

p<-as.character(plts$name)

result.1<-list()
result.2<-list()
result.3<-list()
final.1<-list()
for(i in 1:length(p)){
  if(p[i]=="H7") next
  if(p[i]=="H11") next
    
  #open disease branch data
  dataBr <- data.frame(read.xls(paste0(getwd(),"/",site,"/Disease/",year,"/Disease Survey_Combined.xlsx"), sheet=paste0(p[i]," (branches)")),stringsAsFactors = F)
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
  
  #open flower data
  if(year!=2014) dataFs <- data.frame(read.xls(paste0(getwd(),"/",site,"/Yield/",year,"/FruitSet_Combined.xlsx"), sheet=p[i]),stringsAsFactors = F) else dataFs<-NA
  #add column names
  if(!is.na(dataFs)) {
    colnames(dataFs)<-c("Plot","Coordinate","Shrub.id","Branch.cm","No.of.buds","No.of.flower","No.of.eberry","No.of.leaves","NoCLR","iCLR","NoChl","NoHerb","Notes") 
  #find entry days
  d1 <- data.frame(grep("DATE",dataFs[,4]))
  d1[(nrow(d1)+1),1]<-nrow(dataFs)
  d1$date<-as.Date(dataFs[d1[,1],5])
  for(x in 1:(nrow(d1)-1)){
    if(x < (nrow(d1)-1)) dataFs[(d1[x,1]+2):(d1[(x+1),1]-1),"Date"]<-d1[x,2] else dataFs[(d1[x,1]+2):(d1[(x+1),1]),"Date"]<-d1[x,2]
  }
  dataFs$Date<-as.Date(dataFs$Date,origin = "1970-01-01")
  dataFs <- dataFs[!is.na(dataFs$Date),]}
  
  #open yield files
  dataY<-data.frame(read.xls(paste0(getwd(),"/",site,"/Yield/",year,"/Yield_Combined.xlsx"), sheet=p[i]),stringsAsFactors = F)
  dataY<-dataY[,!is.na(dataY[1,])]
  if(year==2014) colnames(dataY)<-c("Shrub.id","Branch","Redwt.g","Greenwt.g","Otherwt.g") else colnames(dataY)<-c("Shrub.id","Branch","Redwt.g","Greenwt.g","Otherwt.g","NoCBB")
  dataY$Shrub.id<-gsub("\\*","",dataY$Shrub.id)
  #find start of data
  s<-grep("Shrub id",dataY[,1])
  #pull out actual data rows
  y<-dataY[(s+1):length(dataY[,1]),]

  #create empty dataframe for results
  if(year==2014) yield<-data.frame(Plot=character(),ShrubId=character(),Branch.cm=character(),Redwt.g=numeric(),Greenwt.g=numeric(),Otherwt.g=numeric(),stringsAsFactors=FALSE) else yield<-data.frame(Plot=character(),ShrubId=character(),Branch.cm=character(),Redwt.g=numeric(),Greenwt.g=numeric(),Otherwt.g=numeric(),NoCBB=numeric(),stringsAsFactors=FALSE)
  if(year==2014) shr<-as.character(unique(y$Shrub.id)) else shr<-as.character(unique(dataFs$Shrub.id))
  if(length(shr)>7) break
  d.f.s<-list()
  shr<-gsub("\\*","",shr)
  for(j in 1:length(shr)){
    
    fs.disease<-data.frame(Plot=character(),ShrubId=character(),Branch.cm=character(),No.of.buds=numeric(),No.of.flower=numeric(),No.of.eberry=numeric(),No.of.fruits=numeric(),NoCBB=numeric(),NoCBD=numeric(),No.of.leaves=numeric(),NoLM=numeric(),NoCLR=numeric(),iCLR=numeric(),NoChl=numeric(),NoWilt=numeric(),NoHerb=numeric(),Notes=character(),Date=as.Date(character()),stringsAsFactors=FALSE)
    #compare with disease measure
    x1<-dataBr[dataBr$Shrub.id==shr[j]&dataBr$Date>"2014-07-01",]
    if(year!=2014) y1<-dataFs[dataFs$Shrub.id==shr[j],] else y1<-dataBr[dataBr$Shrub.id==shr[j]&dataBr$Date<"2014-07-01",]
    #pull out branch measures for same shrub
    b<-y[y[,1]==as.character(shr[j]),]
    #if(nrow(b)==0 && p[i]=="WE2") next
    if(nrow(b)==0) next
    for(m in 1:nrow(b)){
      if(m<(nrow(b))) y1.1<-data.frame(lapply(y1[y1$Branch.cm==as.character(b[(m),"Branch"]),], as.character),stringsAsFactors=F) else y1.1<-data.frame(c(NA,NA))
      if(m<(nrow(b))) x1.1<-data.frame(lapply(x1[x1$Branch.cm==as.character(b[(m),"Branch"]),], as.character),stringsAsFactors=F) 
      
      yield[4*(j-1)+m,1]<-p[i]
      yield[4*(j-1)+m,2]<-shr[j]
      yield[4*(j-1)+m,3]<-as.character(b[m,2])
      if(length(grep("\\(",as.character(b[m,3])))>0) yield[4*(j-1)+m,4]<-as.numeric(gsub("\\(","",as.character(b[m,3])))*red else yield[4*(j-1)+m,4]<-as.numeric(as.character(b[m,3]))
      if(length(grep("\\(",as.character(b[m,4])))>0) yield[4*(j-1)+m,5]<-as.numeric(gsub("\\(","",as.character(b[m,4])))*green else yield[4*(j-1)+m,5]<-as.numeric(as.character(b[m,4]))
      if(length(grep("\\(",as.character(b[m,5])))>0) yield[4*(j-1)+m,6]<-as.numeric(gsub("\\(","",as.character(b[m,5])))*other else yield[4*(j-1)+m,6]<-as.numeric(as.character(b[m,5]))
      if(year!=2014) yield[4*(j-1)+m,7]<-as.numeric(as.character(b[m,6]))
      
      if(ncol(y1.1)==1) next
      
      if(nrow(y1.1)==0) fs.disease[nrow(fs.disease)+1,3]<-as.character(b[m,2])
      if(nrow(y1.1)==0) fs.disease[nrow(fs.disease),4:17]<-NA else {
      if(year!=2014) fs.disease[nrow(fs.disease)+(1:nrow(y1.1)),3:6]<-y1.1[1:nrow(y1.1),4:7] else fs.disease[nrow(fs.disease)+(1:nrow(y1.1)),3]<-y1.1[1:nrow(y1.1),4]
      if(year==2014) fs.disease[(nrow(fs.disease)-(nrow(y1.1))+1):nrow(fs.disease),7:9]<-y1.1[1:nrow(y1.1),5:7]
      fs.disease[(nrow(fs.disease)-(nrow(y1.1))+1):nrow(fs.disease),10]<-y1.1[1:nrow(y1.1),8] 
      if(year==2014) fs.disease[(nrow(fs.disease)-(nrow(y1.1))+1):nrow(fs.disease),11:16]<-y1.1[1:nrow(y1.1),9:14] else break #need to fix for other years
      #fs.disease[(nrow(fs.disease)-(nrow(y1.1))+1):nrow(fs.disease),16:17]<-y1.1[1:nrow(y1.1),12:13]
      }
      if(nrow(y1.1)==0) fs.disease[nrow(fs.disease),"Date"]<-NA else fs.disease[(nrow(fs.disease)-(nrow(y1.1))+1):nrow(fs.disease),"Date"]<-y1.1[1:nrow(y1.1),"Date"]
      fs.disease[nrow(fs.disease)+(1:nrow(x1.1)),3]<-x1.1[1:nrow(x1.1),4]
      fs.disease[(nrow(fs.disease)-(nrow(x1.1))+1):nrow(fs.disease),7:17]<-x1.1[1:nrow(x1.1),5:15]
      fs.disease[(nrow(fs.disease)-(nrow(x1.1))+1):nrow(fs.disease),"Date"]<-x1.1[1:nrow(x1.1),"Date"]
    }
    fs.disease[,1]<-p[i]
    fs.disease[,2]<-shr[j]
    fs.disease$Same<-1
    fs.disease[grep("changed",fs.disease$Notes),"Same"]<-0
    fs.disease[grep("replaced",fs.disease$Notes),"Same"]<-0
    d.f.s[[j]]<-fs.disease
  }
  result.3[[i]]<-do.call(rbind.data.frame,d.f.s)
  
  #open coffee density measures
  dataCD <- data.frame(read.xls(paste0(getwd(),"/",site,"/Yield/2015/Coffee density data final.xlsx"), sheet=p[i]),stringsAsFactors = F)
  dataCD<-dataCD[,!is.na(dataCD[1,])]
  #find start of data
  s1 <- grep("Plot",dataCD[,1])+1
  colnames(dataCD)<-c("Plot","Subplot","Shrub.id","Height","DBH.1.3","DBH40","CDiameter.x","CDiameter.y","Notes")
  dataCD <- dataCD[s1:nrow(dataCD),]
  
  #open disease shrub data
  dataSh <- data.frame(read.xls(paste0(getwd(),"/",site,"/Disease/",year,"/Disease Survey_Combined.xlsx"), sheet=paste0(p[i]," (shrub)")),stringsAsFactors = F)
  #find start of data
  s1 <- grep("Plot",dataSh[,1])+1
  dataSh <- dataSh[s1:nrow(dataSh),]
  if(year!=2014) colnames(dataSh)<-c("Plot","Coordinate","Shrub.id","Wilt","Height","DBH.1.3","DBH.1.3x","DBH40","DBH40x","CDiameter.x","CDiameter.y","Soil.moisture","Totshoots","Deadshoots","Missingshoots","Notes") else colnames(dataSh)<-c("Plot","Coordinate","Shrub.id","Wilt","Height","DBH.1.3","DBH40","CDiameter.x","CDiameter.y","Soil.moisture","Totshoots","Deadshoots","Missingshoots")
  
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
  
  result2<-data.frame(Plot=character(),ShrubNo=character(),ShrubId=character(),Height=numeric(),DBH1.3=character(),DBH1.3x=character(),DBH40=character(),DBH40x=character(),Xcrown=numeric(),Ycrown=numeric(),Soil.moisture=numeric(),TotShoots=numeric(),DeadShoots=numeric(),MissingShoots=numeric(),stringsAsFactors=FALSE) 
  for(j in 1:length(shr)){
    shrub<-dataSh[dataSh$Shrub.id==shr[j],]
    if(nrow(shrub)!=0) result2[j,1:3] <- data.frame(lapply(shrub[,1:3], as.character), stringsAsFactors=FALSE) else result2[j,1:3] <- data.frame(cbind(p[i],NA,shr[j]), stringsAsFactors=FALSE)
    if(nrow(shrub)!=0) result2[j,4] <- as.numeric(as.character(shrub[,5])) else result2[j,4]<-mean(as.numeric(as.character(dataCD$Height)),na.rm=T)
    
    #find number of shoots per shrub (at 1.3 m)
    dbh<-data.frame(as.character(shrub$DBH.1.3),stringsAsFactors = F)
    if(nrow(dbh)==0) DBH<-paste(as.character(rep(signif(mean(unlist(lapply(str_split(result$DBH1.3,","),as.numeric)),na.rm=T),3),round(mean(result$TotShoots)))),collapse=",") else {
      ind<-gregexpr("/",dbh)
      if(ind[[1]][1]==-1) br=dbh else br<-substr(dbh, 1, ind[[1]][1]-1)
      ind2<-gregexpr(",",br)
      if(ind2[[1]][1]==-1) one<-as.numeric(br) else one<-as.numeric(substr(br, 1, ind2[[1]][1]-1))
      if(ind2[[1]][1]==-1) two<-as.numeric(br) else two<-as.numeric(substr(br, ind2[[1]][1]+1,nchar(br)))
      DBH<-sum(one,two)/2
      for(m in 1:length(ind[[1]])){
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
      rm(m)
    }
       if(is.na(DBH)) result2[j,5]<-"not collected" else result2[j,5]<-DBH
    
    dbh<-data.frame(as.character(shrub$DBH.1.3x))
    if(nrow(shrub)==0) DBH<-signif(mean(unlist(lapply(str_split(result$DBH1.3,","),as.numeric)),na.rm=T),3) else {
      if(year!=2014){
        ind2<-gregexpr(",",dbh)
        one<-as.numeric(substr(dbh, 1, ind2[[1]][1]-1))
        two<-as.numeric(substr(dbh, ind2[[1]][1]+1,nchar(dbh)))
        DBH<-sum(one,two)/2
        } else
          DBH<-mean(as.numeric(str_split(result2[j,5],",")[[1]]),na.rm=T)
    }
    result2[j,6]<-DBH
    
    dbh<-data.frame(as.character(shrub$DBH40))
    if(nrow(dbh)==0) DBH<-paste(rep(signif(mean(unlist(lapply(str_split(result$DBH40,","),as.numeric)),na.rm=T),3),round(mean(unlist(lapply(lapply(str_split(result$DBH40,","),as.numeric),length)),na.rm=T))),collapse=",") else {
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
      rm(ind)
    }
    if(is.na(DBH)) result2[j,7]<-"not collected" else result2[j,7]<-DBH
    
    dbh<-data.frame(as.character(shrub$DBH40x))
    if(nrow(shrub)==0) DBH<-signif(mean(unlist(lapply(str_split(result$DBH40,","),as.numeric)),na.rm=T),3) else {
      if(year!=2014){
      
      ind2<-gregexpr(",",br)
      one<-as.numeric(substr(br, 1, ind2[[1]][1]-1))
      two<-as.numeric(substr(br, ind2[[1]][1]+1,nchar(br)))
      DBH<-sum(one,two)/2
      } else 
        DBH<-mean(as.numeric(str_split(result2[j,7],",")[[1]]),na.rm=T)
    }
    result2[j,8]<-DBH
    
    if(year!=2014&nrow(shrub)>0) result2[j,9:14]<-data.frame(lapply(shrub[,10:15], as.character), stringsAsFactors=FALSE) 
    if(year==2014&nrow(shrub)>0) result2[j,9:14]<-data.frame(lapply(shrub[,8:13], as.character), stringsAsFactors=FALSE) else result2[j,9:14]<-c(mean(as.numeric(result$Xcrown)),mean(as.numeric(result$Ycrown)),NA,NA,NA,NA)
  }
  result.2[[i]]<-result2
  #rm(result,result2,dataCD,dataSh)
  
  final.1[[i]]<- yield
  rm(dataY,y,yield)
}

final<-do.call(rbind.data.frame,final.1)
write.csv(final,paste0(getwd(),"/",site,"/Yield/Yield_",year,"_cleaned.csv"))

result1<-do.call(rbind.data.frame,result.1)
write.csv(result1,paste0(getwd(),"/",site,"/Yield/Coffee.density_",year,"_cleaned.csv"))

result2<-do.call(rbind.data.frame,result.2)
write.csv(result2,paste0(getwd(),"/",site,"/Yield/Shrub.data_",year,"_cleaned.csv"))

result3<-do.call(rbind.data.frame,result.3)
write.csv(result3,paste0(getwd(),"/",site,"/Yield/Disease.Fruitset_",year,"_cleaned.csv"))


