#code to calculate basal area, above ground biomass and cubic meters for each species by plot

setwd("/users/alex/Documents/Research/Africa/ECOLIMITS/Data/Yayu/CarbonStock/")

#library(xlsx)
#library(plyr)
#library(stringr)
#library(ggplot2)
library(tidyverse)

#load plotdata
plts<-read.csv("/Users/alex/Documents/Research/Africa/ECOLIMITS/Data/Yayu/plotnums.csv")
plts<-plts[grep("Met_",plts$name, invert=T),]
f.plts<-plts$name[grep("FC",plts$name)]
c.plts<-plts$name[grep("FC",plts$name,invert=T)]
#load wood density lookup
w.d<-read.csv(paste0(getwd(),"/Species_lookup.csv"))

#create dataframe to save all calculations too
biomass<-list()

h.est=function(dbh, h){
  l      =lm(h~dbh)
  coeffs = coefficients(l)
  pred.h = coeffs[1] + coeffs[2]*dbh
}

Chave2014 <- function(diax, density, height) {
  AGB_est <- 0.0673*(density*((diax)^2)*height)^0.976 
  return(AGB_est)
}


t.hghts<-list()
SPP<-list()
#go through forest plots first
for(i in 1:length(f.plts)){
  df.ls<-data.frame(read.csv(paste0(getwd(),"/Cleaned/",f.plts[i],"_Treecensus.1.1.csv")),stringsAsFactors = F)
  df.ls.2<-data.frame(read.csv(paste0(getwd(),"/Cleaned/",f.plts[i],"_Treecensus.1.2.csv")),stringsAsFactors = F)
  df.ss<-data.frame(read.csv(paste0(getwd(),"/Cleaned/",f.plts[i],"_TreecensusSS.1.1.csv")),stringsAsFactors = F)
  df.ss.2<-data.frame(read.csv(paste0(getwd(),"/Cleaned/",f.plts[i],"_TreecensusSS.1.2.csv")),stringsAsFactors = F)
  
  df.ls$NSpecies<-as.character(df.ls$NSpecies)
  df.ss$NSpecies<-as.character(df.ss$NSpecies)
  df.ls.2$NSpecies<-as.character(df.ls.2$NSpecies)
  df.ss.2$NSpecies<-as.character(df.ss.2$NSpecies)
  #remove dead trees
  df.ls<-df.ls[grep("0",df.ls$Remarks,invert=T),]
  df.ss<-df.ss[grep("0",df.ss$Remarks,invert=T),]
  df.ls.2<-df.ls.2[grep("0",df.ls$Remarks,invert=T),]
  df.ss.2<-df.ss.2[grep("0",df.ss$Remarks,invert=T),]
  
  #remove non-numeric DBH and height
  df.ls$DBH1<-as.numeric(as.character(df.ls$Diameter))
  df.ls$DBH2<-as.numeric(as.character(df.ls.2[match(df.ls$Tag,df.ls.2$Tag),"Diameter"]))
  df.ss$DBH1<-rowMeans(cbind(df.ss$Diameter.1,df.ss$Diam.2,df.ss$Diam.3))
  df.ss.2$DBH1<-rowMeans(cbind(df.ss.2$Diameter.1,df.ss.2$Diam.2,df.ss.2$Diam.3))
  df.ss$DBH2<-df.ss.2[match(df.ss$Tag,df.ss.2$Tag),"DBH1"]
  
  df.ls$Height<-as.numeric(as.character(df.ls$THeight))
  #df.ss$Height<-as.numeric(as.character(df.ss$THeight))
  
  #calculate height measures
  hts<-data.frame(cbind(df.ls$DBH1,df.ls$Height))
  colnames(hts)<-c("dbh","h")
  if(nrow(df.ls[!is.na(df.ls$Height),])==0) hghts<-data.frame(cbind(0,0)) else hghts<-data.frame(cbind(as.numeric(coefficients(lm(hts$dbh~hts$h))[1]),as.numeric(coefficients(lm(hts$dbh~hts$h))[2])))
  colnames(hghts)<-c("intercept","slope")
  hghts$plot<-f.plts[i]
  t.hghts[[i]]<-hghts
  
  #replace any missing tree heights
  if(length(df.ls[is.na(df.ls$Height),"Height"])>0) df.ls[is.na(df.ls$Height),"Height"]<-df.ls[is.na(df.ls$Height),"DBH1"]*hghts[,2]+hghts[,1]
   
  #pull out species for each plot
  spp<-data.frame(unique(c(as.character(df.ls$NSpecies),as.character(df.ss$NSpecies))),stringsAsFactors = F)
  spp<-data.frame(spp[order(spp),],stringsAsFactors = F)
  spp<-data.frame(spp[!is.na(spp[,1]),],stringsAsFactors = F)
  #add wood density
  spp$wd<-as.numeric(w.d[match(spp$spp..is.na.spp...1.....,w.d$species),"WD"])
  #remove missing wood density values
  spp<-spp[!is.na(spp$wd),]
  #calculate basal area and biomass for each species
  for(j in 1:nrow(spp)){
    #skip if species is a climber
    if(w.d[match(spp[j,1],w.d$species),"Climber"]==1) next
    #if(is.na(df.ls[df.ls$NSpecies==spp[j,1],"DBH1"])&&is.na(df.ls[df.ls$NSpecies==spp[j,1],"DBH2"])&&is.na(df.ss[df.ss$NSpecies==spp[j,1],"DBH1"])&&is.na(df.ss[df.ss$NSpecies==spp[j,1],"DBH2"])) next
    if(nrow(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),])>0|nrow(df.ss[df.ss$NSpecies==spp[j,1]&!is.na(df.ss$NSpecies),])>0) spp[j,"BA.1"]<-sum(pi*(df.ls[df.ls$NSpecies==spp[j,1]&df.ls$DBH1>=10,"DBH1"]/200)^2,na.rm=T)/0.36+sum(pi*(df.ss[df.ss$NSpecies==spp[j,1],"DBH1"]/200)^2,na.rm=T)/(5*.1*.1) else spp[j,"BA.1"]<-0
    if(nrow(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),])>0|nrow(df.ss[df.ss$NSpecies==spp[j,1]&!is.na(df.ss$NSpecies),])>0) spp[j,"BA.2"]<-sum(pi*(df.ls[df.ls$NSpecies==spp[j,1]&df.ls$DBH2>=10,"DBH2"]/200)^2,na.rm=T)/0.36+sum(pi*(df.ss[df.ss$NSpecies==spp[j,1],"DBH2"]/200)^2,na.rm=T)/(5*.1*.1) else spp[j,"BA.2"]<-0
    
    #calculate biomass
    if(nrow(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),])>0|nrow(df.ss[df.ss$NSpecies==spp[j,1]&!is.na(df.ss$NSpecies),])>0) spp[j,"BM.1"]<-sum(Chave2014(df.ls[df.ls$NSpecies==spp[j,1]&df.ls$DBH1>=10,"DBH1"],df.ls[df.ls$NSpecies==spp[j,1]&df.ls$DBH1>=10,"Height"],spp[j,"wd"])/1000,na.rm=T)+sum(Chave2014(df.ss[df.ss$NSpecies==spp[j,1],"DBH1"],df.ss[df.ss$NSpecies==spp[j,1],"Height"],spp[j,"wd"])/1000,na.rm=T)/(5*.1*.1) else spp[j,"BM.1"]<-0
    if(nrow(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),])>0|nrow(df.ss[df.ss$NSpecies==spp[j,1]&!is.na(df.ss$NSpecies),])>0) spp[j,"BM.2"]<-sum(Chave2014(df.ls[df.ls$NSpecies==spp[j,1]&df.ls$DBH2>=10,"DBH2"],df.ls[df.ls$NSpecies==spp[j,1]&df.ls$DBH2>=10,"Height"],spp[j,"wd"])/1000,na.rm=T)+sum(Chave2014(df.ss[df.ss$NSpecies==spp[j,1],"DBH2"],df.ss[df.ss$NSpecies==spp[j,1],"Height"],spp[j,"wd"])/1000,na.rm=T)/(5*.1*.1) else spp[j,"BM.2"]<-0
    
    #include number of individuals per ha (for large and small trees)
    if(nrow(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),])>0) spp[j,"N.1"]<-nrow(df.ls[df.ls$NSpecies==spp[j,1]&df.ls$DBH1>=10&!is.na(df.ls$DBH1),]) else spp[j,"N.1"]<-0
    if(nrow(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),])>0) spp[j,"N.2"]<-nrow(df.ls[df.ls$NSpecies==spp[j,1]&df.ls$DBH2>=10&!is.na(df.ls$DBH2),]) else spp[j,"N.2"]<-0
    if(nrow(df.ss[df.ss$NSpecies==spp[j,1]&!is.na(df.ss$NSpecies),])>0) spp[j,"n.1"]<-nrow(df.ss[df.ss$NSpecies==spp[j,1]&df.ss$DBH1<10&!is.na(df.ss$DBH1),])/(5*.1*.1) else spp[j,"n.1"]<-0
    if(nrow(df.ss[df.ss$NSpecies==spp[j,1]&!is.na(df.ss$NSpecies),])>0) spp[j,"n.2"]<-nrow(df.ss[df.ss$NSpecies==spp[j,1]&df.ss$DBH2<10&!is.na(df.ss$DBH2),])/(5*.1*.1) else spp[j,"n.2"]<-0
    
    #include DBH values of individuals >30 cm
    if(nrow(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),])>0&&max(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),"DBH1"],na.rm=T)>30) spp[j,"T.1"]<-paste(as.character(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies)&df.ls$DBH1>=30,"DBH1"]),collapse=",") else spp[j,"T.1"]<-0
    if(nrow(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),])>0&&max(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),"DBH2"],na.rm=T)>30) spp[j,"T.2"]<-paste(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies)&df.ls$DBH2>=30,"DBH2"],collapse=",") else spp[j,"T.2"]<-0
    
    #add timber code
    spp[j,"Timber"]<-w.d[match(spp[j,1],w.d$species),"Timber_code"]
    #add deciduous
    spp[j,"Deciduous"]<-w.d[match(spp[j,1],w.d$species),"Deciduous"]
    #add legume
    spp[j,"Legume"]<-w.d[match(spp[j,1],w.d$species),"Legume_code"]
    #add successional
    spp[j,"Success"]<-as.character(w.d[match(spp[j,1],w.d$species),"Succession"])
  }
  #add Plot
  spp$Plot<-f.plts[i]
  SPP[[i]]<-spp
}
heights<-do.call(rbind.data.frame,t.hghts)
#save height relationships
write.csv(heights,paste0(getwd(),"/forest_heights.csv"))
heights<-read.csv(paste0(getwd(),"/forest_heights.csv"))
heights<-heights[heights$plot=="FC2",]

#each plot and load large and small tree measures
for(i in 1:length(c.plts)){
  if(c.plts[i]=="H5") next
  p.size<-plts[plts$name==c.plts[i],"plotsize"]
  if(p.size>=0.36) sm.p.size<-3*.05*.05 else sm.p.size<-.05*.05
  if(c.plts[i]=="H1") df.ls<-data.frame(Tag=character(),Diameter=numeric(),Nspecies=character()) else df.ls<-data.frame(read.csv(paste0(getwd(),"/Cleaned/",c.plts[i],"_Treecensus.1.1.csv")),stringsAsFactors = F)
  if(c.plts[i]=="H1") df.ls.2<-data.frame(Tag=character(),Diameter=numeric(),Nspecies=character()) else df.ls.2<-data.frame(read.csv(paste0(getwd(),"/Cleaned/",c.plts[i],"_Treecensus.1.2.csv")),stringsAsFactors = F)
  if(c.plts[i]=="B10") df.ss<-data.frame(Tag=character(),Diameter.1=numeric(),Diam.2=numeric(),Diam.3=numeric(),Nspecies=character()) else df.ss<-data.frame(read.csv(paste0(getwd(),"/Cleaned/",c.plts[i],"_TreecensusSS.1.1.csv")),stringsAsFactors = F)
  if(c.plts[i]=="B10") df.ss.2<-data.frame(Tag=character(),Diameter.1=numeric(),Diam.2=numeric(),Diam.3=numeric(),Nspecies=character()) else df.ss.2<-data.frame(read.csv(paste0(getwd(),"/Cleaned/",c.plts[i],"_TreecensusSS.1.2.csv")),stringsAsFactors = F)
  
  df.ls$NSpecies<-as.character(df.ls$NSpecies)
  df.ss$NSpecies<-as.character(df.ss$NSpecies)
  df.ls.2$NSpecies<-as.character(df.ls.2$NSpecies)
  df.ss.2$NSpecies<-as.character(df.ss.2$NSpecies)
  #remove dead trees
  df.ls<-df.ls[grep("0",df.ls$Remarks,invert=T),]
  df.ss<-df.ss[grep("0",df.ss$Remarks,invert=T),]
  df.ls.2<-df.ls.2[grep("0",df.ls$Remarks,invert=T),]
  df.ss.2<-df.ss.2[grep("0",df.ss$Remarks,invert=T),]
  
  #remove non-numeric DBH and height, replace DBH with 0
  df.ls$DBH1<-as.numeric(as.character(df.ls$Diameter))
  df.ls$DBH2<-df.ls.2[match(df.ls$Tag,df.ls.2$Tag),"Diameter"]
  df.ss$DBH1<-rowMeans(cbind(df.ss$Diameter.1,df.ss$Diam.2,df.ss$Diam.3))
  df.ss.2$DBH1<-rowMeans(cbind(df.ss.2$Diameter.1,df.ss.2$Diam.2,df.ss.2$Diam.3))
  df.ss$DBH2<-df.ss.2[match(df.ss$Tag,df.ss.2$Tag),"DBH1"]
  
  #df.ls[is.na(df.ls$DBH1),"DBH1"]<-0
  #df.ls[is.na(df.ls$DBH2),"DBH2"]<-0
  #df.ss[is.na(df.ss$DBH1),"DBH1"]<-0
  #df.ss[is.na(df.ss$DBH2),"DBH2"]<-0
  
  df.ls$Height<-as.numeric(as.character(df.ls$THeight))
  #df.ss$Height<-as.numeric(as.character(df.ss$THeight))
  #heights[heights$transect==strsplit(as.character(c.plts[i])," ")[[1]][1],"slope"]
  
  #pull out species for each plot
  spp<-data.frame(unique(c(as.character(df.ls$NSpecies),as.character(df.ss$NSpecies))),stringsAsFactors = F)
  spp<-data.frame(spp[order(spp),],stringsAsFactors = F)
  spp<-data.frame(spp[!is.na(spp[,1]),],stringsAsFactors = F)
  #add wood density
  spp$wd<-as.numeric(w.d[match(spp$spp..is.na.spp...1.....,w.d$species),"WD"])
  #remove missing wood density values
  spp<-spp[!is.na(spp$wd),]
  #calculate basal area and biomass for each species
  for(j in 1:nrow(spp)){
    #skip if species is a climber
    if(w.d[match(spp[j,1],w.d$species),"Climber"]==1) next
    
    #if(is.na(df.ls[df.ls$NSpecies==spp[j,1],"DBH1"])&&is.na(df.ls[df.ls$NSpecies==spp[j,1],"DBH2"])&&is.na(df.ss[df.ss$NSpecies==spp[j,1],"DBH1"])&&is.na(df.ss[df.ss$NSpecies==spp[j,1],"DBH2"])) next
    if(nrow(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),])>0|nrow(df.ss[df.ss$NSpecies==spp[j,1]&!is.na(df.ss$NSpecies),])>0) spp[j,"BA.1"]<-sum(pi*(df.ls[df.ls$NSpecies==spp[j,1]&df.ls$DBH1>=10,"DBH1"]/200)^2,na.rm=T)/p.size+sum(pi*(df.ss[df.ss$NSpecies==spp[j,1],"DBH1"]/200)^2,na.rm=T)/sm.p.size else spp[j,"BA.1"]<-0
    if(nrow(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),])>0|nrow(df.ss[df.ss$NSpecies==spp[j,1]&!is.na(df.ss$NSpecies),])>0) spp[j,"BA.2"]<-sum(pi*(df.ls[df.ls$NSpecies==spp[j,1]&df.ls$DBH2>=10,"DBH2"]/200)^2,na.rm=T)/p.size+sum(pi*(df.ss[df.ss$NSpecies==spp[j,1],"DBH2"]/200)^2,na.rm=T)/sm.p.size else spp[j,"BA.2"]<-0
    
    #calculate biomass
    if(nrow(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),])>0|nrow(df.ss[df.ss$NSpecies==spp[j,1]&!is.na(df.ss$NSpecies),])>0&spp[j,1]!="Coffea arabica") spp[j,"BM.1"]<-sum(Chave2014(df.ls[df.ls$NSpecies==spp[j,1]&df.ls$DBH1>=10,"DBH1"],df.ls[df.ls$NSpecies==spp[j,1]&df.ls$DBH1>=10,"Height"],spp[j,"wd"])/1000,na.rm=T)/p.size+sum(Chave2014(df.ss[df.ss$NSpecies==spp[j,1],"DBH1"],df.ss[df.ss$NSpecies==spp[j,1],"Height"],spp[j,"wd"])/1000,na.rm=T)/sm.p.size else spp[j,"BM.1"]<-0
    if(nrow(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),])>0|nrow(df.ss[df.ss$NSpecies==spp[j,1]&!is.na(df.ss$NSpecies),])>0&spp[j,1]!="Coffea arabica") spp[j,"BM.2"]<-sum(Chave2014(df.ls[df.ls$NSpecies==spp[j,1]&df.ls$DBH2>=10,"DBH2"],df.ls[df.ls$NSpecies==spp[j,1]&df.ls$DBH2>=10,"Height"],spp[j,"wd"])/1000,na.rm=T)/p.size+sum(Chave2014(df.ss[df.ss$NSpecies==spp[j,1],"DBH2"],df.ss[df.ss$NSpecies==spp[j,1],"Height"],spp[j,"wd"])/1000,na.rm=T)/sm.p.size else spp[j,"BM.2"]<-0
    #coffee allometric equation, y=0.281*dbh^2.06
    if(spp[j,1]=="Coffea arabica") spp[j,"BM.1"]<-sum((0.281*df.ss[df.ss$NSpecies==spp[j,1],"DBH1"]^2.06)/1000,na.rm=T)/sm.p.size
    if(spp[j,1]=="Coffea arabica") spp[j,"BM.2"]<-sum((0.281*df.ss[df.ss$NSpecies==spp[j,1],"DBH2"]^2.06)/1000,na.rm=T)/sm.p.size
    
    #include number of individuals per large plot (for large and small trees)
    if(nrow(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),])>0) spp[j,"N.1"]<-nrow(df.ls[df.ls$NSpecies==spp[j,1]&df.ls$DBH1>=10&!is.na(df.ls$DBH1),]) else spp[j,"N.1"]<-0
    if(nrow(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),])>0) spp[j,"N.2"]<-nrow(df.ls[df.ls$NSpecies==spp[j,1]&df.ls$DBH2>=10&!is.na(df.ls$DBH2),]) else spp[j,"N.2"]<-0
    if(nrow(df.ss[df.ss$NSpecies==spp[j,1]&!is.na(df.ss$NSpecies),])>0) spp[j,"n.1"]<-nrow(df.ss[df.ss$NSpecies==spp[j,1]&df.ss$DBH1<10&!is.na(df.ss$DBH1),])*p.size/sm.p.size else spp[j,"n.1"]<-0
    if(nrow(df.ss[df.ss$NSpecies==spp[j,1]&!is.na(df.ss$NSpecies),])>0) spp[j,"n.2"]<-nrow(df.ss[df.ss$NSpecies==spp[j,1]&df.ss$DBH2<10&!is.na(df.ss$DBH2),])*p.size/sm.p.size else spp[j,"n.2"]<-0
    
    #include DBH values of individuals >30 cm
    if(nrow(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),])>0&&max(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),"DBH1"],na.rm=T)>30) spp[j,"T.1"]<-paste(as.character(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies)&df.ls$DBH1>=30,"DBH1"]),collapse=",") else spp[j,"T.1"]<-0
    if(nrow(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),])>0&&max(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies),"DBH2"],na.rm=T)>30) spp[j,"T.2"]<-paste(df.ls[df.ls$NSpecies==spp[j,1]&!is.na(df.ls$NSpecies)&df.ls$DBH2>=30,"DBH2"],collapse=",") else spp[j,"T.2"]<-0
    
    #add timber code
    spp[j,"Timber"]<-w.d[match(spp[j,1],w.d$species),"Timber_code"]
    #add deciduous
    spp[j,"Deciduous"]<-w.d[match(spp[j,1],w.d$species),"Deciduous"]
    #add legume
    spp[j,"Legume"]<-w.d[match(spp[j,1],w.d$species),"Legume_code"]
    #add successional
    spp[j,"Success"]<-as.character(w.d[match(spp[j,1],w.d$species),"Succession"])
  }
  #add Plot
  spp$Plot<-as.character(c.plts[i])
  SPP[[i+3]]<-spp
  rm(spp,df.ls,df.ls.2,df.ss,df.ss.2,p.size,sm.p.size)
}

final<-do.call(rbind.data.frame,SPP)
#remove NA
final<-final[!is.na(final$BA.1),]
write.csv(final,paste0(getwd(),"/AllPlots_BiomassBasalArea.csv"))

#open calculations
d.f<-data.frame(read.csv(paste0(getwd(),"/AllPlots_BiomassBasalArea.csv")))
colnames(d.f)<-c("x","species",colnames(d.f[,3:18]))

#find most common shade species by basal area in coffee farms
d.f$kebele<-as.character(plts[match(d.f$Plot,plts$name),"Kebele"])
d.c<-d.f[grep("FC",d.f$Plot,invert=T),]
b.a <- d.c %>% group_by(kebele,species) %>% summarise(b.area1=sum(BA.1,na.rm=T)/length(Plot),b.area2=sum(BA.2,na.rm=T)/length(Plot))
#order by basal area
b.a<-b.a[with(b.a,order(kebele,-b.area1)),]
write.csv(b.a,paste0(getwd(),"/Basalarea_spp.csv"))

#plot barplot by kebele
ggplot(b.a,aes(species,b.area1)) + geom_bar(stat="identity",aes(fill=species)) + facet_wrap(~kebele,ncol=3)+
  xlab("Species") + ylab("Basal Area per Kebele [m2]") + theme(
  plot.background = element_blank()
  ,panel.background = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
  ,panel.border = element_blank()
  ,axis.line.x = element_line(color = 'black')
  ,axis.line.y = element_line(color = 'black')
  ,text = element_text(size = 12)
  ,axis.text.x = element_blank()
  ,legend.position = "bottom")
ggsave(paste0(getwd(),"/Basalarea_spp_legend.pdf"),width=10,height=5)

ggplot(b.a,aes(species,b.area1)) + geom_bar(stat="identity",aes(fill=species)) + facet_wrap(~kebele,ncol=3)+
  xlab("Species") + ylab("Basal Area per Kebele [m2]") + theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,axis.text.x = element_blank()
    ,legend.position = "none")
ggsave(paste0(getwd(),"/Basalarea_spp.pdf"))

#create .xlsx file
#write.xlsx(b.a[b.a$trans=="AB",],paste0(getwd(),"/TimberAnalysis.xlsx"),sheetName="Aboabo Species")
#write.xlsx(b.a[b.a$trans=="HM",],paste0(getwd(),"/TimberAnalysis.xlsx"),sheetName="Ahomaho Species",append=T)
#write.xlsx(b.a[b.a$trans=="KA",],paste0(getwd(),"/TimberAnalysis.xlsx"),sheetName="Kwameamoebang Species",append=T)

#go through the plots and write a .xlsx 
for(i in 1:nrow(plts)){
  d<-d.f[d.f$Plot==as.character(plts$name[i]),]
  #sort by basal area
  d<-d[order(-d$BA.1),]
  #sum biomass
  d["Total Shade","BM.1"]<-sum(d[d$species!="Coffea arabica","BM.1"])
  d["Total Shade","BM.2"]<-sum(d[d$species!="Coffea arabica","BM.2"],na.rm=T)
  #recombine columns
  f<-data.frame(cbind(as.character(d$species),signif(d$BA.1,3),signif(d$BA.2,3),signif(d$BM.1,3),signif(d$BM.2,3),d$N.1,d$N.2,d$n.1,d$n.2,as.character(d$T.1),as.character(d$T.2),d$Timber),stringsAsFactors = F)
  colnames(f)<-c("Tree species","Basal Area Yr1 (m2/ha)","Basal Area Yr2 (m2/ha)","Biomass Yr1 (Mg/ha)","Biomass Yr2 (Mg/ha)","No of Trees Yr1 (>10 cm DBH)","No of Trees Yr2 (>10 cm DBH)","No of Trees Yr1 (2-10 cm DBH)","No of Trees Yr2 (2-10 cm DBH)","DBH of Trees >30 cm Yr1","DBH of Trees >30 cm Yr2","Forestry Commission Timber Code")
  f[nrow(f),"Tree species"]<-"Shade Tree Total"
  write.xlsx(f,paste0(getwd(),"/ShadeAnalysis.xlsx"),sheetName=as.character(plts$name[i]),append=T)
}

#calculate diversity of plot by basal area, using Shannon's index given by: -sum(ln(propi)^propi)
#where propi is the proportional abundance of a species
dump<-data.frame(Plot=character(),C.dens1=numeric(),C.dens2=numeric(),Shannon.i1=numeric(),Shannon.i2=numeric(),BA1.legume=numeric(),BA2.legume=numeric(),BA1.deciduous=numeric(),BA2.deciduous=numeric(),BA1.shade=numeric(),BA2.shade=numeric(),BA1.npioneer=numeric(),BA2.npioneer=numeric(),BA1.pioneer=numeric(),BA2.pioneer=numeric(),stringsAsFactors = F)
for(i in 1:nrow(plts)){
  dump[i,"Plot"]<-as.character(plts$name[i])
  d<-d.f[as.character(d.f$Plot)==plts$name[i],]
  if(nrow(d[d$species=="Coffea arabica",])==0) dump[i,2:3]<-0 else dump[i,2:3]<-cbind(signif(d[d$species=="Coffea arabica","N.1"],3),signif(d[d$species=="Coffea arabica","N.2"],3))
  #calculate shannon index of shade trees
  d<-d[d$species!="Coffea arabica",]
  TBA1<-sum(d$BA.1)
  TBA2<-sum(d$BA.2)
  dump[i,4:5]<-cbind(-sum((d$BA.1/TBA1)*log(d$BA.1/TBA1),na.rm=T),-sum((d$BA.2/TBA2)*log(d$BA.2/TBA2),na.rm=T))
  #calculate basal area of legumes, deciduous, shade-tolerants, non-pioneers and pioneers
  dump[i,6:15]<-cbind(sum(d[d$Legume==1,"BA.1"]),sum(d[d$Legume==1,"BA.2"]),sum(d[d$Deciduous==1,"BA.1"]),sum(d[d$Deciduous==1,"BA.2"]),sum(d[d$Success=="Shade","BA.1"]),sum(d[d$Success=="Shade","BA.2"]),sum(d[d$Success=="NPLD","BA.1"]),sum(d[d$Success=="NPLD","BA.2"]),sum(d[d$Success=="Pioneer","BA.1"]),sum(d[d$Success=="Pioneer","BA.2"]))
}


#save vegetation data dump
write.csv(dump,paste0(getwd(),"/Tree_plotdata.csv"))



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#pull out most common species across most diverse plots separate by quartiles
div<-read.csv(paste0(getwd(),"/Tree_plotdata.csv"))
div.1<-div[order(div$Shannon.i1,decreasing=T),]
div.1<-div.1[grep("FP",div.1$Plot,invert=T),]
quant<-quantile(div.1$Shannon.i1,na.rm=T)

q1<-as.character(div.1[div.1$Shannon.i1>quant[4],"Plot"])
q2<-as.character(div.1[div.1$Shannon.i1>quant[3]&div.1$Shannon.i1<quant[4],"Plot"])
q3<-as.character(div.1[div.1$Shannon.i1<quant[3],"Plot"])

#compare species across quartiles of diversity
s.q1<-data.frame(table(as.character(d.f[d.f$Plot %in% q1,"species"])))
s.q1<-s.q1[order(s.q1$Freq,decreasing=T),]
s.q2<-data.frame(table(as.character(d.f[d.f$Plot %in% q2,"species"])))
s.q2<-s.q2[order(s.q2$Freq,decreasing=T),]
s.q3<-data.frame(table(as.character(d.f[d.f$Plot %in% q3,"species"])))
s.q3<-s.q3[order(s.q3$Freq,decreasing=T),]

#find common species across all plots
com.sp<-as.character(s.q3[s.q3$Var1 %in% s.q1[s.q1$Var1 %in% s.q2$Var1,"Var1"],"Var1"])

diff.sp.1.2<-setdiff(s.q1$Var1,s.q2$Var1)
diff.sp.1.3<-setdiff(s.q1$Var1,s.q3$Var1)
#common species not in q2 or q3
unique.q1<-diff.sp.1.2[diff.sp.1.2 %in% diff.sp.1.3]

diff.sp.2.3<-setdiff(s.q2$Var1,s.q3$Var1)

#Create figures of forest species dominance with distance, by transect
#open calculations
d.f<-data.frame(read.csv(paste0(getwd(),"/AllPlots_BiomassBasalArea.csv")))
#remove NA
d.f<-d.f[!is.na(d.f$BA.1),]
#remove zero "success" values
d.f<-d.f[d.f$Success!=0&d.f$Success!="",]
colnames(d.f)<-c("x","species",colnames(d.f[,3:18]))
#add distance value
d.f$distance<-plts[match(d.f$Plot,plts$name3),"distance"]
d.f$distance.1<-plts[match(d.f$Plot,plts$name3),"distance.1"]
d.f$transect<-plts[match(d.f$Plot,plts$name3),"transect"]
#remove cocoa trees
d.f<-d.f[d.f$species!="Theobroma cacao",]

df.q1<-d.f[d.f$Plot %in% q1,]


#plot basal area dominance of each successional guild with distance from forest
ggplot(df.q1,aes(factor(distance),BA.1,fill=Success))+geom_bar(stat="identity",position="dodge")+facet_wrap(~transect,nrow=3)+
  ggtitle("[Most Diverse Plots] Basal area of Shade Trees by Successional Guild\nwith Distance from Forest")+xlab("Distance from Forest (m)")+
  ylab("Basal area [m2]")
sp<-data.frame(table(as.character(df.q1$species)))
sp<-sp[order(sp$Freq,decreasing=T),]
sp<-as.character(sp[sp$Freq>3,1])

df.q1.2<-df.q1[df.q1$species %in% sp,]
ggplot(df.q1.2,aes(distance.1,BA.1,group=species))+geom_point(aes(color=species))+geom_smooth(aes(distance.1,BA.1,color=species),method=lm,se=FALSE)+
  ggtitle("[Most Diverse Plots] Basal area of Shade Trees by Successional Guild\nwith Distance from Forest")+xlab("Distance from Forest (m)")+
  ylab("Basal area [m2]")

df.q2<-d.f[d.f$Plot %in% q2,]
ggplot(df.q2,aes(factor(distance),BA.1,fill=Success))+geom_bar(stat="identity",position="dodge")+facet_wrap(~transect,nrow=3)+
  ggtitle("[Less Diverse Plots] Basal area of Shade Trees by Successional Guild\nwith Distance from Forest")+xlab("Distance from Forest (m)")+
  ylab("Basal area [m2]")
sp<-data.frame(table(as.character(df.q2$species)))
sp<-sp[order(sp$Freq,decreasing=T),]
sp<-as.character(sp[sp$Freq>3,1])

df.q2.2<-df.q2[df.q2$species %in% sp,]
ggplot(df.q2.2,aes(distance.1,BA.1,group=species))+geom_point(aes(color=species))+geom_smooth(aes(distance.1,BA.1,color=species),method=lm,se=FALSE)+
  ggtitle("[Less Diverse Plots] Basal area of Shade Trees by Successional Guild\nwith Distance from Forest")+xlab("Distance from Forest (m)")+
  ylab("Basal area [m2]")



df<-ddply(d.f,.(distance,Success),summarise,BA.1=mean(BA.1))
ggplot(df,aes(factor(distance),log(BA.1),group=Success))+geom_point(aes(color=Success))+geom_line(aes(color=Success))

df2<-ddply(d.f,.(distance,Success,transect),summarise,BA.1=mean(BA.1))

ggplot(df2,aes(factor(distance),log(BA.1),group=Success))+geom_point(aes(color=Success))+geom_line(aes(color=Success))+facet_wrap(~transect,nrow=3)+
  ggtitle("Basal area of Forest Trees by Successional Guild\nwith Distance from Forest")+xlab("Distance from Forest (m)")+
  ylab("log(Basal area) [m2]")

#start with Ahomaho
#df<-d.f[d.f$trans=="HM",]

#check for most common species of shade bearers
x1<-d.f[d.f$Success=="Shade",]
sh<-data.frame(table(as.character(x1$species)))
sh<-sh[order(sh$Freq,decreasing=T),]
sh<-as.character(sh[sh$Freq>4,1])

df.sh<-d.f[d.f$Success=="Shade"&d.f$distance!=0,]
df.sh<-df.sh[df.sh$species %in% sh,]

ggplot(df.sh,aes(distance.1,BA.1,group=species))+geom_point(aes(color=species))+geom_smooth(aes(distance.1,BA.1,color=species),method=lm,se=FALSE)+
  ggtitle("Basal area of Shade Tolerant Species\nwith Distance from Forest")+xlab("Distance (m)")+ylab("Basal Area [m2/ha]")

#check for most common species of NPLD
np<-data.frame(table(as.character(d.f[d.f$Success=="Non-pioneer","species"])))
np<-np[order(np$Freq,decreasing=T),]
np<-as.character(np[np$Freq>3,1])

df.np<-d.f[d.f$Success=="Non-pioneer"&d.f$distance!=0,]
df.np<-df.np[df.np$species %in% np,]

ggplot(df.np,aes(distance.1,BA.1,group=species))+geom_point(aes(color=species))+geom_smooth(aes(distance.1,BA.1,color=species),method=lm,se=FALSE)+
  ggtitle("Basal area of Non-pioneer Species\nwith Distance from Forest")+xlab("Distance (m)")+ylab("Basal Area [m2/ha]")


#check for most common species of Pioneer
p<-data.frame(table(as.character(d.f[d.f$Success=="Pioneer","species"])))
p<-p[order(p$Freq,decreasing=T),]
p<-as.character(p[p$Freq>4,1])
df.p<-d.f[d.f$Success=="Pioneer"&d.f$distance!=0,]
df.p<-df.p[df.p$species %in% p,]

ggplot(df.p,aes(distance.1,log(BA.1),group=species))+geom_point(aes(color=species))+geom_smooth(aes(distance.1,log(BA.1),color=species),method=lm,se=FALSE)+
  ggtitle("Basal area of Pioneer Species\nwith Distance from Forest")+xlab("Distance (m)")+ylab("Basal Area [m2/ha]")

#do again for timber species
timb<-data.frame(table(as.character(d.f[d.f$Timber>0,"species"])))
timb<-timb[order(timb$Freq,decreasing=T),]
timb<-as.character(timb[timb$Freq>9,1])

df.t<-d.f[d.f$Timber>0&d.f$distance!=0,]
df.t<-df.t[df.t$species %in% timb,]
ggplot(df.t,aes(distance.1,BA.1,group=species))+geom_point(aes(color=species))+geom_smooth(aes(distance.1,BA.1,color=species),method=lm,se=FALSE)+
  ggtitle("Basal area of Timber Species\nwith Distance from Forest")+xlab("Distance (m)")+ylab("Basal Area [m2/ha]")

#