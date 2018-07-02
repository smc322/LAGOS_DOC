library(data.table)
require(lme4)
library(maps)
library(dplyr)
library(LAGOS)

data<-lagos_load(version="1.087.1")
output<-readRDS(file="Datasets/JAGS_Color.rds")

cordsids<-data$locus[,c("lagoslakeid", "nhd_lat", "nhd_long")]
outputcords<-merge(output, cordsids, by="lagoslakeid", all.x=T, all.y=F)

nuts<-data$epi.nutr
nutsrel<-nuts[,c("lagoslakeid", "tp", "tn", "chla", "secchi", "doc", "colort", "sampleyear")]

lakeyrmeds<- nutsrel %>% group_by(lagoslakeid, sampleyear) %>% summarise(ly.p.med=median(tp, na.rm=T), ly.n.med=median(tn, na.rm=T), ly.ch.med=median(chla, na.rm=T), ly.sec.med=median(secchi, na.rm=T), ly.doc.med=median(doc, na.rm=T), ly.col.med=median(colort, na.rm=T))
lakemed<- lakeyrmeds %>% group_by(lagoslakeid) %>% summarise(l.p.med=median(ly.p.med, na.rm=T), l.n.med=median(ly.n.med, na.rm=T), l.ch.med=median(ly.ch.med, na.rm=T), l.sec.med=median(ly.sec.med, na.rm=T), l.doc.med=median(ly.doc.med, na.rm=T), l.col.med=median(ly.col.med, na.rm=T))

outputcordsnuts<-merge(outputcords, lakemed, by="lagoslakeid", all.x=T, all.y=F)
postrend<-outputcordsnuts[outputcordsnuts$slopeSign==1,]
negtrend<-outputcordsnuts[outputcordsnuts$slopeSign==0,]
