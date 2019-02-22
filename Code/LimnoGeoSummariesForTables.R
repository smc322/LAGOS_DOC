#create summaries of limno and geo data for two tables in paper
#run "medianstocompare.R" to get limno medians
#summarize median, range of median, number of years of obs (median and range) for each variable

summary(data.doc.med)
summary(data.col.med)

#restrict to appropriate HU8 for nurients

data.col.yr<-readRDS(file="Datasets/color_july18.rds")
data.doc.yr<-readRDS(file="Datasets/doc_july18.rds")

lakehu8<- lagos$locus[,c("lagoslakeid", "hu8_zoneid")]
includehu8<-lakehu8[lakehu8$lagoslakeid %in% data.doc.yr$lagoslakeid | lakehu8$lagoslakeid %in% data.col.yr$lagoslakeid, ]
length(unique(includehu8$hu8_zoneid))


tphu8<-merge(data.tp.med, lakehu8, by="lagoslakeid", all.x=T, all.y=F)
tp.dat.analysis<-tphu8[tphu8$hu8_zoneid %in% includehu8$hu8_zoneid, ]
summary(tp.dat.analysis)

tnhu8<-merge(data.tn.med, lakehu8, by="lagoslakeid", all.x=T, all.y=F)
tn.dat.analysis<-tnhu8[tnhu8$hu8_zoneid %in% includehu8$hu8_zoneid, ]
summary(tn.dat.analysis)


no3hu8<-merge(data.no3.med, lakehu8, by="lagoslakeid", all.x=T, all.y=F)
no3.dat.analysis<-no3hu8[no3hu8$hu8_zoneid %in% includehu8$hu8_zoneid, ]
summary(no3.dat.analysis)


chlhu8<-merge(data.chla.med, lakehu8, by="lagoslakeid", all.x=T, all.y=F)
chla.dat.analysis<-chlhu8[chlhu8$hu8_zoneid %in% includehu8$hu8_zoneid, ]
summary(chla.dat.analysis)


#geo variables/predictors - run covars_updatedNov2018.R

chagso4lulcmorph<-merge(chagso4lulcdepth, area, by="lagoslakeid", all.x=T, all.y=F)
tp<-lakemedp[,c(1,2)]

covars.tp<-merge(chagso4lulcmorph, tp, by="lagoslakeid", all.x=T, all.y=T)

datalakescovars<-covars.tp[covars.tp$lagoslakeid %in% pc$lagoslakeid | covars.tp$lagoslakeid %in% pd$lagoslakeid, ]


#what are years for precip trend, again?
pptrawdatmod<-readRDS("Datasets/precipcol_july18.rds")
