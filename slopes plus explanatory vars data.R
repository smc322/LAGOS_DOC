
pctchgs<-readRDS(file="Datasets/SlopesCVars.rds")

library(LAGOSNE)
lagos <-lagosne_load(version = "1.087.1")

pptcol<-readRDS("Datasets/JAGS_PPTCOL_july18.rds")
pptdoc<-readRDS("Datasets/JAGS_PPTDOC_july18.rds")

pptcol$var="color"
pptdoc$var="doc"
pptcol$pptpctchg=pptcol$slopemean*100
pptdoc$pptpctchg=pptdoc$slopemean*100
pc<-pptcol[,c("lagoslakeid", "var", "pptpctchg")]
pd<-pptdoc[,c("lagoslakeid", "var", "pptpctchg")]

pptchg<-rbind(pc, pd)

chgppt<-na.omit(merge(pctchgs, pptchg, by=c("lagoslakeid", "var"), all.x=T, all.y=T))

so4dep<-lagos$hu12.chag[,c("hu12_zoneid", "hu12_dep_so4_1985_mean", "hu12_dep_so4_2010_mean")]
hu12.llid<-lagos$locus[,c("lagoslakeid", "hu12_zoneid")]
so4dep.llid<-merge(so4dep, hu12.llid, by="hu12_zoneid")
so4dep.llid$so4depchange<-so4dep.llid$hu12_dep_so4_2010_mean-so4dep.llid$hu12_dep_so4_1985_mean
so4dep.llid$so4changepct<-so4dep.llid$so4depchange/so4dep.llid$hu12_dep_so4_1985_mean*100
so4dat<-so4dep.llid[,c("lagoslakeid", "so4changepct")]

chgpptso4<-merge(chgppt, so4dat, by="lagoslakeid")

cordsids<-unique(lagos$locus[,c("lagoslakeid", "nhd_lat", "nhd_long")])

chgpptso4cords<-merge(chgpptso4, cordsids, by="lagoslakeid", all.x=T, all.y=F)


#wetlands = 90 and 95

lulc<-lagos$iws.lulc
lulc$wetlands<- lulc$iws_nlcd2006_pct_90+lulc$iws_nlcd2006_pct_95
wtl<-lulc[,c("lagoslakeid", "wetlands")]


chagpptso4cordswtl<-merge(chgpptso4cords, wtl, by="lagoslakeid", all.x=T, all.y=F)


depth<-lagos$lakes_limno[,c("lagoslakeid", "maxdepth")]


chagpptso4cordswtldepth<-merge(chagpptso4cordswtl, depth, by="lagoslakeid", all.x=T, all.y=F)


write.csv(chagpptso4cordswtldepth, file="Datasets/dataandcovarsJF.csv")
