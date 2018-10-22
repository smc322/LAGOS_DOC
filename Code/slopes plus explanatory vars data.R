
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

map.ndep.mat<-lagos$hu12.chag[,c("hu12_zoneid", "hu12_dep_no3_2000_mean", "hu12_runoff_mean", "hu12_prism_ppt_30yr_normal_800mm2_annual_mean", "hu12_prism_tmean_30yr_normal_800mm2_annual_mean", "hu12_baseflowindex_mean")]

chag.llid<-merge(map.ndep.mat, hu12.llid, by="hu12_zoneid")

chag.chgs<-merge(chgpptso4, chag.llid, by="lagoslakeid", all.x=T, all.y=F)

cordsids<-unique(lagos$locus[,c("lagoslakeid", "nhd_lat", "nhd_long")])

chgpptso4cords<-merge(chag.chgs, cordsids, by="lagoslakeid", all.x=T, all.y=F)


#wetlands = 90 and 95

lulc<-lagos$iws.lulc
lulc$wetlands<- lulc$iws_nlcd2006_pct_90+lulc$iws_nlcd2006_pct_95
lulc$agri<- lulc$iws_nlcd2006_pct_81 +lulc$iws_nlcd2006_pct_82
lulc$urban<- lulc$iws_nlcd2006_pct_22 + lulc$iws_nlcd2006_pct_23 + lulc$iws_nlcd2006_pct_24

  
lulc.sums<-lulc[,c("lagoslakeid", "wetlands", "agri", "urban")]


chagpptso4cordswtl<-merge(chgpptso4cords, lulc.sums, by="lagoslakeid", all.x=T, all.y=F)


depth<-lagos$lakes_limno[,c("lagoslakeid", "maxdepth")]

area<-lagos$locus[,c("lagoslakeid", "lake_area_ha")]


chagpptso4cordswtldepth<-merge(chagpptso4cordswtl, depth, by="lagoslakeid", all.x=T, all.y=F)

all.covars<-merge(chagpptso4cordswtldepth, area, by="lagoslakeid", all.x=T, all.y=F)

all.covars$hu12_zoneid=NULL

write.csv(all.covars, file="Datasets/dataandcovarsJF_17Sept.csv")
