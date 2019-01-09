

library(LAGOSNE)
lagos <-lagosne_load(version = "1.087.1")

data<-lagos$epi_nutr
data$sampledate= as.Date(as.character(data$sampledate), format="%m/%d/%Y")
data$lagoslakeid= as.factor(data$lagoslakeid)

#Calculate TN from TKN
data$tn_calculated = data$tkn + data$no2no3
data$tn_combined = data$tn
data$tn_combined[which(is.na(data$tn_combined))] = data$tn_calculated[which(is.na(data$tn_combined))]

#Estimate colort from colora but not from state agencies
datanosa<-data[which(!data$programtype=="State Agency"),]
tamod<-lm(colort~colora, data=datanosa)
coeffs=coefficients(tamod); coeffs

#colort is .7214*colora -5.145

plot(colort~colora, data=datanosa)
abline(a=-5.145, b=.7214)

data$colort_calc<-(data$colora*.7214)-5.145
data$colort_calc[which(data$programtype=="State Agency")]<-NA
data$colort_combined=data$colort
data$colort_combined[which(is.na(data$colort_combined))] = data$colort_calc[which(is.na(data$colort_combined))]

phosdat <- na.omit(data[, c("lagoslakeid", "tp", "sampleyear", "samplemonth", "sampledate")])
chldat<-na.omit(data[, c("lagoslakeid", "chla", "sampleyear", "samplemonth", "sampledate")])

#only keep 1980 and later, summarize median by lake year first then by lake based on that

modernp <- phosdat[phosdat$sampleyear>1979,]
lakeyrmedp<- modernp %>% group_by(lagoslakeid, sampleyear) %>% summarise(ly.med=median(tp, na.rm=T))
lakemedp<- na.omit(lakeyrmedp %>% group_by(lagoslakeid) %>% summarise(lakemedp=median(ly.med, na.rm=T), count=n()))

modernchl<-chldat[chldat$sampleyear>1979,]
lakeyrmedchl <-modernchl%>% group_by(lagoslakeid, sampleyear) %>% summarise(ly.med=median(chla, na.rm=T))
lakemedchl<- na.omit(lakeyrmedchl %>% group_by(lagoslakeid) %>% summarise(lakemedchla=median(ly.med, na.rm=T), count=n()))

pptcol<-readRDS("Datasets/JAGS_PPTCOL_july18.rds")
pptdoc<-readRDS("Datasets/JAGS_PPTDOC_july18.rds")

pptcol$var="color"
pptdoc$var="doc"
pptcol$pptpctchg=pptcol$slopemean*100
pptdoc$pptpctchg=pptdoc$slopemean*100
pc<-pptcol[,c("lagoslakeid", "var", "pptpctchg")]
pd<-pptdoc[,c("lagoslakeid", "var", "pptpctchg")]

pcchla<-merge(pc, lakemedchl, by="lagoslakeid", all.x=T, all.y=F)

#make all other variables together then merge back into ppt pch changes for each

so4dep<-lagos$hu12.chag[,c("hu12_zoneid", "hu12_dep_so4_1985_mean", "hu12_dep_so4_2010_mean")]
hu12.llid<-lagos$locus[,c("lagoslakeid", "hu12_zoneid")]
so4dep.llid<-merge(so4dep, hu12.llid, by="hu12_zoneid")
so4dep.llid$so4depchange<-so4dep.llid$hu12_dep_so4_2010_mean-so4dep.llid$hu12_dep_so4_1985_mean
so4dep.llid$so4changepct<-so4dep.llid$so4depchange/so4dep.llid$hu12_dep_so4_1985_mean*100
so4dat<-so4dep.llid[,c("lagoslakeid", "so4changepct")]


map.ndep.mat<-lagos$hu12.chag[,c("hu12_zoneid", "hu12_dep_no3_2000_mean", "hu12_runoff_mean", "hu12_prism_ppt_30yr_normal_800mm2_annual_mean", "hu12_prism_tmean_30yr_normal_800mm2_annual_mean", "hu12_baseflowindex_mean")]

chag.llid<-merge(map.ndep.mat, hu12.llid, by="hu12_zoneid")

chag.chgs<-merge(so4dat, chag.llid, by="lagoslakeid", all.x=T, all.y=F)

cordsids<-unique(lagos$locus[,c("lagoslakeid", "nhd_lat", "nhd_long")])

chagso4<-merge(chag.chgs, cordsids, by="lagoslakeid", all.x=T, all.y=F)


#wetlands = 90 and 95

lulc<-lagos$iws.lulc
lulc$wetlands<- lulc$iws_nlcd2006_pct_90+lulc$iws_nlcd2006_pct_95
lulc$agri<- lulc$iws_nlcd2006_pct_81 +lulc$iws_nlcd2006_pct_82
lulc$urban<- lulc$iws_nlcd2006_pct_22 + lulc$iws_nlcd2006_pct_23 + lulc$iws_nlcd2006_pct_24
lulc$forest<-lulc$iws_nlcd2006_pct_41+lulc$iws_nlcd2006_pct_42+lulc$iws_nlcd2006_pct_43

  
lulc.sums<-lulc[,c("lagoslakeid", "wetlands", "agri", "urban", "forest")]


chagso4lulc<-merge(chagso4, lulc.sums, by="lagoslakeid", all.x=T, all.y=F)


depth<-lagos$lakes_limno[,c("lagoslakeid", "maxdepth")]

area<-lagos$locus[,c("lagoslakeid", "lake_area_ha")]


chagso4lulcdepth<-merge(chagso4lulc, depth, by="lagoslakeid", all.x=T, all.y=F)

chagso4lulcmorph<-merge(chagso4lulcdepth, area, by="lagoslakeid", all.x=T, all.y=F)

#that file is all covariates except precip change - do that separately for each, add chla and tp separately for each
covarsppt.col<-merge(chagso4lulcmorph, pc, by="lagoslakeid", all.x=F, all.y=T)
chla<-lakemedchl[,c(1,2)]
col.chla<-merge(covarsppt.col, chla, by="lagoslakeid", all.x=T, all.y=F)
tp<-lakemedp[,c(1,2)]
col.chla.tp<-merge(col.chla, tp, by="lagoslakeid", all.x=T, all.y=F)
#retain 402 rows even after NAs omittted

covarsppt.doc<-merge(chagso4lulcmorph, pd, by="lagoslakeid", all.x=F, all.y=T)
doc.chla<-merge(covarsppt.doc, chla, by="lagoslakeid", all.x=T, all.y=F)
doc.chla.tp<-merge(doc.chla, tp, by="lagoslakeid", all.x=T, all.y=F)

saveRDS(col.chla.tp, file="Datasets/ColorCovars_Nov18.rds")
saveRDS(doc.chla.tp, file="Datasets/DOCCovars_Nov18.rds")
