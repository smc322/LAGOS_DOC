library(reshape)
library(data.table)
require(lme4)
library(maps)
library(dplyr)
library(LAGOSNE)

lagos<-lagosne_load(version="1.087.1")

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
data$colort_calc[which(data$colort_calc<10)]<-NA
data$colort_combined=data$colort
data$colort_combined[which(is.na(data$colort_combined))] = data$colort_calc[which(is.na(data$colort_combined))]
data$colort_combined[which(data$colort_combined<.5)]<-NA


datafilter <- function(var) {
  #data for variable of interest
  varofinterest <- na.omit(data[, c("lagoslakeid", var, "sampleyear", "samplemonth", "sampledate")])
  #only keep 1980 and later
  modern <- varofinterest[varofinterest$sampleyear>1979,]
  #limit to summer months
  #sfull = modern[modern$samplemonth == 6|modern$samplemonth == 7|modern$samplemonth==8|modern$samplemonth==9, ]
  #limit to 15 June and 15 Sept
  # sfull$sampleday = format(sfull$sampledate, "%d")
  # sfull$sampleday = as.numeric(sfull$sampleday)
  # 
  # keep.june = which(sfull$samplemonth == 6 & sfull$sampleday >= 15)
  # keep.july = which(sfull$samplemonth == 7)
  # keep.august = which(sfull$samplemonth == 8)
  # keep.september = which(sfull$samplemonth==9 & sfull$sampleday <= 15)
  # keep.all = c(keep.june, keep.july, keep.august, keep.september)
  # keep.all = as.numeric(keep.all)
  # 
  # seasonal<-sfull[keep.all, ]
  
  #only keep lakes with 5 years of data
  lakeyrmed<- modern %>% group_by(lagoslakeid, sampleyear) %>% summarise(ly.med=median(get(var), na.rm=T))
  lakemed<- na.omit(lakeyrmed %>% group_by(lagoslakeid) %>% summarise(l.med=median(ly.med, na.rm=T), count=n()))
  yrs5<- filter(lakemed, count>4)
  lakekeeps<-unique(yrs5$lagoslakeid)
  
  lakeyrmeds5 <- lakeyrmed[lakeyrmed$lagoslakeid %in% lakekeeps, ]
  lakeyrmeds5$year1 <-lakeyrmeds5$sampleyear-1980
  
  
  lakes = unique(lakeyrmeds5$lagoslakeid)
  years = c(1980:2013)
  keep = c()
  for (j in 1:length(unique(lakeyrmeds5$lagoslakeid))) {
    lake.years = lakeyrmeds5$sampleyear[lakeyrmeds5$lagoslakeid == lakes[j]]
    spread = max(lake.years) - min(lake.years)
    if (spread > 9){
      keep[j] = TRUE
    } else {
      keep[j] = FALSE
    }
  }
  lakes = unique(lakeyrmeds5$lagoslakeid)[keep == TRUE]
  
  return(lakeyrmeds5[lakeyrmeds5$lagoslakeid %in% lakes, ])

  }

data.doc.yr<-datafilter("doc")
data.tp.yr<-datafilter("tp")
data.col.yr<-datafilter("colort_combined")
data.tn.yr<-datafilter("tn_combined")
data.no3.yr<-datafilter("no2no3")
data.chla.yr<-datafilter("chla")

##make map of above

locus<-lagos$locus
cordsids<-unique(locus[,c("lagoslakeid", "nhd_lat", "nhd_long", "state_zoneid")])

mapofvar<- function (varoutput, filename)  {
  
varloc<-merge(varoutput, cordsids, by="lagoslakeid", all.x=T, all.y=F)

pdf(filename, width=13, height=8)
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30", lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
points(varloc$nhd_long, varloc$nhd_lat, pch=21, col="black", lwd=.5, bg=rgb(169,169,169, 200,max=255), cex=1)
dev.off()

}

mapofvar(data.doc.yr, "Figures/DOC_map.pdf")
mapofvar(data.tp.yr, "Figures/TP_map.pdf")
mapofvar(data.col.yr, "Figures/old/Col_map.pdf")
mapofvar(data.tn.yr, "Figures/TN_map.pdf")
mapofvar(data.no3.yr, "Figures/NO3_map.pdf")
mapofvar(data.chla.yr, "Figures/Chl_map.pdf")

#wtf to do about MI?
doccords<-merge(data.doc.yr, cordsids, by="lagoslakeid", all.x=T, all.y=F)
colcords<-merge(data.col.yr, cordsids, by="lagoslakeid", all.x=T, all.y=F)
milakes<-doccords[doccords$state_zoneid=="State_3",]
milakes.col<-colcords[colcords$state_zoneid=="State_3",]
midocid<-unique(milakes$lagoslakeid)
micolid<-unique(milakes.col$lagoslakeid)

overlapdocp <- data.tp.yr[data.tp.yr$lagoslakeid %in% midocid, ]
#17 of 19 overlap
overlapcolp<-data.tp.yr[data.tp.yr$lagoslakeid %in% micolid, ]
#12 of 12 overlap
overlapdoctn<- data.tn.yr[data.tn.yr$lagoslakeid %in% midocid, ]
#1 of 19
overlapcoltn<-data.tn.yr[data.tn.yr$lagoslakeid %in% micolid, ]
#1 of 12
overlapdocno3<- data.no3.yr[data.no3.yr$lagoslakeid %in% midocid, ]
#7 of 19
overlapcolno3<-data.no3.yr[data.no3.yr$lagoslakeid %in% micolid, ]
#1 of 12

##decided to keep nutrient data from hu8s that are associated with all color and doc lakes combined, so generate list of lagoslakeids and hu8s that include them, then axe any nut data that are not in those.

lakehu8<- lagos$locus[,c("lagoslakeid", "hu8_zoneid")]
includehu8<-lakehu8[lakehu8$lagoslakeid %in% data.doc.yr$lagoslakeid | lakehu8$lagoslakeid %in% data.col.yr$lagoslakeid, ]
length(unique(includehu8$hu8_zoneid))

#merge nutrient data with hu8 identifier then keep hu8s that are on the list above

tphu8<-merge(data.tp.yr, lakehu8, by="lagoslakeid", all.x=T, all.y=F)
tp.dat.analysis<-tphu8[tphu8$hu8_zoneid %in% includehu8$hu8_zoneid, ]
mapofvar(tp.dat.analysis, "Figures/TPAnalysis.pdf")


tnhu8<-merge(data.tn.yr, lakehu8, by="lagoslakeid", all.x=T, all.y=F)
tn.dat.analysis<-tnhu8[tnhu8$hu8_zoneid %in% includehu8$hu8_zoneid, ]
mapofvar(tn.dat.analysis, "Figures/TNAnalysis.pdf")


no3hu8<-merge(data.no3.yr, lakehu8, by="lagoslakeid", all.x=T, all.y=F)
no3.dat.analysis<-no3hu8[no3hu8$hu8_zoneid %in% includehu8$hu8_zoneid, ]
mapofvar(no3.dat.analysis, "Figures/NO3Analysis.pdf")


chlhu8<-merge(data.chla.yr, lakehu8, by="lagoslakeid", all.x=T, all.y=F)
chla.dat.analysis<-chlhu8[chlhu8$hu8_zoneid %in% includehu8$hu8_zoneid, ]
mapofvar(chla.dat.analysis, "Figures/ChlaAnalysis.pdf")


#save datasets for analysis!
saveRDS(data.col.yr, file="Datasets/color_mar19.rds")
saveRDS(data.doc.yr, file="Datasets/doc_july18.rds")
saveRDS(tp.dat.analysis, file="Datasets/tp_july18.rds")
saveRDS(tn.dat.analysis, file="Datasets/tn_july18.rds")
saveRDS(no3.dat.analysis, file="Datasets/no3_july18.rds")
saveRDS(chla.dat.analysis, file="Datasets/chl_july18.rds")
