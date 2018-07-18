#want to compare median color/doc/tp to slopes, calc those here

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
data$colort_combined=data$colort
data$colort_combined[which(is.na(data$colort_combined))] = data$colort_calc[which(is.na(data$colort_combined))]



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
  
  return(lakemed[lakemed$lagoslakeid %in% lakes, ])

  }

data.doc.med<-datafilter("doc")
names(data.doc.med)=c("lagoslakeid", "doc.med", "doc.nobs")
data.tp.med<-datafilter("tp")
names(data.tp.med)=c("lagoslakeid", "tp.med", "tp.nobs")
data.col.med<-datafilter("colort_combined")
names(data.col.med)=c("lagoslakeid", "col.med", "col.nobs")
data.tn.med<-datafilter("tn_combined")
names(data.tn.med)=c("lagoslakeid", "tn.med", "tn.nobs")
data.no3.med<-datafilter("no2no3")
names(data.no3.med)=c("lagoslakeid", "no3.med", "no3.nobs")
data.chla.med<-datafilter("chla")
names(data.chla.med)=c("lagoslakeid", "chla.med", "chla.nobs")
