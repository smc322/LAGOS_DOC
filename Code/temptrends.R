##temp trends. get hu12 monthly tmean data from EDI:

library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)

infile3  <- "https://pasta.lternet.edu/package/data/eml/edi/214/1/73042ba3b653624fd887e1a456249cec" 
infile3 <- sub("^https","http",infile3) 
dt3 <-read.csv(infile3,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "hu12_zoneid",     
                 "year",     
                 "tmean_January",     
                 "tmean_February",     
                 "tmean_March",     
                 "tmean_April",     
                 "tmean_May",     
                 "tmean_June",     
                 "tmean_July",     
                 "tmean_August",     
                 "tmean_September",     
                 "tmean_October",     
                 "tmean_November",     
                 "tmean_December",     
                 "tmean_winter",     
                 "tmean_spring",     
                 "tmean_summer",     
                 "tmean_fall",     
                 "tmean_annual"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$hu12_zoneid)!="factor") dt3$hu12_zoneid<- as.factor(dt3$hu12_zoneid)
if (class(dt3$tmean_January)=="factor") dt3$tmean_January <-as.numeric(levels(dt3$tmean_January))[as.integer(dt3$tmean_January) ]
if (class(dt3$tmean_February)=="factor") dt3$tmean_February <-as.numeric(levels(dt3$tmean_February))[as.integer(dt3$tmean_February) ]
if (class(dt3$tmean_March)=="factor") dt3$tmean_March <-as.numeric(levels(dt3$tmean_March))[as.integer(dt3$tmean_March) ]
if (class(dt3$tmean_April)=="factor") dt3$tmean_April <-as.numeric(levels(dt3$tmean_April))[as.integer(dt3$tmean_April) ]
if (class(dt3$tmean_May)=="factor") dt3$tmean_May <-as.numeric(levels(dt3$tmean_May))[as.integer(dt3$tmean_May) ]
if (class(dt3$tmean_June)=="factor") dt3$tmean_June <-as.numeric(levels(dt3$tmean_June))[as.integer(dt3$tmean_June) ]
if (class(dt3$tmean_July)=="factor") dt3$tmean_July <-as.numeric(levels(dt3$tmean_July))[as.integer(dt3$tmean_July) ]
if (class(dt3$tmean_August)=="factor") dt3$tmean_August <-as.numeric(levels(dt3$tmean_August))[as.integer(dt3$tmean_August) ]
if (class(dt3$tmean_September)=="factor") dt3$tmean_September <-as.numeric(levels(dt3$tmean_September))[as.integer(dt3$tmean_September) ]
if (class(dt3$tmean_October)=="factor") dt3$tmean_October <-as.numeric(levels(dt3$tmean_October))[as.integer(dt3$tmean_October) ]
if (class(dt3$tmean_November)=="factor") dt3$tmean_November <-as.numeric(levels(dt3$tmean_November))[as.integer(dt3$tmean_November) ]
if (class(dt3$tmean_December)=="factor") dt3$tmean_December <-as.numeric(levels(dt3$tmean_December))[as.integer(dt3$tmean_December) ]
if (class(dt3$tmean_winter)=="factor") dt3$tmean_winter <-as.numeric(levels(dt3$tmean_winter))[as.integer(dt3$tmean_winter) ]
if (class(dt3$tmean_spring)=="factor") dt3$tmean_spring <-as.numeric(levels(dt3$tmean_spring))[as.integer(dt3$tmean_spring) ]
if (class(dt3$tmean_summer)=="factor") dt3$tmean_summer <-as.numeric(levels(dt3$tmean_summer))[as.integer(dt3$tmean_summer) ]
if (class(dt3$tmean_fall)=="factor") dt3$tmean_fall <-as.numeric(levels(dt3$tmean_fall))[as.integer(dt3$tmean_fall) ]
if (class(dt3$tmean_annual)=="factor") dt3$tmean_annual <-as.numeric(levels(dt3$tmean_annual))[as.integer(dt3$tmean_annual) ]

# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(hu12_zoneid)
summary(year)
summary(tmean_January)
summary(tmean_February)
summary(tmean_March)
summary(tmean_April)
summary(tmean_May)
summary(tmean_June)
summary(tmean_July)
summary(tmean_August)
summary(tmean_September)
summary(tmean_October)
summary(tmean_November)
summary(tmean_December)
summary(tmean_winter)
summary(tmean_spring)
summary(tmean_summer)
summary(tmean_fall)
summary(tmean_annual) 
detach(dt3)               


#get rid of extraneous cols and make col names for months into numeric data that we want in our month column after melting

dt.mo<-dt3[,c(1:14)]
dt.mo.2 <- dt.mo[dt.mo$year>1979,]

names(dt.mo.2)<-c("hu12_zoneid", "year", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
tmeanv<-melt(dt.mo.2, id=c("hu12_zoneid", "year"))
names(tmeanv)<-c("hu12_zoneid", "year", "month", "tmeanmo")

tmeanv$month = as.numeric(as.character(tmeanv$month))

#ym doesn't work for some reason but I think it's bc we didn't end up using it with ppt.
#tmeanv$moyr= ym(paste(tmeanv$year, tmeanv$month, sep='-'), format="%Y-%M")

#save it in case we need it for anything else
#saveRDS(pptv, "Datasets/hu12_ppt_vert.rds")

#match precip data with DOC data - realized we just need annual
dt.an<-dt3[,c(1,2,19)]
tmeanan <- dt.an[dt.an$year>1979,]
names(tmeanan)<- c("hu12_zoneid", "sampleyear", "tmeanann")

#calc average ppt for eaach hu12, then subtract those averages from annual data from each data point to get anomoly

hu12medtmean<- tmeanan %>% group_by(hu12_zoneid) %>% summarise(hu12medtmean=median(tmeanann, na.rm=T))
tmeanwmed<-merge(tmeanan, hu12medtmean, by="hu12_zoneid")
tmeanwmed$tmeananom<-tmeanwmed$tmeanann-tmeanwmed$hu12medtmean

doc.dat<-readRDS("Datasets/doc_july18.rds")
hu12.llid<-lagos$locus[,c("lagoslakeid", "hu12_zoneid")]
doc.hu12<-merge(doc.dat, hu12.llid, by="lagoslakeid")
doc.tmean<-merge(doc.hu12, tmeanwmed, by=c("hu12_zoneid", "sampleyear"))

#save precip data associated with doc data to run model, rename cols to match input to make JAGS conversion easy

tmean.model<-doc.tmean[,c(2,3,5,8)]
names(tmean.model)=c("sampleyear", "lagoslakeid", "year1", "ly.med")
saveRDS(tmean.model, "Datasets/tmeandoc_mar19.rds")


#match precip anom for color same way

col.dat<-readRDS("Datasets/color_mar19.rds")
hu12.llid<-lagos$locus[,c("lagoslakeid", "hu12_zoneid")]
col.hu12<-merge(col.dat, hu12.llid, by="lagoslakeid")
col.tmean<-merge(col.hu12, tmeanwmed, by=c("hu12_zoneid", "sampleyear"))

#save precip data associated with doc data to run model, rename cols to match input to make JAGS conversion easy

tmean.model<-col.tmean[,c(2,3,5,8)]
names(tmean.model)=c("sampleyear", "lagoslakeid", "year1", "ly.med")
saveRDS(tmean.model, "Datasets/tmeancol_mar19.rds")
