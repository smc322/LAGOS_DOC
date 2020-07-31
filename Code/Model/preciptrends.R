##precip anomoly trends. get hu12 monthly precip data from EDI:

library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)

infile1  <- "https://pasta.lternet.edu/package/data/eml/edi/214/1/a52cf99cb2715c9dc3f01b912ff611ee" 
infile1 <- sub("^https","http",infile1) 
dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "hu12_zoneid",     
                 "year",     
                 "ppt_January",     
                 "ppt_February",     
                 "ppt_March",     
                 "ppt_April",     
                 "ppt_May",     
                 "ppt_June",     
                 "ppt_July",     
                 "ppt_August",     
                 "ppt_September",     
                 "ppt_October",     
                 "ppt_November",     
                 "ppt_December",     
                 "ppt_winter",     
                 "ppt_spring",     
                 "ppt_summer",     
                 "ppt_fall",     
                 "ppt_annual"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$hu12_zoneid)!="factor") dt1$hu12_zoneid<- as.factor(dt1$hu12_zoneid)
if (class(dt1$ppt_January)=="factor") dt1$ppt_January <-as.numeric(levels(dt1$ppt_January))[as.integer(dt1$ppt_January) ]
if (class(dt1$ppt_February)=="factor") dt1$ppt_February <-as.numeric(levels(dt1$ppt_February))[as.integer(dt1$ppt_February) ]
if (class(dt1$ppt_March)=="factor") dt1$ppt_March <-as.numeric(levels(dt1$ppt_March))[as.integer(dt1$ppt_March) ]
if (class(dt1$ppt_April)=="factor") dt1$ppt_April <-as.numeric(levels(dt1$ppt_April))[as.integer(dt1$ppt_April) ]
if (class(dt1$ppt_May)=="factor") dt1$ppt_May <-as.numeric(levels(dt1$ppt_May))[as.integer(dt1$ppt_May) ]
if (class(dt1$ppt_June)=="factor") dt1$ppt_June <-as.numeric(levels(dt1$ppt_June))[as.integer(dt1$ppt_June) ]
if (class(dt1$ppt_July)=="factor") dt1$ppt_July <-as.numeric(levels(dt1$ppt_July))[as.integer(dt1$ppt_July) ]
if (class(dt1$ppt_August)=="factor") dt1$ppt_August <-as.numeric(levels(dt1$ppt_August))[as.integer(dt1$ppt_August) ]
if (class(dt1$ppt_September)=="factor") dt1$ppt_September <-as.numeric(levels(dt1$ppt_September))[as.integer(dt1$ppt_September) ]
if (class(dt1$ppt_October)=="factor") dt1$ppt_October <-as.numeric(levels(dt1$ppt_October))[as.integer(dt1$ppt_October) ]
if (class(dt1$ppt_November)=="factor") dt1$ppt_November <-as.numeric(levels(dt1$ppt_November))[as.integer(dt1$ppt_November) ]
if (class(dt1$ppt_December)=="factor") dt1$ppt_December <-as.numeric(levels(dt1$ppt_December))[as.integer(dt1$ppt_December) ]
if (class(dt1$ppt_winter)=="factor") dt1$ppt_winter <-as.numeric(levels(dt1$ppt_winter))[as.integer(dt1$ppt_winter) ]
if (class(dt1$ppt_spring)=="factor") dt1$ppt_spring <-as.numeric(levels(dt1$ppt_spring))[as.integer(dt1$ppt_spring) ]
if (class(dt1$ppt_summer)=="factor") dt1$ppt_summer <-as.numeric(levels(dt1$ppt_summer))[as.integer(dt1$ppt_summer) ]
if (class(dt1$ppt_fall)=="factor") dt1$ppt_fall <-as.numeric(levels(dt1$ppt_fall))[as.integer(dt1$ppt_fall) ]
if (class(dt1$ppt_annual)=="factor") dt1$ppt_annual <-as.numeric(levels(dt1$ppt_annual))[as.integer(dt1$ppt_annual) ]

# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(hu12_zoneid)
summary(year)
summary(ppt_January)
summary(ppt_February)
summary(ppt_March)
summary(ppt_April)
summary(ppt_May)
summary(ppt_June)
summary(ppt_July)
summary(ppt_August)
summary(ppt_September)
summary(ppt_October)
summary(ppt_November)
summary(ppt_December)
summary(ppt_winter)
summary(ppt_spring)
summary(ppt_summer)
summary(ppt_fall)
summary(ppt_annual) 
detach(dt1)               


#get rid of extraneous cols and make col names for months into numeric data that we want in our month column after melting

dt.mo<-dt1[,c(1:14)]
dt.mo.2 <- dt.mo[dt.mo$year>1979,]

names(dt.mo.2)<-c("hu12_zoneid", "year", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
pptv<-melt(dt.mo.2, id=c("hu12_zoneid", "year"))
names(pptv)<-c("hu12_zoneid", "year", "month", "pptmo")

pptv$month = as.numeric(as.character(pptv$month))

pptv$moyr= ym(paste(pptv$year, pptv$month, sep='-'), format="%Y-%M")

#save it in case we need it for anything else
saveRDS(pptv, "Datasets/hu12_ppt_vert.rds")

#match precip data with DOC data - realized we just need annual
dt.an<-dt1[,c(1,2,19)]
pptan <- dt.an[dt.an$year>1979,]
names(pptan)<- c("hu12_zoneid", "sampleyear", "pptann")

#calc average ppt for eaach hu12, then subtract those averages from annual data from each data point to get anomoly

hu12medppt<- pptan %>% group_by(hu12_zoneid) %>% summarise(hu12medppt=median(pptann, na.rm=T))
pptwmed<-merge(pptan, hu12medppt, by="hu12_zoneid")
pptwmed$pptanom<-pptwmed$pptann-pptwmed$hu12medppt

doc.dat<-readRDS("Datasets/doc_july18.rds")
hu12.llid<-lagos$locus[,c("lagoslakeid", "hu12_zoneid")]
doc.hu12<-merge(doc.dat, hu12.llid, by="lagoslakeid")
doc.ppt<-merge(doc.hu12, pptwmed, by=c("hu12_zoneid", "sampleyear"))

#save precip data associated with doc data to run model, rename cols to match input to make JAGS conversion easy

precip.model<-doc.ppt[,c(2,3,5,8)]
names(precip.model)=c("sampleyear", "lagoslakeid", "year1", "ly.med")
saveRDS(precip.model, "Datasets/precipdoc_july18.rds")


#match precip anom for color same way

col.dat<-readRDS("Datasets/color_mar19.rds")
hu12.llid<-lagos$locus[,c("lagoslakeid", "hu12_zoneid")]
col.hu12<-merge(col.dat, hu12.llid, by="lagoslakeid")
col.ppt<-merge(col.hu12, pptwmed, by=c("hu12_zoneid", "sampleyear"))

#save precip data associated with doc data to run model, rename cols to match input to make JAGS conversion easy

precip.model<-col.ppt[,c(2,3,5,8)]
names(precip.model)=c("sampleyear", "lagoslakeid", "year1", "ly.med")
saveRDS(precip.model, "Datasets/precipcol_mar19.rds")
