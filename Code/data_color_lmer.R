##want to figure out doc change over time r for each lake with at least 5 yrs data... set up mixed model, DOC vs time for each lake
library(data.table)
require(lme4)
library(maps)
library(dplyr)
library(LAGOS)
data<-lagos_load(version="1.087.1")

col.dat <- na.omit(data$epi.nutr[, c("lagoslakeid", "colort", "sampleyear")])
col.dat <- col.dat[col.dat$sampleyear>1979,]

#make median response by lake, first median for each year, then median of those
lakeyrmed<- col.dat %>% group_by(lagoslakeid, sampleyear) %>% summarise(ly.med=median(colort, na.rm=T))
lakemed<- na.omit(lakeyrmed %>% group_by(lagoslakeid) %>% summarise(l.med=median(ly.med, na.rm=T), count=n()))
yrs5<- filter(lakemed, count>4)
lakekeeps<-unique(yrs5$lagoslakeid)

lakeyrmeds5 <- lakeyrmed[lakeyrmed$lagoslakeid %in% lakekeeps, ]
lakeyrmeds5$year1 <-lakeyrmeds5$sampleyear-1980

#write file to give to Ty
saveRDS(lakeyrmeds5, file="Datasets/ColorTdata.rds")

cordsids<-data$locus[,c("lagoslakeid", "nhd_lat", "nhd_long")]
lakecords<-merge(yrs5, cordsids, by="lagoslakeid", all.x=T, all.y=F)

pdf("realdoc_nobs_map.pdf", width=13, height=8)
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30", lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
points(lakecords$nhd_long, lakecords$nhd_lat, pch=21, col="black", lwd=.5, bg="black", cex=1)

dev.off()

docmod <- lmer(ly.med ~ year1 +
                (1+ year1|lagoslakeid) , data=lakeyrmeds5, REML=FALSE) 
summary(docmod)
blups<-coef(docmod)$lagoslakeid
blups<-setDT(blups, keep.rownames=TRUE)[]
names(blups)<-c("lagoslakeid", "intercept", "slope")

positive<- filter(blups, slope>0)
poswcord<-merge(positive, cordsids, by="lagoslakeid", all.x=T, all.y=F)
negative<- filter(blups, slope<0)
negwcord<-merge(negative, cordsids, by="lagoslakeid", all.x=T, all.y=F)

pdf("posnegslope_color_map.pdf", width=13, height=8)
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30", lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
points(poswcord$nhd_long, poswcord$nhd_lat, pch=21, col="black", lwd=.5, bg="red", cex=1)
points(negwcord$nhd_long, negwcord$nhd_lat, pch=21, col="black", lwd=.5, bg="blue", cex=1)

dev.off()

depth<-na.omit(data$lake.specific[,c(1,6)])
depth$lakeid<-as.character(depth$lagoslakeid)
area<-na.omit(data$locus[,c(1,6)])
tp<-na.omit(data$epi.nutr[,c(2,22, 93)])
tplymed<- tp %>% group_by(lagoslakeid, sampleyear) %>% summarise(ly.med=median(tp, na.rm=T))
tplakemed<- na.omit(lakeyrmed %>% group_by(lagoslakeid) %>% summarise(l.med=median(ly.med, na.rm=T), count=n()))

blup.d<-merge(blups, depth, by="lagoslakeid", all.x=T, all.y=F)
