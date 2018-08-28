library(lubridate)
library(tidyverse)
library(sf)
library(LAGOSNE)
library(USAboundaries)
library(USAboundariesData)
library(cowplot)
library(maps)

#get data files
lagos <-lagosne_load(version = "1.087.1")

doc<-readRDS(file="Datasets/JAGS_DOC_july18.rds")
#doc$lagoslakeid=as.numeric(doc$lagoslakeid)
col<-readRDS(file="Datasets/JAGS_Color_july18.rds")
tn<-readRDS(file="Datasets/JAGS_TN_july18.rds")
tp<-readRDS(file="Datasets/JAGS_TP_july18.rds")
no3<-readRDS(file="Datasets/JAGS_NO3_july18.rds")


locus<-lagos$locus
cordsids<-unique(locus[,c("lagoslakeid", "nhd_lat", "nhd_long")])

mapofvar<- function (varoutput, filename)  {
  varoutput$slopeProbs[which(varoutput$slopeSign==0)] <- 
    abs(varoutput$slopeProbs[which(varoutput$slopeSign==0)]-1)
  
  color.gradient <- function(x, colors=c("blue","white","red"), colsteps=100) {
    return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
  }
  
  varloc<-merge(varoutput, cordsids, by="lagoslakeid", all.x=T, all.y=F)
  
  png(filename, width=13, height=5, units="in")
  map(database = "state", regions=c("Minnesota", "Wisconsin", "Michigan","Pennsylvania","New York",
                                    "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                    "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30", lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
  points(varloc$nhd_long, varloc$nhd_lat, pch=21, col="black", bg=color.gradient(varoutput$slopeProbs), cex=1)
  dev.off()
  
}

mapofvar(doc, "Figures/DOCSlopeProbMap.pdf")
mapofvar(col, "Figures/ColSlopeProbMap.pdf")
mapofvar(tn, "Figures/TNSlopeProbMap.pdf")
mapofvar(tp, "Figures/TPSlopeProbMap.pdf")
mapofvar(no3, "Figures/NO3SlopeProbMap.pdf")
