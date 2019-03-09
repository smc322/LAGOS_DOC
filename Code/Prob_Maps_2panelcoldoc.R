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

doc$slopeProbs[which(doc$slopeSign==0)] <- 
  abs(doc$slopeProbs[which(doc$slopeSign==0)]-1)

col$slopeProbs[which(col$slopeSign==0)] <- 
  abs(col$slopeProbs[which(col$slopeSign==0)]-1)

alpha=255
colors<- c(rgb(5, 113, 176,max=255, alpha=alpha),
           rgb(146, 197, 222,max=255, alpha=alpha),
           rgb(247, 247, 247,max=255, alpha=alpha),
           rgb(244, 165, 130,max=255, alpha=alpha),
           rgb(202, 0, 32,max=255, alpha=alpha))

get.col.bins <- function(slopes, alpha=175) {
  z=slopes
  #x=1/7
  #quants<-quantile(z, probs = c(x, 2*x, 3*x, 4*x, 5*x, 6*x), na.rm=T)
  
  ii <- cut(z, breaks = c(-Inf, -0.01, 0.0, .0112, .025, Inf), 
            include.lowest = TRUE)
  
  #ii <- cut(z, breaks = c(0,250, 500, 750, 1000, 1250, 1500,Inf), 
  #         include.lowest = TRUE)
  
  #purple blue green
  # levels(ii) <- c(rgb(246,239,247,max=255, alpha=alpha),
  #                 rgb(208,209,230,max=255, alpha=alpha),
  #                 rgb(166,189,219,max=255, alpha=alpha),
  #                 rgb(103,169,207,max=255, alpha=alpha),
  #                 rgb(54,144,192,max=255, alpha=alpha),
  #                 rgb(2,129,138,max=255, alpha=alpha),
  #                 rgb(1,100,80,max=255, alpha=alpha))
  #yellow green blue
  levels(ii) <- c(rgb(5, 113, 176,max=255, alpha=alpha),
                  rgb(146, 197, 222,max=255, alpha=alpha),
                  rgb(247, 247, 247,max=255, alpha=alpha),
                  rgb(244, 165, 130,max=255, alpha=alpha),
                  rgb(202, 0, 32,max=255, alpha=alpha))
  
  
  ii = as.character(ii)
  ii[is.na(ii)==TRUE] <- rgb(255,255,255,max=255)
  return(ii)
}

docloc<-merge(doc, cordsids, by="lagoslakeid", all.x=T, all.y=F)

docloc$sig=3

for (i in 1:nrow(docloc)){
  if (docloc$slopeProbs[i] < 0.9) {
    docloc$sig[i] = 1
  } else {
    docloc$sig[i] = 2
  }
}


colloc<-merge(col, cordsids, by="lagoslakeid", all.x=T, all.y=F)

colloc$sig = 3
for (i in 1:nrow(colloc)){
  if (colloc$slopeProbs[i] < 0.9) {
    colloc$sig[i] = 1
  } else {
    colloc$sig[i] = 2
  }
}


pdf("Figures/Fig1_DOCColProbmaps_slopes.pdf", width=12, height=14)
par(mfrow=c(2,1), oma=c(0,0,0,0), mar=c(0,0,0,0), xpd=NA)

  
  map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois", "Indiana", "Ohio", "Michigan","Pennsylvania","New York",
                                    "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                    "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30", lwd=1)
  points(docloc$nhd_long, docloc$nhd_lat, col= c("grey70", "black")[docloc$sig], bg=get.col.bins(docloc$slopemean), pch = c(21), cex=1.3)
  text(-97, 49.7, "a) DOC % change per year", cex=2, pos=4)
  
  
  map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois", "Indiana", "Ohio", "Michigan","Pennsylvania","New York",
                                    "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                    "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30", lwd=1)
  points(colloc$nhd_long, colloc$nhd_lat, col=c("grey70", "black")[colloc$sig], bg=get.col.bins(colloc$slopemean), pch=c(21), cex=1.3)
  
  points(x = seq(from = -83, to =-71.5, by = (2.5)), y= rep(50,5), pch = 22, cex = 8, bg = colors, col="grey30")
  text(x = seq(from = -83, to = -71.5, by = (2.5)), y = rep(49,5), labels=c("<-1%", "-1-0%", "0-1.1%", "1.1-2.5%", ">2.5%"), cex = 1.2)
  text(-97, 49.7, "b) Color % change per year", cex=2, pos=4)
  
  dev.off()
  


