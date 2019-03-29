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
col<-readRDS(file="Datasets/JAGS_Color_mar19.rds")
tn<-readRDS(file="Datasets/JAGS_TN_july18.rds")
tp<-readRDS(file="Datasets/JAGS_TP_july18.rds")
no3<-readRDS(file="Datasets/JAGS_NO3_july18.rds")


locus<-lagos$locus
cordsids<-unique(locus[,c("lagoslakeid", "nhd_lat", "nhd_long")])



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
  

##calculate percentages of pos and neg slopes that are >90% probable
  
col.pos<-col[col$slopeSign==1,]
col.pos.prob<-col.pos$slopeProbs
length(col.pos.prob[which(col.pos.prob>.9)])

col.neg<-col[col$slopeSign==0,]
col.neg.prob<-col.neg$slopeProbs
length(col.neg.prob[which(col.neg.prob>.9)])

doc.pos<-doc[doc$slopeSign==1,]
doc.pos.prob<-doc.pos$slopeProbs
length(doc.pos.prob[which(doc.pos.prob>.9)])

doc.neg<-doc[doc$slopeSign==0,]
doc.neg.prob<-doc.neg$slopeProbs
length(doc.neg.prob[which(doc.neg.prob>.9)])

#correlate trends for when both DOC and color have data

both<-merge(doc, col, by="lagoslakeid", all.x=T, all.y=T)
mod<-lm(slopemean.x~slopemean.y, data=both)
summary(mod)

#make plot for SI to show the lack of relationship between DOC and col trends
png("Figures/Supplementary/S7_DOCColTrends.png", width=5, height=5, units='in', res=300)
par(mar=c(4,4,1,1))
plot(slopemean.x~slopemean.y, data=both, xlab="", ylab="", pch=1, xlim=c(-.12, .12), ylim=c(-.02, .12))

mtext("Color % change per year", side =1, line=2.5)
mtext("DOC % change per year", side=2, line=2.5)

abline(v=0, col="darkgrey", lwd=.75, lty=3)
abline(h=0, col="darkgrey", lwd=.75, lty=3)
points(slopemean.x~slopemean.y, data=both, pch=1)
abline(a=.011, b=.126, lty=2)
dev.off()