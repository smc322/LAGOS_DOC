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

both.nona<-na.omit(both)

allnegs<-both.nona[both.nona$slopemean.x<0 & both.nona$slopemean.y<0,]
allpos<-both.nona[both.nona$slopemean.x>0 & both.nona$slopemean.y>0,]

improbable<-both.nona[both.nona$slopeProbs.x<0.9 & both.nona$slopeProbs.y<0.9,]
docincreaseonly<-both.nona[both.nona$slopeProbs.x>0.9 & both.nona$slopeProbs.y<0.9 & both.nona$slopemean.x>0 ,]
docdecreaseonly<-both.nona[both.nona$slopeProbs.x>0.9 & both.nona$slopeProbs.y<0.9 & both.nona$slopemean.x<0 ,]
colincreaseonly<-both.nona[both.nona$slopeProbs.y>0.9 & both.nona$slopeProbs.x<0.9 & both.nona$slopemean.y>0 ,]
coldecreaseonly<-both.nona[both.nona$slopeProbs.y>0.9 & both.nona$slopeProbs.x<0.9 & both.nona$slopemean.y<0 ,]
bothincrease<-both.nona[both.nona$slopeProbs.x>0.9 & both.nona$slopeProbs.y>0.9 & both.nona$slopemean.x>0 & both.nona$slopemean.y>0,]
bothdecrease<-both.nona[both.nona$slopeProbs.x>0.9 & both.nona$slopeProbs.y>0.9 & both.nona$slopemean.x<0 & both.nona$slopemean.y<0,]
docinccoldec<-both.nona[both.nona$slopeProbs.x>0.9 & both.nona$slopeProbs.y>0.9 & both.nona$slopemean.x>0 & both.nona$slopemean.y<0,]
colincdocdec<-both.nona[both.nona$slopeProbs.x>0.9 & both.nona$slopeProbs.y>0.9 & both.nona$slopemean.x<0 & both.nona$slopemean.y>0,]
mixedmessages<-rbind(docinccoldec, colincdocdec)


#make plot for SI to show the lack of relationship between DOC and col trends
png("Figures/Supplementary/S7_DOCColTrends_greyimprobs.png", width=5, height=5, units='in', res=300)
par(mar=c(4,4,1,1))
plot(slopemean.x~slopemean.y, data=both, xlab="", ylab="" , col="white", pch=1, xlim=c(-.12, .12), ylim=c(-.02, .12))

mtext("Color % change per year", side =1, line=2.5)
mtext("DOC % change per year", side=2, line=2.5)

abline(v=0, col="darkgrey", lwd=.75, lty=3)
abline(h=0, col="darkgrey", lwd=.75, lty=3)
points(slopemean.x~slopemean.y, data=improbable, pch=1, col="grey70")
points(slopemean.x~slopemean.y, data=docincreaseonly, pch=24, col="black", bg=rgb(102, 194, 165, 180, max=255))
points(slopemean.x~slopemean.y, data=docdecreaseonly, pch=25, col="black", bg=rgb(102, 194, 165, 180, max=255))
points(slopemean.x~slopemean.y, data=colincreaseonly, pch=24, col="black", bg=rgb(252, 141, 98, 180, max=255))
points(slopemean.x~slopemean.y, data=coldecreaseonly, pch=25, col="black", bg=rgb(252, 141, 98, 180, max=255))
points(slopemean.x~slopemean.y, data=bothincrease, pch=24, col="black", bg=rgb(141, 160, 203, 180, max=255))
points(slopemean.x~slopemean.y, data=bothdecrease, pch=25, col="black", bg=rgb(141, 160, 203, 180, max=255))
points(slopemean.x~slopemean.y, data=mixedmessages, pch=23, col="black", bg=rgb(141, 160, 203, 180, max=255))
abline(a=.011, b=.126, lty=2)

legend(-.13, .127, c("Both increasing", "Both decreasing", "Opposite directions", "DOC increasing", "DOC decreasing", "Color increasing", "Color decreasing", "No Change"), pch=c(24, 25, 23, 24, 25, 24, 25, 1), col=c("black", "black", "black", "black", "black", "black", "black", "grey70"), pt.bg=c(bg=rgb(141, 160, 203, 180, max=255), bg=rgb(141, 160, 203, 180, max=255), bg=rgb(141, 160, 203, 180, max=255), rgb(102, 194, 165, 180, max=255), rgb(102, 194, 165, 180, max=255), rgb(252, 141, 98, 180, max=255), rgb(252, 141, 98, 180, max=255), "grey70"), bty='n', cex=0.8)
dev.off()


#make 3 panel map with significance noted

insigloc<-merge(improbable, cordsids, by="lagoslakeid", all.x=T, all.y=F)
docuploc<-merge(docincreaseonly, cordsids, by="lagoslakeid", all.x=T, all.y=F)
docdownloc<-merge(docdecreaseonly, cordsids, by="lagoslakeid", all.x=T, all.y=F)
coluploc<-merge(colincreaseonly, cordsids, by="lagoslakeid", all.x=T, all.y=F)
coldownloc<-merge(coldecreaseonly, cordsids, by="lagoslakeid", all.x=T, all.y=F)
bothuploc<-merge(bothincrease, cordsids, by="lagoslakeid", all.x=T, all.y=F)
bothdownloc<-merge(bothdecrease, cordsids, by="lagoslakeid", all.x=T, all.y=F)
mixeddownloc<-merge(mixedmessages, cordsids, by="lagoslakeid", all.x=T, all.y=F)

pdf("Figures/Fig1_DOCColProbmaps_slopes_3panel.pdf", width=12, height=18)
par(mfrow=c(3,1), oma=c(0,0,0,0), mar=c(0,0,0,0), xpd=NA)


map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois", "Indiana", "Ohio", "Michigan","Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30", lwd=1)
points(docloc$nhd_long, docloc$nhd_lat, col= c("grey70", "black")[docloc$sig], bg=get.col.bins(docloc$slopemean), pch = c(21), cex=1.5)
text(-97, 49.7, "a) DOC % change per year", cex=2, pos=4)


map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois", "Indiana", "Ohio", "Michigan","Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30", lwd=1)
points(colloc$nhd_long, colloc$nhd_lat, col=c("grey70", "black")[colloc$sig], bg=get.col.bins(colloc$slopemean), pch=c(21), cex=1.5)

points(x = seq(from = -83, to =-71.5, by = (2.5)), y= rep(50,5), pch = 22, cex = 8, bg = colors, col="grey30")
text(x = seq(from = -83, to = -71.5, by = (2.5)), y = rep(49,5), labels=c("<-1%", "-1-0%", "0-1.1%", "1.1-2.5%", ">2.5%"), cex = 1.2)
text(-97, 49.7, "b) Color % change per year", cex=2, pos=4)

map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois", "Indiana", "Ohio", "Michigan","Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30", lwd=1)
# points(insigloc$nhd_long, insigloc$nhd_lat, col="grey20", bg=c("grey70"), pch=c(21), cex=1.3)
# points(docuploc$nhd_long, docuploc$nhd_lat, col="grey20", bg=rgb(202, 0, 32, 180,max=255), pch=c(22), cex=1.3)
# points(docdownloc$nhd_long, docdownloc$nhd_lat, col="grey20", bg=rgb(5, 113, 176, 180,max=255), pch=c(22), cex=1.3)
# points(coluploc$nhd_long, coluploc$nhd_lat, col="grey20", bg=rgb(202, 0, 32, 180,max=255), pch=c(23), cex=1.3)
# points(coldownloc$nhd_long, coldownloc$nhd_lat, col="grey20", bg=rgb(5, 113, 176, 180,max=255), pch=c(23), cex=1.3)
points(bothuploc$nhd_long, bothuploc$nhd_lat, col="grey20", bg=rgb(202, 0, 32, 180,max=255), pch=c(21), cex=1.8)
points(bothdownloc$nhd_long, bothdownloc$nhd_lat, col="grey20", bg=rgb(5, 113, 176, 180,max=255), pch=c(21), cex=1.8)
points(mixeddownloc$nhd_long, mixeddownloc$nhd_lat, col="grey20", bg=rgb(.54, .41, .8, .705,max=1), pch=c(21), cex=1.8)


#points(x = seq(from = -83, to =-71.5, by = (2.5)), y= rep(50,5), pch = 22, cex = 8, bg = colors, col="grey30")
#text(x = seq(from = -83, to = -71.5, by = (2.5)), y = rep(49,5), labels=c("<-1%", "-1-0%", "0-1.1%", "1.1-2.5%", ">2.5%"), cex = 1.2)
text(-97, 49.7, "c) Color vs. DOC changes", cex=2, pos=4)

dev.off()

