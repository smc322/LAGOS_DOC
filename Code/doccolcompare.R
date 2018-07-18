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

names(doc)<-c("lagoslakeid", "SP.doc", "SS.doc", "SM.doc")

names(col)<-c("lagoslakeid", "SP.col", "SS.col", "SM.col")

names(tp)<-c("lagoslakeid", "SP.tp", "SS.tp", "SM.tp")


doc.col<-merge(doc, col, by="lagoslakeid", all.x=T, all.y=T)
doc.col.complete<-na.omit(doc.col)
completec<-merge(doc.col.complete, cordsids, by="lagoslakeid", all.x=T, all.y=F)

##run medianstocompare.R to get medians to merge into this for comparing median lake concentrations and number of observations

ctrend1=merge(completec, data.doc.med, by="lagoslakeid")
ctrend2=merge(ctrend1, data.col.med, by="lagoslakeid")
ctrend.tp=merge(doc, data.tp.med, by="lagoslakeid")
ctrend.chl=merge(ctrend2, data.chla.med, by="lagoslakeid")

brgr<-merge(col, tp, by="lagoslakeid")


doc.both<-merge(doc, data.doc.med, by="lagoslakeid")
doc.col<-merge(doc.both, data.col.med, by="lagoslakeid")

col.both<-merge(col, data.col.med, by="lagoslakeid")

png("Figures/DOCtrendColplot.png", width=6, height=4, units='in', res=300)
par(mar=c(4,4,1,1))
plot(doc.col$SM.doc*100~doc.col$col.med, cex=doc.col$doc.nobs/20, pch=21, ylab="", xlab="", yaxt='n', xaxt='n', ylim=c(-5, 11), xlim=c(0, 205), bg=rgb(169,169,169,150, max=255), col="black")
axis(2, at=c(-5, 0, 5, 10), labels=c("-5", "0", "5", "10"))
mtext("DOC % Change Per Year", side=2, line=2)
axis(1, at=c(0, 50, 100, 150, 200))
mtext("Median Color (PCU)", side=1, line=2)
legend(165, 12, legend=c("5 years", "10 years", "15 years", "20 years"), pch=21, col="black", pt.bg=rgb(169,169,169,150, max=255), pt.cex=c(5/20, 10/20, 15/20, 20/20), bty='n')
abline(h=0)
 dev.off()
 
 
 png("Figures/ColtrendColplot.png", width=6, height=4, units='in', res=300)
 par(mar=c(4,4,1,1))
 plot(col.both$SM.col*100~col.both$col.med, cex=col.both$col.nobs/20, pch=21, ylab="", xlab="", yaxt='n', xaxt='n', ylim=c(-20, 11), xlim=c(0, 215), bg=rgb(169,169,169,150, max=255), col="black")
 axis(2, at=c(-20, -10, 0, 10), labels=c("-20", "-10", "0", "10"))
 mtext("Color % Change Per Year", side=2, line=2)
 axis(1, at=c(0, 50, 100, 150, 200))
 mtext("Median Color (PCU)", side=1, line=2)
 legend(165, -10, legend=c("5 years", "10 years", "15 years", "20 years"), pch=21, col="black", pt.bg=rgb(169,169,169,150, max=255), pt.cex=c(5/20, 10/20, 15/20, 20/20), bty='n')
 abline(h=0)
 dev.off()


 png("Figures/DOCColTrendsColMed.png", width=6, height=8, units='in', res=300)
 par(mfrow=c(2,1), oma=c(4,4,1,1), mar=c(0,0,0,0))
 plot(doc.col$SM.doc*100~doc.col$col.med, cex=doc.col$doc.nobs/20, pch=21, ylab="", xlab="", yaxt='n', xaxt='n', ylim=c(-11, 11), xlim=c(0, 205), bg=rgb(169,169,169,150, max=255), col="black")
 axis(2, at=c(-10, 0, 10))
 mtext("DOC % Change Per Year", side=2, line=2)
 legend(165, 12, legend=c("5 years", "10 years", "15 years", "20 years"), pch=21, col="black", pt.bg=rgb(169,169,169,150, max=255), pt.cex=c(5/20, 10/20, 15/20, 20/20), bty='n')
 abline(h=0)
 
 
 plot(col.both$SM.col*100~col.both$col.med, cex=col.both$col.nobs/20, pch=21, ylab="", xlab="", yaxt='n', xaxt='n', ylim=c(-11, 11), xlim=c(0, 205), bg=rgb(169,169,169,150, max=255), col="black")
 axis(2, at=c(-10, 0, 10))
 mtext("Color % Change Per Year", side=2, line=2)
 axis(1, at=c(0, 50, 100, 150, 200))
 mtext("Median Color (PCU)", side=1, line=2)
 abline(h=0)
 dev.off()

 
 doc.tp<-merge(doc.both, data.tp.med, by="lagoslakeid")
 
 col.tp<-merge(col.both, data.tp.med, by="lagoslakeid")
 
 png("Figures/DOCColTrendsTPMed.png", width=6, height=8, units='in', res=300)
 par(mfrow=c(2,1), oma=c(4,4,1,1), mar=c(0,0,0,0))
 plot(doc.tp$SM.doc*100~doc.tp$tp.med, cex=doc.tp$doc.nobs/20, pch=21, ylab="", xlab="", yaxt='n', xaxt='n', ylim=c(-11, 11), xlim=c(0, 105), bg=rgb(169,169,169,150, max=255), col="black")
 axis(2, at=c(-10, 0, 10))
 mtext("DOC % Change Per Year", side=2, line=2)
 legend(165, 12, legend=c("5 years", "10 years", "15 years", "20 years"), pch=21, col="black", pt.bg=rgb(169,169,169,150, max=255), pt.cex=c(5/20, 10/20, 15/20, 20/20), bty='n')
 abline(h=0)
 
 
 plot(col.tp$SM.col*100~col.tp$tp.med, cex=col.tp$col.nobs/20, pch=21, ylab="", xlab="", yaxt='n', xaxt='n', ylim=c(-11, 11), xlim=c(0, 105), bg=rgb(169,169,169,150, max=255), col="black")
 axis(2, at=c(-10, 0, 10))
 mtext("Color % Change Per Year", side=2, line=2)
 axis(1, at=c(0, 25, 50, 75, 100))
 mtext("Median TP (ug/L)", side=1, line=2)
 abline(h=0)
 dev.off()
 
 precip<-lagos$hu12.chag[,c("hu12_zoneid", "hu12_prism_ppt_30yr_normal_800mm2_annual_mean")]
 hu12.llid<-lagos$locus[,c("lagoslakeid", "hu12_zoneid")]
 precip.llid<-merge(precip, hu12.llid, by="hu12_zoneid")
 
 doc.precip<-na.omit(merge(doc.both, precip.llid, by="lagoslakeid"))
 color.precip<-na.omit(merge(col.both, precip.llid, by="lagoslakeid"))
 
 png("Figures/DOCColTrendsPPT.png", width=6, height=8, units='in', res=300)
 par(mfrow=c(2,1), oma=c(4,4,1,1), mar=c(0,0,0,0))
 plot(doc.precip$SM.doc*100~doc.precip$hu12_prism_ppt_30yr_normal_800mm2_annual_mean, cex=doc.col$doc.nobs/20, pch=21, ylab="", xlab="", yaxt='n', xaxt='n', ylim=c(-11, 11), xlim=c(550, 1550), bg=rgb(169,169,169,150, max=255), col="black")
 axis(2, at=c(-10, 0, 10))
 mtext("DOC % Change Per Year", side=2, line=2)
 legend(600, -4, legend=c("5 years", "10 years", "15 years", "20 years"), pch=21, col="black", pt.bg=rgb(169,169,169,150, max=255), pt.cex=c(5/20, 10/20, 15/20, 20/20), bty='n')
 abline(h=0)
 
 
 plot(color.precip$SM.col*100~color.precip$hu12_prism_ppt_30yr_normal_800mm2_annual_mean, cex=color.precip$col.nobs/20, pch=21, ylab="", xlab="", yaxt='n', xaxt='n', ylim=c(-11, 11), xlim=c(550, 1550), bg=rgb(169,169,169,150, max=255), col="black")
 axis(2, at=c(-10, 0, 10))
 mtext("Color % Change Per Year", side=2, line=2)
 axis(1, at=c(600, 900, 1200, 1500))
 mtext("30 Year Normal PPT", side=1, line=2)
 abline(h=0)
 dev.off()
 
 ##cursory sulfur change 1985-2010 vs doc/col change
 
 so4dep<-lagos$hu12.chag[,c("hu12_zoneid", "hu12_dep_so4_1985_mean", "hu12_dep_so4_2010_mean")]
 hu12.llid<-lagos$locus[,c("lagoslakeid", "hu12_zoneid")]
 so4dep.llid<-merge(so4dep, hu12.llid, by="hu12_zoneid")
 so4dep.llid$so4depchange<-so4dep.llid$hu12_dep_so4_2010_mean-so4dep.llid$hu12_dep_so4_1985_mean
 
 doc.dep<-na.omit(merge(doc.both, so4dep.llid, by="lagoslakeid"))
 color.dep<-na.omit(merge(col.both, so4dep.llid, by="lagoslakeid"))
 
 png("Figures/DOCColTrendsSO4dep.png", width=6, height=8, units='in', res=300)
 par(mfrow=c(2,1), oma=c(4,4,1,1), mar=c(0,0,0,0))
 plot(doc.dep$SM.doc*100~doc.dep$so4depchange, cex=doc.dep$doc.nobs/20, pch=21, ylab="", xlab="", yaxt='n', xaxt='n', ylim=c(-11, 11), xlim=c(-31, 1), bg=rgb(169,169,169,150, max=255), col="black")
 axis(2, at=c(-10, 0, 10))
 mtext("DOC % Change Per Year", side=2, line=2)
 legend(600, -4, legend=c("5 years", "10 years", "15 years", "20 years"), pch=21, col="black", pt.bg=rgb(169,169,169,150, max=255), pt.cex=c(5/20, 10/20, 15/20, 20/20), bty='n')
 abline(h=0)
 
 
 plot(color.dep$SM.col*100~color.dep$so4depchange, cex=color.dep$col.nobs/20, pch=21, ylab="", xlab="", yaxt='n', xaxt='n', ylim=c(-11, 11), xlim=c(-31, 1), bg=rgb(169,169,169,150, max=255), col="black")
 axis(2, at=c(-10, 0, 10))
 mtext("Color % Change Per Year", side=2, line=2)
 axis(1, at=c(-30, -20, -10, 0))
 mtext("Change in SO4 Dep 1985-2010", side=1, line=2)
 abline(h=0)
 dev.off()
 
 