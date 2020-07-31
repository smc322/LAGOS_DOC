## compare doc trends to ppt trends calculated with doc data

ppt<-readRDS("Datasets/JAGS_PPTCOL_july18.rds")
col<-readRDS("Datasets/JAGS_Color_july18.rds")

names(col)<-c("lagoslakeid", "SP.col", "SS.col", "SM.col")
names(ppt)<-c("lagoslakeid", "SP.ppt", "SS.ppt", "SM.ppt")

colppt<-merge(col, ppt, by="lagoslakeid")
colppt.med<-merge(colppt, data.col.med, by="lagoslakeid")

png("Figures/ColtrendPPTtrend.png", width=6, height=4, units='in', res=300)
par(mar=c(4,4,1,1))
plot(colppt.med$SM.col*100~colppt.med$SM.ppt, cex=colppt.med$col.nobs/20, pch=21, ylab="", xlab="", yaxt='n', xaxt='n', ylim=c(-21, 11), xlim=c(-.1, 1), bg=rgb(169,169,169,150, max=255), col="black")
axis(2, at=c(-20, -10, 0, 10))
mtext("Color % Change Per Year", side=2, line=2)
axis(1, at=c(0, .25, .5, .75))
mtext("PPT Trend Slope", side=1, line=2)
legend(165, 12, legend=c("5 years", "10 years", "15 years", "20 years"), pch=21, col="black", pt.bg=rgb(169,169,169,150, max=255), pt.cex=c(5/20, 10/20, 15/20, 20/20), bty='n')
abline(h=0, lty=3)
abline(v=0, lty=3)
dev.off()
