## compare doc trends to ppt trends calculated with doc data

ppt<-readRDS("Datasets/JAGS_PPTDOC_july18.rds")
doc<-readRDS("Datasets/JAGS_DOC_july18.rds")

names(doc)<-c("lagoslakeid", "SP.doc", "SS.doc", "SM.doc")
names(ppt)<-c("lagoslakeid", "SP.ppt", "SS.ppt", "SM.ppt")

docppt<-merge(doc, ppt, by="lagoslakeid")
docppt.med<-merge(docppt, data.doc.med, by="lagoslakeid")
docppt.med$pptpct=docppt.med$SM.ppt*100

png("Figures/DOCtrendPPTtrend.png", width=6, height=4, units='in', res=300)
par(mar=c(4,4,1,1))
plot(docppt.med$SM.doc*100~docppt.med$SM.ppt, cex=docppt.med$doc.nobs/20, pch=21, ylab="", xlab="", yaxt='n', xaxt='n', ylim=c(-5, 11), xlim=c(-.4, 1.65), bg=rgb(169,169,169,150, max=255), col="black")
axis(2, at=c(-5, 0, 5, 10), labels=c("-5", "0", "5", "10"))
mtext("DOC % Change Per Year", side=2, line=2)
axis(1, at=c(-.5, 0, .5, 1, 1.5))
mtext("PPT Trend Slope", side=1, line=2)
legend(165, 12, legend=c("5 years", "10 years", "15 years", "20 years"), pch=21, col="black", pt.bg=rgb(169,169,169,150, max=255), pt.cex=c(5/20, 10/20, 15/20, 20/20), bty='n')
abline(h=0, lty=3)
abline(v=0, lty=3)
dev.off()
