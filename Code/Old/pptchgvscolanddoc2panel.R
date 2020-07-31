#2panel col trend and doc trends vs ppt trend


pptdoc<-readRDS("Datasets/JAGS_PPTDOC_july18.rds")
pptcol<-readRDS("Datasets/JAGS_PPTCOL_july18.rds")
doc<-readRDS("Datasets/JAGS_DOC_july18.rds")
col<-readRDS("Datasets/JAGS_Color_july18.rds")

names(col)<-c("lagoslakeid", "SP.col", "SS.col", "SM.col")
names(doc)<-c("lagoslakeid", "SP.doc", "SS.doc", "SM.doc")
names(pptdoc)<-c("lagoslakeid", "SP.pptdoc", "SS.pptdoc", "SM.pptdoc")
names(pptcol)<-c("lagoslakeid", "SP.pptcol", "SS.pptcol", "SM.pptcol")


docppt<-merge(doc, pptdoc, by="lagoslakeid")
docppt$pptpct=docppt$SM.pptdoc*100
docppt.med<-merge(docppt, data.doc.med, by="lagoslakeid")


colppt<-merge(col, pptcol, by="lagoslakeid")
colppt$pptpct=colppt$SM.pptcol*100
colppt.med<-merge(colppt, data.col.med, by="lagoslakeid")


png("Figures/DOCColTrendsPPTchg.png", width=6, height=8, units='in', res=300)
par(mfrow=c(2,1), oma=c(4,4,1,1), mar=c(0,0,0,0))

plot(docppt.med$SM.doc*100~docppt.med$SM.pptdoc, cex=docppt.med$doc.nobs/20, pch=21, ylab="", xlab="", yaxt='n', xaxt='n', ylim=c(-5, 11), xlim=c(-.35, 1.50), bg=rgb(169,169,169,150, max=255), col="black")
axis(2, at=c(-5, 0, 5, 10), labels=c("-5", "0", "5", "10"))
mtext("DOC % Change Per Year", side=2, line=2)
abline(h=0, lty=3)
abline(v=0, lty=3)

plot(colppt.med$SM.col*100~colppt.med$SM.pptcol, cex=colppt.med$col.nobs/20, pch=21, ylab="", xlab="", yaxt='n', xaxt='n', ylim=c(-18, 9), xlim=c(-.35, 1.5), bg=rgb(169,169,169,150, max=255), col="black")
axis(2, at=c(-16,-8, 0, 8))
mtext("Color % Change Per Year", side=2, line=2)
axis(1, at=c(-.33, 0, .33, .66, 1.0, 1.33))
mtext("Precipitation % Change Per Year", side=1, line=2)
legend(1.1, -10, legend=c("5 years", "10 years", "15 years", "20 years"), pch=21, col="black", pt.bg=rgb(169,169,169,150, max=255), pt.cex=c(5/20, 10/20, 15/20, 20/20), bty='n')
abline(h=0, lty=3)
abline(v=0, lty=3)

dev.off()