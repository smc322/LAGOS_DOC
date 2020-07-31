##cursory sulfur change 1985-2010 vs doc/col change

so4dep<-lagos$hu12.chag[,c("hu12_zoneid", "hu12_dep_so4_1985_mean", "hu12_dep_so4_2010_mean")]
hu12.llid<-lagos$locus[,c("lagoslakeid", "hu12_zoneid")]
so4dep.llid<-merge(so4dep, hu12.llid, by="hu12_zoneid")
so4dep.llid$so4depchange<-so4dep.llid$hu12_dep_so4_2010_mean-so4dep.llid$hu12_dep_so4_1985_mean
so4dep.llid$so4changepct<-so4dep.llid$so4depchange/so4dep.llid$hu12_dep_so4_1985_mean*100

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

png("Figures/Fig3_DOCColTrendsSO4depPCT.png", width=6, height=8, units='in', res=300)
par(mfrow=c(2,1), oma=c(4,4,1,1), mar=c(0,0,0,0))
plot(doc.dep$SM.doc*100~doc.dep$so4changepct, cex=doc.dep$doc.nobs/20, pch=21, ylab="", xlab="", yaxt='n', xaxt='n', ylim=c(-11, 11), xlim=c(-80, -30), bg=rgb(169,169,169,150, max=255), col="black")
axis(2, at=c(-10, 0, 10))
mtext("DOC % Change Per Year", side=2, line=2)
legend(600, -4, legend=c("5 years", "10 years", "15 years", "20 years"), pch=21, col="black", pt.bg=rgb(169,169,169,150, max=255), pt.cex=c(5/20, 10/20, 15/20, 20/20), bty='n')
abline(h=0)


plot(color.dep$SM.col*100~color.dep$so4changepct, cex=color.dep$col.nobs/20, pch=21, ylab="", xlab="", yaxt='n', xaxt='n', ylim=c(-11, 11), xlim=c(-80, -30), bg=rgb(169,169,169,150, max=255), col="black")
axis(2, at=c(-10, 0, 10))
mtext("Color % Change Per Year", side=2, line=2)
axis(1, at=c(-80, -65, -50, -35))
mtext("% Change in SO4 Dep 1985-2010", side=1, line=2)
abline(h=0)
dev.off()

