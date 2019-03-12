#violin plot of slopes from model for 5 response variables - DOC, col, TP, TN, NO3

library(vioplot)

doc<-readRDS(file="Datasets/JAGS_DOC_july18.rds")
col<-readRDS(file="Datasets/JAGS_Color_mar19.rds")
tn<-readRDS(file="Datasets/JAGS_TN_july18.rds")
tp<-readRDS(file="Datasets/JAGS_TP_july18.rds")
no3<-readRDS(file="Datasets/JAGS_NO3_july18.rds")
chl<-readRDS(file="Datasets/JAGS_chla_july18.rds")


x1 <- doc$slopemean*100
x2 <- col$slopemean*100
x3 <- tp$slopemean*100
x4 <- tn$slopemean*100
x5 <- no3$slopemean*100
x6 <- chl$slopemean*100

png("Figures/Fig2_SlopesViolin.png", width=6, height=5, units='in', res=300)
par(mar=c(2,3,1,1))
vioplot(x1, x2, x3, x4, x5,x6, names=c(expression("DOC"[""]), expression("Color"[""]), expression("TP"[""]), expression("TN"[""]), expression("NO"[3]), expression("Chla"[""])), 
        col="lightgrey", ylim=c(-45, 40))
text(1, 14, "73%")
text(1, -9, "27%")
text(2, 15, "62%")
text(2, -14.5, "38%")
text(3, 35, "53%")
text(3, -14, "47%")
text(4, 10, "46%")
text(4, -15, "54%")
text(5, 37, "42%")
text(5, -42, "58%")
text(6, 14, "44%")
text(6, -15, "56%")
mtext("% Change Per Year", side=2, line=2)
abline(h=0)
dev.off()



##try out some plots for nutrient vs carbon vars

doc.tn<-na.omit(merge(doc, tn, by="lagoslakeid"))
plot(slopemean.y~slopemean.x, data=doc.tn, xlab="DOC % chg", ylab="TN % chg", pch=16)

doc.tp<-na.omit(merge(doc, tp, by="lagoslakeid"))
plot(slopemean.y~slopemean.x, data=doc.tp, xlab="DOC % chg", ylab="TP % chg", pch=16)

doc.no3<-na.omit(merge(doc, no3, by="lagoslakeid"))
plot(slopemean.y~slopemean.x, data=doc.no3, xlab="DOC % chg", ylab="NO3 % chg", pch=16)

doc.chl<-na.omit(merge(doc, chl, by="lagoslakeid"))
plot(slopemean.y~slopemean.x, data=doc.chl, xlab="DOC % chg", ylab="chla % chg", pch=16)

col.tn<-na.omit(merge(col, tn, by="lagoslakeid"))
plot(slopemean.y~slopemean.x, data=col.tn, xlab="Color % chg", ylab="TN % chg", pch=16)

col.tp<-na.omit(merge(col, tp, by="lagoslakeid"))
plot(slopemean.y~slopemean.x, data=col.tp, xlab="Color % chg", ylab="TP % chg", pch=16)

col.no3<-na.omit(merge(col, no3, by="lagoslakeid"))
plot(slopemean.y~slopemean.x, data=col.no3, xlab="Color % chg", ylab="NO3 % chg", pch=16)

col.chl<-na.omit(merge(col, chl, by="lagoslakeid"))
plot(slopemean.y~slopemean.x, data=col.chl, xlab="Color % chg", ylab="chla % chg", pch=16)


mod.doctn<-lm(slopemean.x~slopemean.y, data=doc.tn)
summary(mod.doctn)

mod.doctp<-lm(slopemean.x~slopemean.y, data=doc.tp)
summary(mod.doctp)

mod.docno3<-lm(slopemean.x~slopemean.y, data=doc.no3)
summary(mod.docno3)

mod.docchl<-lm(slopemean.x~slopemean.y, data=doc.chl)
summary(mod.docchl)

mod.coltn<-lm(slopemean.x~slopemean.y, data=col.tn)
summary(mod.coltn)

mod.coltp<-lm(slopemean.x~slopemean.y, data=col.tp)
summary(mod.coltp)

mod.colno3<-lm(slopemean.x~slopemean.y, data=col.no3)
summary(mod.colno3)

mod.colchl<-lm(slopemean.x~slopemean.y, data=col.chl)
summary(mod.colchl)


#make 8 panel for supplement - doc vs all variables, color vs all variables

png("Figures/Supplementary/S8_DOCColNutTrends.png", width=4, height=8, units='in', res=300)
par(mfrow=c(4,2), oma=c(4,4,1,1), mar=c(0,0,0,0))
plot(slopemean.y~slopemean.x, data=doc.tp, xlab="", ylab="", xaxt='n', yaxt='n', pch=1, xlim=c(-.05, .12), ylim=c(-.06, .12))
text(.12, .11, "n=236", pos=2, cex=1.2, col="darkred")
axis(2, at=c(-.05, 0, .05, .1), cex.axis=.85)
mtext(expression("TP "[]*"% change per year"), side=2, line=2, cex=.8)
plot(slopemean.y~slopemean.x, data=col.tp, xlab="", ylab="", xaxt='n', yaxt='n', pch=1, xlim=c(-.12, .12), ylim=c(-.06, .12))
text(.12, .11, "n=382", pos=2, cex=1.2, col="darkred")

plot(slopemean.y~slopemean.x, data=doc.tn, xlab="", ylab="", xaxt='n', yaxt='n', pch=1, xlim=c(-.05, .12), ylim=c(-.11, .07))
axis(2, at=c(-.1, -.05, 0, .05), cex.axis=.85)
mtext(expression("TN "[]*"% change per year"), side=2, line=2, cex=.8)
text(.12, .06, "n=35", pos=2, cex=1.2, col="darkred")
plot(slopemean.y~slopemean.x, data=col.tn, xlab="", ylab="", xaxt='n', yaxt='n', pch=1, xlim=c(-.12, .12), ylim=c(-.11, .07))
text(.12, .06, "n=99", pos=2, cex=1.2, col="darkred")

plot(slopemean.y~slopemean.x, data=doc.no3, xlab="", ylab="", xaxt='n',yaxt='n', pch=1, xlim=c(-.05, .12), ylim=c(-.4, .4))
axis(2, at=c(-.3, -.15, 0, .15, .3), cex.axis=.85)
mtext(expression("NO"[3]*"% change per year"), side=2, line=2, cex=.8)
text(.12, .37, "n=278", pos=2, cex=1.2, col="darkred")
plot(slopemean.y~slopemean.x, data=col.no3, xlab="", ylab="", xaxt='n', yaxt='n', pch=1, xlim=c(-.12, .12), ylim=c(-.4, .4))
text(.12, .37, "n=459", pos=2, cex=1.2, col="darkred")

plot(slopemean.y~slopemean.x, data=doc.chl, xlab="", ylab="", xaxt='n',yaxt='n', pch=1, xlim=c(-.05, .12), ylim=c(-.12, .12))
axis(2, at=c(-.1, -.05, 0, .05, .1), cex.axis=.85)
axis(1, at=c(-.05, 0, .05, .1), cex.axis=.85)
mtext(expression("Chlorophyll "[]*"% change per year"), side=2, line=2, cex=.8)
mtext("DOC % change per year", side=1, line=2, cex=.8)
text(.12, .11, "n=136", pos=2, cex=1.2, col="darkred")
plot(slopemean.y~slopemean.x, data=col.chl, xlab="", ylab="", xaxt='n', yaxt='n', pch=1, xlim=c(-.12, .12), ylim=c(-.12, .12))
axis(1, at=c(-.1, -.05, 0, .05, .1), cex.axis=.85)
mtext("Color % Change Per Year", side=1, line=2, cex=.8)
text(.12, .11, "n=257", pos=2, cex=1.2, col="darkred")

dev.off()