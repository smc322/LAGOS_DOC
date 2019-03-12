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
text(2, 15, "60%")
text(2, -14.5, "40%")
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