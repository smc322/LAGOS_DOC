#violin plot of slopes from model for 5 response variables - DOC, col, TP, TN, NO3

library(vioplot)

doc<-readRDS(file="Datasets/JAGS_DOC_july18.rds")
col<-readRDS(file="Datasets/JAGS_Color_july18.rds")
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

png("Figures/SlopesViolin.png", width=6, height=5, units='in', res=300)
vioplot(x1, x2, x3, x4, x5,x6, names=c("DOC", "Color", "TP", "TN", "NO3", "Chla"), 
        col="lightgrey")
mtext("% Change Per Year", side=2, line=2)
abline(h=0)
dev.off()