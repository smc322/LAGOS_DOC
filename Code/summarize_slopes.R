doc<-readRDS(file="Datasets/JAGS_DOC_july18.rds")
col<-readRDS(file="Datasets/JAGS_Color_july18.rds")
tn<-readRDS(file="Datasets/JAGS_TN_july18.rds")
tp<-readRDS(file="Datasets/JAGS_TP_july18.rds")
no3<-readRDS(file="Datasets/JAGS_NO3_july18.rds")

doc$var="doc"
col$var="color"
tn$var="tn"
tp$var="tp"
no3$var="no3"

results.all=rbind(doc, col, tn, tp, no3)

results.all$pctchg<-results.all$slopemean*100

boxplot(pctchg~var, data=results.all)
axis(1, log="y")

#do some magic to get this on a log scale even though some values are neg
pos<-results.all[results.all$slopeSign==1,]
neg<-results.all[results.all$slopeSign==0,]
pos$logslope=log(1+pos$slopemean)
neg$logslope=log(1+abs(neg$slopemean))
neg$logslope=-1*neg$logslope
combologged<-rbind(pos,neg)
boxplot(logslope~var, data=combologged, yaxt='n')
