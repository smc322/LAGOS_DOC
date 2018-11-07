# rm(list=ls())
library(data.table)
library(lme4)
library(jagsUI)
library(MCMCpack)
library(arm)

library(car)
library(ggmcmc)
library(gridExtra)
library(ggthemes)
library(coda)

dat <- readRDS('../Datasets/color_july18.rds')
#realized some lakes have negative color vals (perhaps from color a conversion - oops. toss those 60 lakes for now to log and we can figure it out if we can add them in later)
negs<-dat[dat$ly.med<0,]
dat<-dat[!dat$lagoslakeid %in% negs$lagoslakeid, ]

dat$ly.med=log(dat$ly.med+.01)
head(dat)
dim(dat)
length(unique(dat$lagoslakeid))



#### Read in Covariate data
covs <- readRDS('../Datasets/ColorCovars_Nov18.rds')
head(covs)
dim(covs)

covs <- data.table(covs)

covs[, lagoslakeid:=as.factor(lagoslakeid)]

# Select covariates for lakes in dat
cov_subset <- covs[covs$lagoslakeid %in% dat$lagoslakeid,]
dim(cov_subset)
length(unique(cov_subset$lagoslakeid))
head(cov_subset)
summary(cov_subset)

# Remove lakes with missing covariate data
cov_subset <- cov_subset[!is.na(urban)]
summary(cov_subset)
cov_subset <- cov_subset[!is.na(maxdepth)]
cov_subset <- cov_subset[!is.na(so4changepct)]
cov_subset <- cov_subset[!is.na(lakemedp)]
summary(cov_subset)
dim(cov_subset)

# Remove lakes from dat that have missing covariate data
dat <- dat[dat$lagoslakeid %in% cov_subset$lagoslakeid,]
length(unique(dat$lagoslakeid))
# Sort both data sets by lakd id
dat <- dat[order(dat$lagoslakeid),] 
cov_subset <- cov_subset[order(cov_subset$lagoslakeid),] 

# Transform few covariates
cov_subset[, maxdepth:=log(maxdepth)]
cov_subset[, lake_area_ha:=log(lake_area_ha)]
cov_subset[, wetlands:=logit(wetlands)]
cov_subset[, agri:=logit(agri)]
cov_subset[, urban:=logit(urban)]
cov_subset[, forest:=logit(forest)]
cov_subset[, lakemedp:=log(lakemedp+0.1)]

# Standardize covariates
changeCols <- colnames(cov_subset)[c(2,4:8,11:16,18,20)]
cov_subset[,(changeCols):= lapply(.SD, scale), .SDcols = changeCols]

# Covariate matrix for modeling slopes
x_mat <- as.matrix(cov_subset[,c(2,4:8,11:16,18,20)])
colnames(x_mat)
cor(x_mat)
dim(x_mat)
################################################################
########## BUGS CODE ############################################
#################################################################
sink("Model.txt")
cat("
    model {
    for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    
    y.hat[i] <- alpha[group[i]] + beta[group[i]] * x[i]  
    e.y[i] <- y[i] - y.hat[i]
    
    }
    
    tau.y <- pow(sigma.y, -2)
    sigma.y ~ dunif (0, 10)
    
    # Level-2 of the model
    for(j in 1:J){
    alpha[j] <- BB[j,1]
    beta[j] <- BB[j,2]
    
    BB[j,1:K] ~ dmnorm (BB.hat[j,], Tau.B[,])
    BB.hat[j,1] <- mu.a 
    e.a[j] <- alpha[j] - BB.hat[j,1]
    BB.hat[j,2] <- mu.b + b[1] * z[j, 1] + b[2] * z[j, 2] + b[3] * z[j, 3] +  b[4] * z[j, 4] +  b[5] * z[j, 5] +  b[6] * z[j, 6] +
                          b[7] * z[j, 7] + b[8] * z[j, 8] + b[9] * z[j, 9] +  b[10] * z[j, 10] +  b[11] * z[j, 11] +  b[12] * z[j, 12]  +
                          b[13] * z[j, 13] +  b[14] * z[j, 14]  
    e.b[j] <- beta[j] - BB.hat[j,2]

    
    }
    
   

# Bayesian LASSO -  a Laplace (double exponential) prior
    for(k in 1:ncovs){
      b[k] ~ ddexp(0, lambda1)
    }

# Hyper-prior on lambda
    lambda1 ~ dexp(10)
    mu.a ~ dnorm(0,0.0001)
    mu.b ~ dnorm(0,0.0001)
    
    
    # Model variance-covariance
    Tau.B[1:K,1:K] ~ dwish(W[,], df)
    df <- K+1
    Sigma.B[1:K,1:K] <- inverse(Tau.B[,])
    for (k in 1:K){
    for (k.prime in 1:K){
    rho.B[k,k.prime] <- Sigma.B[k,k.prime]/
    sqrt(Sigma.B[k,k]*Sigma.B[k.prime,k.prime])
    }
    sigma.B[k] <- sqrt(Sigma.B[k,k])
    }
    
    }
    ",fill=TRUE)
sink()

# Number of sites
J <-length(unique(dat$lagoslakeid))


# Create identity matrix for Wishart dist'n
#!!!!!!!Number of parameters to estimate (K)
K <- 2

W <- diag(K)

# Make sure ID is numeric and goes from 1-number of fisj
dat$ID2 <- as.numeric(as.factor(as.numeric(dat$lagoslakeid)))

# load data
data <- list(y = dat$ly.med , x = dat$year1, group = dat$ID2, n = dim(dat)[1],
             J = J, W = W, K = K, ncovs = dim(x_mat)[2], z = x_mat)


# Initial values
inits <- function (){
  list (BB=array(c(rep(rnorm(1,0,1),J),rep(rnorm(1,0,1),J)), c(J,K)), 
        mu.a=rnorm(1,0,1),mu.b=rnorm(1,0,1),
        sigma.y=runif(1,0,10), 
        Tau.B=rwish(K+1,diag(K))	 )
}

# Parameters monitored
params1 <- c("BB","mu.a","mu.b", "sigma.y","sigma.B","rho.B","b","alpha","beta","e.a","e.b","e.y")


# MCMC settings
ni <- 10000
nt <- 1
nb <- 5000
nc <- 3


out1 <- jags(data, inits, params1, "Model.txt", n.chains = nc, 
             n.thin = nt, n.iter = ni, n.burnin = nb, parallel = T)

# Check model convergence
# out.mcmc <- as.mcmc(out1)
# S <- ggs(out.mcmc$samples)
# ggs_traceplot(S, family="mu.a")
# ggs_traceplot(S, family="mu.b")
# ggs_traceplot(S, family="^b")
# ggs_traceplot(S, family="sigma.B")
# ggs_traceplot(S, family="sigma.y")
# ggs_traceplot(S,family="BB\\[1,.\\]")

# Summarize posteriors
print(out1, dig = 3)

y = dat$ly.med
# data level summaries
rsquared.y <- 1 - mean (apply (out1$sims.list$e.y, 1, var))/ var (y)
# summaries for the intercept model
rsquared.a <- 1 - mean (apply (out1$sims.list$e.a, 1, var))/ mean (apply (out1$sims.list$alpha, 1, var))
# summaries for the slope model
rsquared.b <- 1 - mean (apply (out1$sims.list$e.b, 1, var))/ mean (apply (out1$sims.list$beta, 1, var))
print (round (c (rsquared.y, rsquared.a, rsquared.b), 2))




BugsOut <- out1$summary
write.csv(BugsOut, "BUGSutputSummary.csv", row.names = T)
# Export MCMC samples 
# mcmcOut <- out1$sims.list
# saveRDS(mcmcOut, file="jags_out.rds")
# read back in mcmcOut
# out <- readRDS("jags_out.rds")
# str(out)

###############################
## PLOT COVARIATE EFFECTS #####
# Select random slopes  
mean.beta <- out1$mean$BB[,2]

# Fake data to predict
fake1 <- matrix(NA, nrow=50, ncol=dim(x_mat)[2])
for(j in 1:dim(x_mat)[2]){
  fake1[,j] <- seq(min(x_mat[,j]), max(x_mat[,j]), length=50)
}


# Obtain  CIs and fitted lines
est.lineA <- array(NA, dim=c( out1$mcmc.info$n.samples, dim(fake1)[1], dim(fake1)[2])  ) #container for predicted values
dim(est.lineA)

for(i in 1:out1$mcmc.info$n.samples ){
    for(j in 1:dim(fake1)[1] ){
      for(k in 1:dim(fake1)[2]){
      est.lineA[i,j,k] <- out1$sims.list$mu.b[i] + out1$sims.list$b[i, k] * fake1[j, k] 
    }
  }
}


# CIs for fitted values
fit1 <- apply(est.lineA, c(2,3), mean )
upper.CIA <- apply(est.lineA, c(2,3), quantile, 0.975)
lower.CIA <- apply(est.lineA, c(2,3), quantile, 0.025)


## Grab 90% CIs for beta's
CRIs <- matrix(NA, nrow=2, ncol=length(mean.beta))
for(i in 1:length(mean.beta)) { 
  CRIs[,i] <- quantile(out1$sims.list$BB[,i,2],probs=c(0.025, 0.975) )
}

# Significant indicator for plotting
beta_sig <- apply(out1$sims.list$b,2,quantile, c(0.025,0.975))
beta_sig_indicator <- numeric()
for(j in 1:dim(x_mat)[2]){
beta_sig_indicator[j] <- as.numeric(beta_sig[1,j] * beta_sig[2,j] > 0)
}
color <- rep("black", dim(x_mat)[2])
color[beta_sig_indicator==1] <- "blue"

#################
# Create figure
pdf("col_cov_plot.pdf", height = 6, width = 6)
def.par <- par(no.readonly = TRUE)

nf <- layout(matrix(c(1:15),nrow=5,ncol=3,byrow=TRUE),  TRUE) 
# layout.show(nf)
par(oma=c(2,3.0,0,1),mai=c(0.10,0.1,0.05,0) )


size.labels = 1
size.text = 1
axissize <- 1
ylabnums <- seq(min(CRIs[1,]),max(CRIs[,2]),length=5)

x.label = 'Standardized covariate'
y.label = expression(paste('Lake-specific trend (',beta[j], ')'))

for(j in 1:dim(x_mat)[2]){
plot(x_mat[,j], mean.beta ,pch=16,axes=F, xlab='',ylab='',cex=0.8,type='n',
     ylim=c(min(CRIs[1,]), max(upper.CIA)) )

    axis(side=1,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01)
  
  if( j ==1 | j==4 | j==7 | j==10){
    axis(side=2,cex.axis=axissize , mgp=c(0,0.3,0),tck= -0.01, las=1,at=format(pretty(ylabnums), digits=2), 
         labels=format(pretty(ylabnums), digits=2))
  } else {
    axis(side=2,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01, labels=F)
  }	
  
polygon(x=c(fake1[,j],rev(fake1[,j])),y=c(lower.CIA[,j],rev(upper.CIA[,j])),
        col=adjustcolor('gray',0.5),border=NA)

points(x_mat[,j], mean.beta,pch=16,cex=0.8)

segments(x0=x_mat[,j], x1=x_mat[,j],
         y0=CRIs[1,], y1=CRIs[2,], col='black',lwd=1)


lines(fake1[,j],fit1[,j], lwd = 3, col="black", lty = 1)

mtext(x.label, line = 0.4, side = 1, cex = size.text, outer=T)
mtext(y.label, line = 1.3, side = 2, cex = size.text, outer=T)

text(quantile(x_mat[,j], 0.99),-0.2, colnames(x_mat)[j], col=color[j])

box()
} # end for loop
dev.off()

# END PLOT







# Calculate lake-specific probability of trend being in the
# direction of the posterior mean
# lake specific slopes
slopes <- out1$sims.list$BB[, ,2]
str(slopes)
# sign of slope
slopeSign <- out1$mean$BB[,2] > 0     
# calc
slopeProbs <- numeric()
for(i in 1:J){
  if(slopeSign[i] > 0){
    slopeProbs[i] <- mean(slopes[,i] > 0)
  } else {
    slopeProbs[i] <- mean(slopes[,i] < 0)
  }
}
# Trend probs for each lake
slopeProbs

#################################
### lake-specific panel plot
#################################
predX <- seq(min(dat$year1), max(dat$year1), length=50) # fake data to
predict

# Container for predicted values
est.lineB <- array(NA, c(out1$mcmc.info$n.samples,length(predX),J) )

# Put each groups MCMC draws for all 2 parameters in its own list
group.params <- list()
for(m in 1:J){
  group.params[[m]] <- out1$sims.list$BB[,m,]
}


for(k in 1:J){ # loop over groups (J)
  for(i in 1:out1$mcmc.info$n.samples){  
    for(t in 1:length(predX)){
      est.lineB[i,t,k] <- group.params[[k]][i,1] + group.params[[k]][i,2] *
        predX[t]
    }	  
  }
}
dim(est.lineB)

# Get posterior mean and CIs
groupMean <- apply(est.lineB,c(2,3),mean)
dim(groupMean)
upper.CIB <- apply(est.lineB,c(2,3),quantile, 0.975, na.rm=T)
lower.CIB <- apply(est.lineB,c(2,3),quantile, 0.025, na.rm=T)

# Generate fake y-axis for creating plot
z <- seq(min(dat$ly.med,na.rm=T),max(dat$ly.med,na.rm=T),length=50) 

################# PLOT ############################


pdf("lake_panel.pdf", height = 12, width = 12)
def.par <- par(no.readonly = TRUE)

size.labels = 1
size.text = 1
axissize <- 1
x.label = 'Year'
y.label = 'Response'

nf <- layout(matrix(c(1:80),nrow=10,ncol=8,byrow=TRUE),  TRUE) 
# layout.show(nf)
par(mar=c(0.0,0.1,0.1,0.1),oma=c(3,4,0,1),mai=c(0.0,0.05,0.05,0) )

# Group-specific plots

for(i in 1:80){
  
  plot(predX,z, ylim=c(min(dat$ly.med,na.rm=T),max(dat$ly.med,na.rm=T)),
       xlim=c(min(dat$year1),max(dat$year1)), axes=F, ylab='', xlab='',
       type='n')
  
  points(dat$year1[dat$ID2==i], dat$ly.med[dat$ID2==i], cex=0.8,
         pch=16,col='black') 
  
  
  if( i <=72){
    axis(side=1,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01, labels=F ) 
  } else {
    axis(side=1,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01)
  }	
  
  if( i ==1 | i==9 | i==17| i==25| i==33| i==41| i==49| i==57| i==65| i==73 ){
    axis(side=2,cex.axis=axissize , mgp=c(0,0.3,0),tck= -0.01, las=1)
  } else {
    axis(side=2,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01, labels=F)
  }	
  
  # Add CIs
  polygon(x=c(predX,rev(predX)),y=c(lower.CIB[,i],rev(upper.CIB[,i])),
          col=adjustcolor('gray',0.2))
  
  # Add posterior means
  lines(predX, groupMean[,i],lwd=1, col='black',lty=1)
  
  # text(0.1,1000,i,cex=0.8)
  box()
  
}

mtext(y.label, line = 2.3, side = 2, cex = size.text,outer=T)
mtext(x.label, line = 1.5, side = 1, cex = size.text, outer=T)


par(def.par)
dev.off()
