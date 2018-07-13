# rm(list=ls())
library(data.table)
library(lme4)
library(jagsUI)
library(MCMCpack)
library(arm)

dat <- readRDS('Datasets/doc_july18.rds')
dat$ly.med=log(dat$ly.med)
head(dat)
dim(dat)
length(unique(dat$lagoslakeid))


#################################################################
########## BUGS CODE ############################################
#################################################################
sink("Model.txt")
cat("
    model {
    for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    
    y.hat[i] <- alpha[group[i]] + beta[group[i]] * x[i]  
    
    }
    
    tau.y <- pow(sigma.y, -2)
    sigma.y ~ dunif (0, 10)
    
    # Level-2 of the model
    for(j in 1:J){
    alpha[j] <- BB[j,1]
    beta[j] <- BB[j,2]
    
    BB[j,1:K] ~ dmnorm (BB.hat[j,], Tau.B[,])
    BB.hat[j,1] <- mu.a 
    BB.hat[j,2] <- mu.b 
    
    }
    
    
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
             J = J, W = W, K = K )


# Initial values
inits <- function (){
  list (BB=array(c(rep(rnorm(1,0,1),J),rep(rnorm(1,0,1),J)), c(J,K)), 
        mu.a=rnorm(1,0,1),mu.b=rnorm(1,0,1),
        sigma.y=runif(1,0,10), 
        Tau.B=rwish(K+1,diag(K))	 )
}


# Parameters monitored
params1 <- c("BB","mu.a","mu.b", "sigma.y","sigma.B","rho.B")


# MCMC settings
ni <- 10000
nt <- 1
nb <- 5000
nc <- 3



out1 <- jags(data, inits, params1, "Model.txt", n.chains = nc, 
             n.thin = nt, n.iter = ni, n.burnin = nb, parallel = T)


# Summarize posteriors
print(out1, dig = 3)

BugsOut <- out1$summary
write.csv(BugsOut, "BUGSutputSummary.csv", row.names = T)
# Export MCMC samples 
# mcmcOut <- out1$sims.list
# saveRDS(mcmcOut, file="jags_out.rds")
# read back in mcmcOut
# out <- readRDS("jags_out.rds")
# str(out)


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
