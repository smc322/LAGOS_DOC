#need to run ty doc jags model code - want to get slope, slope prob, whether slope is pos or neg. need to summarize 15,000 its from jags to get slope, others are already named in ty code. bind with lagoslakeid in same order as input


slopemean= apply(slopes, 2, mean)

slopesum = data.frame(cbind(slopeProbs, slopeSign, slopemean))

input<-readRDS('Datasets/DOCdata.rds')
lagoslakeid<-unique(input$lagoslakeid)
dataforanalysis<-data.frame(cbind(lagoslakeid, slopesum))

saveRDS(dataforanalysis, file="Datasets/JAGS_DOC.rds")
