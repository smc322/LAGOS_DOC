#need to run ty doc jags model code - want to get slope, slope prob, whether slope is pos or neg. need to summarize 15,000 its from jags to get slope, others are already named in ty code. bind with lagoslakeid in same order as input


slopemean= apply(slopes, 2, mean)

slopesum = data.frame(cbind(slopeProbs, slopeSign, slopemean))

input<-readRDS('Datasets/doc_july18.rds')
lagoslakeid<-unique(input$lagoslakeid)
dataforanalysis<-data.frame(cbind(lagoslakeid, slopesum))

saveRDS(dataforanalysis, file="Datasets/JAGS_DOC_july18.rds")


#same thing for t color

slopemean= apply(slopes, 2, mean)

slopesum = data.frame(cbind(slopeProbs, slopeSign, slopemean))

#input<-readRDS('Datasets/color_july18.rds')
input=dat
lagoslakeid<-unique(input$lagoslakeid)
dataforanalysis<-data.frame(cbind(lagoslakeid, slopesum))

saveRDS(dataforanalysis, file="Datasets/JAGS_Color_mar19.rds")

# and P (function, anyone?)

slopemean= apply(slopes, 2, mean)

slopesum = data.frame(cbind(slopeProbs, slopeSign, slopemean))

input<-readRDS('Datasets/tp_july18.rds')
lagoslakeid<-unique(input$lagoslakeid)
dataforanalysis<-data.frame(cbind(lagoslakeid, slopesum))

saveRDS(dataforanalysis, file="Datasets/JAGS_TP_july18.rds")


# and N (function, anyone?)

slopemean= apply(slopes, 2, mean)

slopesum = data.frame(cbind(slopeProbs, slopeSign, slopemean))

input<-readRDS('Datasets/tn_july18.rds')
lagoslakeid<-unique(input$lagoslakeid)
dataforanalysis<-data.frame(cbind(lagoslakeid, slopesum))

saveRDS(dataforanalysis, file="Datasets/JAGS_TN_july18.rds")


# and NO3 (finally done)

slopemean= apply(slopes, 2, mean)

slopesum = data.frame(cbind(slopeProbs, slopeSign, slopemean))

input<-readRDS('Datasets/no3_july18.rds')
lagoslakeid<-unique(input$lagoslakeid)
dataforanalysis<-data.frame(cbind(lagoslakeid, slopesum))

saveRDS(dataforanalysis, file="Datasets/JAGS_NO3_july18.rds")


# and chla (notsofast)

slopemean= apply(slopes, 2, mean)

slopesum = data.frame(cbind(slopeProbs, slopeSign, slopemean))

input<-readRDS('Datasets/chl_july18.rds')
lagoslakeid<-unique(input$lagoslakeid)
dataforanalysis<-data.frame(cbind(lagoslakeid, slopesum))

saveRDS(dataforanalysis, file="Datasets/JAGS_Chla_july18.rds")


#### summarize precip trends that accompany DOC and color trends (matched yearly precip data for the lake year median DOC data, then color data)

# for precip associated with DOC

slopemean= apply(slopes, 2, mean)

slopesum = data.frame(cbind(slopeProbs, slopeSign, slopemean))

input<-readRDS('Datasets/precipdoc_july18.rds')
lagoslakeid<-unique(input$lagoslakeid)
dataforanalysis<-data.frame(cbind(lagoslakeid, slopesum))

saveRDS(dataforanalysis, file="Datasets/JAGS_PPTDOC_july18.rds")



# and associated w/ color

slopemean= apply(slopes, 2, mean)

slopesum = data.frame(cbind(slopeProbs, slopeSign, slopemean))

input<-readRDS('Datasets/precipcol_mar19.rds')
lagoslakeid<-unique(input$lagoslakeid)
dataforanalysis<-data.frame(cbind(lagoslakeid, slopesum))

saveRDS(dataforanalysis, file="Datasets/JAGS_PPTCOL_mar19.rds")


#### summarize temperature trends that accompany DOC and color trends (matched yearly tmean data for the lake year median DOC data, then color data)

# for tmean associated with DOC

slopemean= apply(slopes, 2, mean)

slopesum = data.frame(cbind(slopeProbs, slopeSign, slopemean))

input<-readRDS('Datasets/tmeandoc_mar19.rds')
lagoslakeid<-unique(input$lagoslakeid)
dataforanalysis<-data.frame(cbind(lagoslakeid, slopesum))

saveRDS(dataforanalysis, file="Datasets/JAGS_TMEANDOC_mar19.rds")



# and associated w/ color

slopemean= apply(slopes, 2, mean)

slopesum = data.frame(cbind(slopeProbs, slopeSign, slopemean))

input<-readRDS('Datasets/tmeancol_mar19.rds')
lagoslakeid<-unique(input$lagoslakeid)
dataforanalysis<-data.frame(cbind(lagoslakeid, slopesum))

saveRDS(dataforanalysis, file="Datasets/JAGS_TMEANCOL_mar19.rds")
