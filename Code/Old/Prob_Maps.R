library(lubridate)
library(tidyverse)
library(sf)
library(LAGOSNE)
library(USAboundaries)
library(USAboundariesData)
library(cowplot)

#get data files
dat_full <-lagosne_load(version = "1.087.1")
JAGS_DOC <- readRDS("~/Documents/GitHub/LAGOS_DOC/Datasets/JAGS_DOC.rds")

JAGS_DOC$slopeProbs[which(JAGS_DOC$slopeSign==0)] <- 
  abs(JAGS_DOC$slopeProbs[which(JAGS_DOC$slopeSign==0)]-1)
#add lat longs
dat <- dat_full$locus %>% 
  select(lagoslakeid,nhd_long,nhd_lat) %>% 
  right_join(JAGS_DOC)
dat <- coordinatize(dat)
state_names= c('wisconsin','minnesota','vermont','maine','michigan','missouri','rhode island','new york','iowa','illinois','indiana','ohio','new hampshire','pennsylvania','connecticut','massachusetts','new jersey')
lagos_states<-us_states(resolution = "high", states = state_names) %>%
  st_transform(crs = 4326)
p <- ggplot() +
  geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) +
  geom_sf(data = dat,
          aes(color=slopeProbs), alpha = 0.6,show.legend=T) +
  scale_colour_gradient(low = "red",high = "blue", breaks=c(0,.5,1))
p

ggplot2::ggsave(filename = "Figures/Trend_prob_map.png",plot = p,units = "in",width = 6,height = 3,dpi = 300)
