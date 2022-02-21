#################################################################
# Get packages

# set python environment 
pathToPython = "C:/Users/maria/anaconda3/envs/snowflakes"

Sys.setenv(RETICULATE_PYTHON = pathToPython)
library(reticulate)
import("tensorflow")

library(RColorBrewer)
library(sp)
library(spdep)
library(spatstat)
library(rgdal)
library(maptools)
library(GISTools)
library(raster) # for grid merging
library(tidyverse)
library(sf)
library(keras)
library(dbscan)
library(factoextra)
library(fpc)
library(data.table)
library(reshape2)
library(padr)

#################################################################
# loading previously prepared grid population data for Warsaw
load('data/gridPopWaw.RData')

#pop.df.waw - data from grid
#pop.grid.waw - spatial polygons object
#pop.waw Spatial Polygons Data Frame - all in one
pop.grid.waw.sf <- st_as_sf(pop.grid.waw, crs = "+proj=longlat +datum=NAD83")

# plot grid geometry
st_geometry(pop.grid.waw.sf) %>% plot()

#Grid ID to order and merge the data 
pop.grid.waw.sf$ID_grid <- 1:601



#################################################################
# load start-up data and prepare year-by-year samples

load('data/companiesPointData.RData')
# firmSubLimSelf #dataset with point coordinates of start-ups in Warsaw

#podzbiory do wizualizacji
firmSubLimSelf10<-firmSubLimSelf[firmSubLimSelf$Incorp==2010,] 
firmSubLimSelf11<-firmSubLimSelf[firmSubLimSelf$Incorp==2011,] 
firmSubLimSelf12<-firmSubLimSelf[firmSubLimSelf$Incorp==2012,] 
firmSubLimSelf13<-firmSubLimSelf[firmSubLimSelf$Incorp==2013,] 
firmSubLimSelf14<-firmSubLimSelf[firmSubLimSelf$Incorp==2014,] 
firmSubLimSelf15<-firmSubLimSelf[firmSubLimSelf$Incorp==2015,] 
firmSubLimSelf16<-firmSubLimSelf[firmSubLimSelf$Incorp==2016,] 
firmSubLimSelf17<-firmSubLimSelf[firmSubLimSelf$Incorp==2017,] 
firmSubLimSelf18<-firmSubLimSelf[firmSubLimSelf$Incorp==2018,] 


#################################################################
# Loading poviat maps - in two classes

pow<-readOGR("data", "powiaty") 
pow<- spTransform(pow, CRS("+proj=longlat +datum=NAD83"))
waw.pow<-pow[pow$jpt_nazwa_=='powiat Warszawa',]

pov.sf <- st_read("data/powiaty.shp")
pov.sf <- st_transform(pov.sf, crs = "+proj=longlat +datum=NAD83")
waw.pov.sf <- pov.sf %>% filter(jpt_nazwa_=='powiat Warszawa') 

#################################################################
# preparing count data year by year

countsByYearT <- matrix(data=NA, nrow=1, ncol=601)
countsByYearT[,1:30]

for (i in 10:18){
  nameOfDataset <- paste0("firmSubLimSelf", i)
  firmy.sel <- get(nameOfDataset)
  
  coordinates(firmy.sel)<-c("Longitude","Latitude") # class changed for SpatialPoints
  pop.grid.waw$ID<-rownames(pop.df.waw) # identification of units in grid
  pop.df.waw$ID<-rownames(pop.df.waw) # identification of units in data.frame
  crs(firmy.sel)<-crs(pop.grid.waw) # equalize the projection between objects
  firmy.sel$ID<-over(firmy.sel, pop.grid.waw) # assign ID grid to companies 
  head(firmy.sel)
  
  crds<-coordinates(pop.grid.waw) # centroids of grid cells 
  
  # changing the class of the object
  firmy.sel.df<-as.data.frame(firmy.sel)
  
  # point data aggregation by grid ID
  firmy.agg.no<-aggregate(firmy.sel.df$ID, by=list(firmy.sel.df$ID), length)
  
  # merge datasets 
  pop.df.waw.m<-merge(pop.df.waw, firmy.agg.no, by.x="ID", by.y="Group.1", all.x=TRUE)
  names(pop.df.waw.m)
  names(pop.df.waw.m)[18] <- "countComp"
  
  # clean the NA values
  m1<-which(is.na(pop.df.waw.m$countComp))
  pop.df.waw.m$countComp[m1]<-0
  
  #merging with results matrix
  countsByYearT<- rbind(countsByYearT,pop.df.waw.m$countComp )
}

totalCountInGrid <- colSums(countsByYearT,na.rm=T)
totalCountInGrid[1:30]

countsByYearT[1,]<- totalCountInGrid
#save total count per grid cell as the first row

#transpose into the shape according to population grid
countsByYear <- t(countsByYearT)
countsByYear[1:30,]

colnames(countsByYear) <- c('totCount', 'Year2010', 'Year2011',  'Year2012',  'Year2013',
                            'Year2014',  'Year2015',  'Year2016',  'Year2017',  'Year2018')

#bind pop grid data with firm counts year by year
pop.df.waw.firms<- cbind(pop.df.waw, countsByYear)
head(pop.df.waw.firms)

save(pop.df.waw.firms, file = 'data/gridFirmyCountsYearByYear2.RData')
#load('gridFirmyTotal.RData')
