################################################################################
# Packages

library(RColorBrewer)
library(sp)
library(spdep)
library(spatstat)
# library(rgdal) #archived on CRAN along spatial package evolution
# library(maptools)
# library(GISTools)
library(raster) # for grid merging
library(tidyverse)
library(sf)
library(keras)
library(dbscan)
library(factoextra)
library(fpc)
library(osmdata)
library(OpenStreetMap)
library(cowplot)
library(viridis)


# figure with 2017 model comparison is in the 007 script (end part)

################################################################################
# Data loading

# Powiat data
pow<-readOGR("data", "powiaty") 
pow<- spTransform(pow, CRS("+proj=longlat +datum=NAD83"))
waw.pow<-pow[pow$jpt_nazwa_=='powiat Warszawa',]

pov.sf <- st_read("data/powiaty.shp")
pov.sf <- st_transform(pov.sf, crs = "+proj=longlat +datum=NAD83")
waw.pov.sf <- pov.sf %>% filter(jpt_nazwa_=='powiat Warszawa') 


################################################################################
# Counts of companies by grid 
load('data/gridFirmyCountsYearByYear2.RData')
head(pop.df.waw.firms)

################################################################################
# Census grid division
load('data/gridPopWaw.RData')

pop.grid.waw.sf <- st_as_sf(pop.grid.waw, crs = "+proj=longlat +datum=NAD83")
st_geometry(pop.grid.waw.sf) %>% plot()
#Grid ID to order and merge the data 
pop.grid.waw.sf$ID_grid <- 1:601

################################################################################
# object for visualisation purposes
play <- pop.grid.waw.sf
play$totalCount <- pop.df.waw.firms$totCount

################################################################################
# Point data about companies

load('data/companiesPointData.RData')
head(firmSubLimSelf)
# firmSubLimSelf #dataset with point coordinates of start-ups in Warsaw

firmSubLimSelf.sf <- st_as_sf(firmSubLimSelf, coords = c("Longitude", "Latitude"), 
                              crs = "+proj=longlat +datum=NAD83")

firmSubLimSelf <- firmSubLimSelf %>% 
  filter(st_within(firmSubLimSelf.sf, waw.pov.sf, sparse = F))

#ID for dataset merging
firmSubLimSelf$IDtotal <- 1:nrow(firmSubLimSelf)

#subsets for visualization
firmSubLimSelf10<-firmSubLimSelf[firmSubLimSelf$Incorp==2010,] 
firmSubLimSelf11<-firmSubLimSelf[firmSubLimSelf$Incorp==2011,] 
firmSubLimSelf12<-firmSubLimSelf[firmSubLimSelf$Incorp==2012,] 
firmSubLimSelf13<-firmSubLimSelf[firmSubLimSelf$Incorp==2013,] 
firmSubLimSelf14<-firmSubLimSelf[firmSubLimSelf$Incorp==2014,] 
firmSubLimSelf15<-firmSubLimSelf[firmSubLimSelf$Incorp==2015,] 
firmSubLimSelf16<-firmSubLimSelf[firmSubLimSelf$Incorp==2016,] 
firmSubLimSelf17<-firmSubLimSelf[firmSubLimSelf$Incorp==2017,] 
firmSubLimSelf18<-firmSubLimSelf[firmSubLimSelf$Incorp==2018,] 



###################################################################
################ Map objects ######################################

WAW_box2<-getbb("Warszawa, Polska")
query<-opq(WAW_box2) %>% add_osm_feature(key='admin_level', value='9') #uzyskiwanie dzielnic

bound_waw.sf<-osmdata_sf(query) #get the warsaw discrict lines

# adding a buffer to the borders to mitigate the uneven line problem
smallbuffer <- st_buffer(waw.pov.sf, dist = 0.0007)

# borders of districts in Warsaw
wawDistBorders <- st_intersection(bound_waw.sf$osm_lines %>% 
                                    st_transform(crs = "+proj=longlat +datum=NAD83"), 
                                  smallbuffer)

waw.pov.sf.line <- st_cast(waw.pov.sf,"MULTILINESTRING")

#####################################################################
####################### Background maps #############################

library(OpenStreetMap)

Waw_osm<-openmap(c(52.40951,20.84959),c(52.04650,21.27325)) #maxmin minmax  map dimension


#######################################################
# FIGURE 1

points <- autoplot.OpenStreetMap( OpenStreetMap::openproj( Waw_osm ) ) +
  geom_point( data = firmSubLimSelf, aes( x = Longitude, y = Latitude), shape=18, size = 1, alpha = .5, col = "#33638DFF")+
  geom_sf(wawDistBorders, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  geom_sf(waw.pov.sf.line, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="bottom") 

# GRID plot with open street map background
grid <- autoplot.OpenStreetMap( OpenStreetMap::openproj( Waw_osm ) ) +
  geom_sf(play, mapping = aes(geometry = geometry, fill = totalCount), alpha = 0.85, inherit.aes = FALSE) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  scale_fill_viridis(option = "H")

plot_grid(points, grid, ncol = 2, align = "hv") # 840x400


#####################################################
# FIGURE 2 #540x680

# Plot with points over the years
autoplot.OpenStreetMap( OpenStreetMap::openproj( Waw_osm ) ) +
  geom_point( data = firmSubLimSelf, aes( x = Longitude, y = Latitude), shape=18,  col = "#33638DFF", size = 1, alpha = .5 )+
  geom_sf(wawDistBorders, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  geom_sf(waw.pov.sf.line, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  #geom_sf(waw.pov.sf, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  theme_bw() +
  facet_wrap(~ Incorp, nrow=3)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#####################################################
# FIGURE 3

#hot spot map
autoplot.OpenStreetMap( OpenStreetMap::openproj( Waw_osm ) ) +
  stat_density_2d(data = firmSubLimSelf,
                  aes(x = Longitude,
                      y = Latitude,
                      fill = stat(level)),
                  alpha = .9,
                  bins = 25,
                  geom = "polygon") +
  # adding district data
  geom_sf(wawDistBorders, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  geom_sf(waw.pov.sf.line, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  scale_fill_gradientn(colors = brewer.pal(7, "YlOrRd"))+
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) 


#####################################################
# FIGURE 4


# Plot with densities over the years
#autoplot.OpenStreetMap( OpenStreetMap::openproj( Waw_osm ) ) +
ggplot()+
  stat_density_2d(data = firmSubLimSelf ,
                  aes(x = Longitude,
                      y = Latitude,
                      fill = stat(level)),
                  alpha = .9,
                  bins = 25,
                  geom = "polygon") +
  scale_fill_gradientn(colors = brewer.pal(7, "YlOrRd")) +
  geom_sf(wawDistBorders, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  geom_sf(waw.pov.sf.line, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  facet_wrap(~ Incorp, nrow=3)+
  theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


###################################################
# FIGURE 5

library("dbscan")
library('factoextra')
library('fpc')


set.seed(111)
db<- dbscan::dbscan(firmSubLimSelf[,2:3], eps=0.0035, minPts=20)

firmSubLimSelf$clust<-db$cluster
firmSubLimSelf[firmSubLimSelf$clust==0,]$clust<-NA
firmSubLimSelf$clust<-as.factor(firmSubLimSelf$clust)

firmSubLimSelf$clust %>% summary()
inClust <- nrow(firmSubLimSelf[!is.na(firmSubLimSelf$clust),])
outClust <- nrow(firmSubLimSelf[is.na(firmSubLimSelf$clust),])
noClust <- max(as.integer(firmSubLimSelf$clust), na.rm = T)


orderClust <- (firmSubLimSelf) %>% 
  group_by(firmSubLimSelf$clust) %>%
  summarize(Count=n()) %>%
  arrange(desc(Count))

orderClust$`firmSubLimSelf$clust`

orderClust <- orderClust[-2,]
orderClust$order <- 1:nrow(orderClust)
names(orderClust)[1] <- "clust2"

firmSubLimSelf$clust2 <- as.numeric(firmSubLimSelf$clust)
firmSubLimSelf.merge <- merge(firmSubLimSelf, orderClust, by = "clust2", all.x = TRUE, sort = FALSE)

firmSubLimSelf.merge$order <- as.numeric(firmSubLimSelf.merge$order)

ggplot()+
  geom_point( data = firmSubLimSelf.merge[is.na(firmSubLimSelf.merge$order),], aes( x = Longitude, y = Latitude), col = "gray", shape=20,  size = 1)+
  geom_point( data = firmSubLimSelf.merge[!is.na(firmSubLimSelf.merge$order),], aes( x = Longitude, y = Latitude, col = order), shape=16,  size = 1)+
  geom_sf(wawDistBorders, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  geom_sf(waw.pov.sf.line, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  scale_colour_viridis(option = "H", trans = 'reverse') +
  labs(title ="", color = "Cluster ordering \nby number of points")+
  theme_minimal()
#870x580

###################################################
# FIGURE 6

set.seed(111)
db<- fpc::dbscan(firmSubLimSelf10[,2:3], eps=0.006, MinPts=10) 
firmSubLimSelf10$cluster <- db$cluster

set.seed(111)
db<- fpc::dbscan(firmSubLimSelf11[,2:3], eps=0.006, MinPts=10) 
firmSubLimSelf11$cluster <- db$cluster

set.seed(111)
db<- fpc::dbscan(firmSubLimSelf12[,2:3], eps=0.006, MinPts=10) 
firmSubLimSelf12$cluster <- db$cluster

set.seed(111)
db<- fpc::dbscan(firmSubLimSelf13[,2:3], eps=0.006, MinPts=10)
firmSubLimSelf13$cluster <- db$cluster

set.seed(111)
db<- fpc::dbscan(firmSubLimSelf14[,2:3], eps=0.006, MinPts=10) 
firmSubLimSelf14$cluster <- db$cluster

set.seed(111)
db<- fpc::dbscan(firmSubLimSelf15[,2:3], eps=0.006, MinPts=10) 
firmSubLimSelf15$cluster <- db$cluster

set.seed(111)
db<- fpc::dbscan(firmSubLimSelf16[,2:3], eps=0.006, MinPts=10) 
firmSubLimSelf16$cluster <- db$cluster

set.seed(111)
db<- fpc::dbscan(firmSubLimSelf17[,2:3], eps=0.006, MinPts=10) 
firmSubLimSelf17$cluster <- db$cluster

set.seed(111)
db<- fpc::dbscan(firmSubLimSelf18[,2:3], eps=0.006, MinPts=10) 
firmSubLimSelf18$cluster <- db$cluster

totalYears <- rbind(firmSubLimSelf10, firmSubLimSelf11, firmSubLimSelf12, 
                    firmSubLimSelf13, firmSubLimSelf14, firmSubLimSelf15,
                    firmSubLimSelf16, firmSubLimSelf17, firmSubLimSelf18)

names(totalYears)

totalYears<- totalYears[, c("IDtotal", "cluster")]

firmSubLimSelf.clust <- merge(firmSubLimSelf, totalYears, by.x="IDtotal", all.x=TRUE)
firmSubLimSelf.clust[firmSubLimSelf.clust$cluster == 0, "cluster"] <- NA

firmSubLimSelf.clust %>% group_by(Incorp) %>% summarize(clustNo = max(cluster, na.rm = T), n = length(cluster), 
                                                        outClust = length(which(is.na(cluster))),
                                                        inClust = n-outClust,
                                                        outShare = outClust/n)


ggplot()+
  geom_point( data = firmSubLimSelf.clust[is.na(firmSubLimSelf.clust$cluster),], aes( x = Longitude, y = Latitude), col = "gray", shape=20,  size = 1)+
  geom_point( data = firmSubLimSelf.clust[!is.na(firmSubLimSelf.clust$cluster),], aes( x = Longitude, y = Latitude, col = cluster), shape=16,  size = 1)+
  geom_sf(wawDistBorders, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  geom_sf(waw.pov.sf.line, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  theme_bw() +
  scale_colour_viridis(direction = -1, option = "H") +
  facet_wrap(~ Incorp, nrow=3)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  guides(color=FALSE) #remove alpha legend part
#600x680



################################################################################
# Figure 11 - 2018 rok


point18 <- ggplot() +
  geom_point( data = subset(firmSubLimSelf, Incorp == 2018), aes( x = Longitude, y = Latitude), 
              shape=18, size = 1, alpha = 1, col = "#33638DFF")+
  geom_sf(wawDistBorders, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  geom_sf(waw.pov.sf.line, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="bottom",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


dens18 <- ggplot()+
  geom_sf(waw.pov.sf, mapping = aes(geometry=geometry), col = "grey", inherit.aes = FALSE)+
  stat_density_2d(data = subset(firmSubLimSelf, Incorp == 2018) ,
                  aes(x = Longitude,
                      y = Latitude,
                      fill = stat(level)),
                  alpha = 1,
                  bins = 25,
                  geom = "polygon") +
  scale_fill_gradientn(colors = brewer.pal(7, "YlOrRd")) +
  geom_sf(wawDistBorders, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  geom_sf(waw.pov.sf.line, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1) ) 


firmSubLimSelf.clust2018 <- subset(firmSubLimSelf.clust, Incorp==2018)

clust18 <- ggplot()+
  geom_point( data = firmSubLimSelf.clust2018[is.na(firmSubLimSelf.clust2018$cluster),], aes( x = Longitude, y = Latitude), col = "gray", shape=16,  size = 1)+
  geom_point( data = firmSubLimSelf.clust2018[!is.na(firmSubLimSelf.clust2018$cluster),], aes( x = Longitude, y = Latitude, col = cluster), shape=16,  size = 1)+
  geom_sf(wawDistBorders, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  geom_sf(waw.pov.sf.line, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  theme_bw() +
  scale_colour_viridis(direction = -1, option = "H") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#600x680


plot_grid(dens18, clust18, ncol = 2, align = "hv") # 840x360
