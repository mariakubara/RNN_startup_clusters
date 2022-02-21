###########################################################################

# creating yearly division into clusters

###########################################################################
###########################################################################

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


#################################################################
# preparing cluster dummies data year by year


countsByYearT <- matrix(data=NA, nrow=1, ncol=601)
countsByYearT[,1:30]

for (i in 10:18){
  nameOfDataset <- paste0("firmSubLimSelf", i)
  firmy.sel <- get(nameOfDataset)
  
  coordinates(firmy.sel)<-c("Longitude","Latitude") 
  pop.grid.waw$ID<-rownames(pop.df.waw) 
  pop.df.waw$ID<-rownames(pop.df.waw) 
  crs(firmy.sel)<-crs(pop.grid.waw) 
  firmy.sel$ID<-over(firmy.sel, pop.grid.waw) 
  
  crds<-coordinates(pop.grid.waw) 

  firmy.sel.df<-as.data.frame(firmy.sel)

  firmy.agg.no<-aggregate(firmy.sel.df$cluster, by=list(firmy.sel.df$ID), sum)
  
  pop.df.waw.m<-merge(pop.df.waw, firmy.agg.no, by.x="ID", by.y="Group.1", all.x=TRUE)
  names(pop.df.waw.m)
  names(pop.df.waw.m)[18] <- "isCluster"
  
  m1<-which(is.na(pop.df.waw.m$isCluster)) 
  pop.df.waw.m$isCluster[m1]<-0
  
  m2<-which(pop.df.waw.m$isCluster>0) 
  pop.df.waw.m$isCluster[m2]<-1
  
  #merging with results matrix
  countsByYearT<- rbind(countsByYearT,pop.df.waw.m$isCluster )
}

totalCountInGrid <- colSums(countsByYearT,na.rm=T)
totalCountInGrid[1:30]

countsByYearT[1,]<- totalCountInGrid
#save total count per grid cell as the first row

#transpose into the shape according to population grid
countsByYear <- t(countsByYearT)
countsByYear[1:30,]

colnames(countsByYear) <- c('totClustOverlap', 'clust2010', 'clust2011',  'clust2012',  'clust2013',
                            'clust2014',  'clust2015',  'clust2016',  'clust2017',  'clust2018')

#bind pop grid data with firm counts year by year
pop.df.waw.firms<- cbind(pop.df.waw, countsByYear)
head(pop.df.waw.firms)

# change the name
pop.df.waw.clusters <- pop.df.waw.firms

save(pop.df.waw.clusters, file = 'data/gridFirmyClustersYearByYear.RData')
