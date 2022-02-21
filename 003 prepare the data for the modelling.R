
##################################################################
# Getting data in shape


# Load population dataset per grid - with firm counts year by year per cell
load("data/gridFirmyCountsYearByYear2.RData")
pop.df.waw.firms %>% head()

# Load population dataset per grid - with dummies about whether there is a cluster or now
load('data/gridFirmyClustersYearByYear.RData')
pop.df.waw.clusters %>% head()



# create a copy of an object to make transformations
pop.df.waw.firmsAcc <- pop.df.waw.firms

pop.df.waw.firmsAcc %>% head()
pop.df.waw.firmsAcc$ID_grid <- 1:601

pop.df.waw.firmsAcc <- pop.df.waw.firmsAcc %>% mutate(clust2010 = pop.df.waw.clusters$clust2010, 
                                                      clust2011 = pop.df.waw.clusters$clust2011, 
                                                      clust2012 = pop.df.waw.clusters$clust2012, 
                                                      clust2013 = pop.df.waw.clusters$clust2013, 
                                                      clust2014 = pop.df.waw.clusters$clust2014, 
                                                      clust2015 = pop.df.waw.clusters$clust2015, 
                                                      clust2016 = pop.df.waw.clusters$clust2016, 
                                                      clust2017 = pop.df.waw.clusters$clust2017, 
                                                      clust2018 = pop.df.waw.clusters$clust2018)



# Tune intensities -> to fraction of all firms in a given year
pop.df.waw.firmsAcc %>% select(Year2010) %>% sum() ->tot10
pop.df.waw.firmsAcc %>% select(Year2011) %>% sum() ->tot11
pop.df.waw.firmsAcc %>% select(Year2012) %>% sum() ->tot12
pop.df.waw.firmsAcc %>% select(Year2013) %>% sum() ->tot13
pop.df.waw.firmsAcc %>% select(Year2014) %>% sum() ->tot14
pop.df.waw.firmsAcc %>% select(Year2015) %>% sum() ->tot15
pop.df.waw.firmsAcc %>% select(Year2016) %>% sum() ->tot16
pop.df.waw.firmsAcc %>% select(Year2017) %>% sum() ->tot17
pop.df.waw.firmsAcc %>% select(Year2018) %>% sum() ->tot18

# new object with fractions and lag total
pop.df.waw.firmsAcc.totals <- pop.df.waw.firmsAcc

# adding totals of each year
pop.df.waw.firmsAcc.totals$total10 <- tot10
pop.df.waw.firmsAcc.totals$total11 <- tot11
pop.df.waw.firmsAcc.totals$total12 <- tot12
pop.df.waw.firmsAcc.totals$total13 <- tot13
pop.df.waw.firmsAcc.totals$total14 <- tot14
pop.df.waw.firmsAcc.totals$total15 <- tot15
pop.df.waw.firmsAcc.totals$total16 <- tot16
pop.df.waw.firmsAcc.totals$total17 <- tot17
pop.df.waw.firmsAcc.totals$total18 <- tot18

#Fraction of the total number of companies founded in given year
pop.df.waw.firmsAcc.totals$frac10 <- pop.df.waw.firmsAcc.totals$Year2010/pop.df.waw.firmsAcc.totals$total10
pop.df.waw.firmsAcc.totals$frac11 <- pop.df.waw.firmsAcc.totals$Year2011/pop.df.waw.firmsAcc.totals$total11
pop.df.waw.firmsAcc.totals$frac12 <- pop.df.waw.firmsAcc.totals$Year2012/pop.df.waw.firmsAcc.totals$total12
pop.df.waw.firmsAcc.totals$frac13 <- pop.df.waw.firmsAcc.totals$Year2013/pop.df.waw.firmsAcc.totals$total13
pop.df.waw.firmsAcc.totals$frac14 <- pop.df.waw.firmsAcc.totals$Year2014/pop.df.waw.firmsAcc.totals$total14
pop.df.waw.firmsAcc.totals$frac15 <- pop.df.waw.firmsAcc.totals$Year2015/pop.df.waw.firmsAcc.totals$total15
pop.df.waw.firmsAcc.totals$frac16 <- pop.df.waw.firmsAcc.totals$Year2016/pop.df.waw.firmsAcc.totals$total16
pop.df.waw.firmsAcc.totals$frac17 <- pop.df.waw.firmsAcc.totals$Year2017/pop.df.waw.firmsAcc.totals$total17
pop.df.waw.firmsAcc.totals$frac18 <- pop.df.waw.firmsAcc.totals$Year2018/pop.df.waw.firmsAcc.totals$total18



## Normalizing to 0 1 -> we have probability of being the most intense cluster
normaliz01 <- function(x, na.rm = TRUE) (x-min(x, na.rm = na.rm))/(max(x, na.rm = na.rm)-min(x, na.rm = na.rm))

pop.df.waw.firmsAcc.totals <- pop.df.waw.firmsAcc.totals %>% mutate(across(matches("frac"), normaliz01))


#Make a spatial weight matrix to get neighbourhood averages

# contiguity spatial weights matrix for grid
cont.nb<-poly2nb(pop.grid.waw, queen=T) # conversion of sp to nb class
cont.listw<-nb2listw(cont.nb, style="W") 
cont.listw # summary of spatial weights matrix



#Lags in direct neighborhood (from fractions, average fraction in the direct neighborhood)
pop.df.waw.firmsAcc.totals$lag_frac10<-lag.listw(x=cont.listw, var=(pop.df.waw.firmsAcc.totals$frac10))
pop.df.waw.firmsAcc.totals$lag_frac11<-lag.listw(x=cont.listw, var=(pop.df.waw.firmsAcc.totals$frac11))
pop.df.waw.firmsAcc.totals$lag_frac12<-lag.listw(x=cont.listw, var=(pop.df.waw.firmsAcc.totals$frac12))
pop.df.waw.firmsAcc.totals$lag_frac13<-lag.listw(x=cont.listw, var=(pop.df.waw.firmsAcc.totals$frac13))
pop.df.waw.firmsAcc.totals$lag_frac14<-lag.listw(x=cont.listw, var=(pop.df.waw.firmsAcc.totals$frac14))
pop.df.waw.firmsAcc.totals$lag_frac15<-lag.listw(x=cont.listw, var=(pop.df.waw.firmsAcc.totals$frac15))
pop.df.waw.firmsAcc.totals$lag_frac16<-lag.listw(x=cont.listw, var=(pop.df.waw.firmsAcc.totals$frac16))
pop.df.waw.firmsAcc.totals$lag_frac17<-lag.listw(x=cont.listw, var=(pop.df.waw.firmsAcc.totals$frac17))
pop.df.waw.firmsAcc.totals$lag_frac18<-lag.listw(x=cont.listw, var=(pop.df.waw.firmsAcc.totals$frac18))



#dataset to tune model on
companiesGrid <- pop.df.waw.firmsAcc.totals[,c("ID", "ID_grid", "frac10","frac11", "frac12","frac13","frac14","frac15",
                                               "frac16", "frac17","frac18","lag_frac10", "lag_frac11",
                                               "lag_frac12","lag_frac13","lag_frac14","lag_frac15","lag_frac16",
                                               "lag_frac17", "lag_frac18", 
                                               "clust2010", "clust2011", "clust2012", "clust2013", "clust2014",
                                               "clust2015", "clust2016", "clust2017", "clust2018")]


################### Prepare for Neural Networks 


companiesGridFrac <- companiesGrid[,1:11] 
companiesGridFracLag <- companiesGrid[,c(1:2,12:20)] 
companiesGridClust <- companiesGrid[,c(1:2,21:29)] 


compFrac <- melt(setDT(companiesGridFrac ), measure.vars = 3:11, variable.name = "year")
names(compFrac)[4] <- "frac"
compFracLag <- melt(setDT(companiesGridFracLag), measure.vars = 3:11, variable.name = "year")
names(compFracLag)[4] <- "fracLag"
compFracClust <- melt(setDT(companiesGridClust), measure.vars = 3:11, variable.name = "year")
names(compFracClust)[4] <- "clust"

# improve lower parts


compFracLag$year <- substr(compFracLag$year, 5, 10)
compFracClust$year <- gsub("clust20", "frac", compFracClust$year)

companiesGridLongA <- merge(compFrac, compFracLag, by = c("ID", "ID_grid", "year"))
companiesGridLong <- merge(companiesGridLongA, compFracClust, by = c("ID", "ID_grid", "year"))

companiesGridLong$year <- gsub("frac", "20", companiesGridLong$year)

#Data in long format, we have ID from map (total grid) and ID_grid in Warsaw grid
#frac is a fraction of start-ups we have in this particular cell
#fracLag is a fraction of start-ups averaged in the neighborhood (spatial lag)
head(companiesGridLong)


########### Get rolling windows 

## Filling in empty years
pa_padded <- companiesGridLong %>%
  select(ID, ID_grid, year, frac, fracLag, clust) %>%
  arrange(ID_grid, year) %>%
  mutate(date = as.Date(paste0(year, '-12-31'))) %>% 
  arrange(ID_grid, date) %>%
  pad(interval='year', by='date', group='ID_grid') %>% 
  mutate(yearID = year(date), frac = replace_na(frac, 0), fracLag = replace_na(fracLag, 0), 
         clust = replace_na(clust, 0)) %>%
  select(-date) 


pa_window <- pa_padded %>%
  group_by(ID_grid) %>%
  rename(frac_current = frac, fracLag_current = fracLag,
         clust_current = clust) %>%
  mutate(frac_minus1 = lag(frac_current, 1),
         frac_minus2 = lag(frac_current, 2),
         frac_plus1 = lead(frac_current, 1),
         fracLag_minus1 = lag(fracLag_current, 1),
         fracLag_minus2 = lag(fracLag_current, 2),
         fracLag_plus1 = lead(fracLag_current, 1),
         clust_minus1 = lag(clust_current, 1),
         clust_minus2 = lag(clust_current, 2),
         clust_plus1 = lead(clust_current, 1),
  ) %>%
  select(ID, ID_grid, yearID, frac_minus2, frac_minus1, frac_current, frac_plus1,
         fracLag_minus2, fracLag_minus1, fracLag_current, fracLag_plus1,
         clust_minus2, clust_minus1, clust_current, clust_plus1) %>%
  filter(!is.na(frac_minus1) & !is.na(frac_minus2) & !is.na(frac_plus1) &
           !is.na(fracLag_minus1) & !is.na(fracLag_minus2) & !is.na(fracLag_plus1) &
           !is.na(clust_minus1) & !is.na(clust_minus2) & !is.na(clust_plus1)) %>% 
  ungroup



#Removing 2017 from the sample - to put it into the final testing 

nrow(pa_window)

# all the observations are here
pa_windowAll <- pa_window

# dataset that will be passed into training (except for the last year in sample)
pa_window <- pa_windowAll[pa_windowAll$yearID!="2017",]

# dataset with the last year for predictions only
pa_window2017 <- pa_windowAll[pa_windowAll$yearID=="2017",]





