library(sp)
library(rgeos)
dir <- '~/google_drive/research/data/weather/spatial_weather/bin/'
## Minimum working example of matching point to nearest X weather stations
## Load county centroids
 locations <- read.delim(paste(dir,'county_test.txt',sep=''),header=T, sep=",")
## locations <- read.delim(paste(dir,'county_centroid.txt',sep=''),header=T, sep=",")
## Make string FIPS codes
locations$state_fips <- sprintf("%02d",locations$state_fips)
locations$county_fips <- sprintf("%03d",locations$county_fips)
locations$fips <- paste(locations$state_fips,locations$county_fips,sep='')
locations$state_fips <- locations$fips
locations$county_fips <- NULL
locations$fips <- NULL
names(locations)[1] <- 'fips'

## Set spatial parameters
sp_locations <- locations
coordinates(sp_locations) <- ~longitude+latitude

## Here is a neat trick: calculate the k nearest neighbors for every point!
k <- 2
d <- gDistance(sp_locations, byid=T)
min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2:(k+1)])

## Alterations to find k nearest neighbors
## Brute force
for (i in 1:k) {
    if (i == 1) {
        newdata <- cbind(locations, n_id=1, locations[min.d[i,],],
                         apply(d, 1, function(x) sort(x, decreasing=F)[2]))
    } else {
        newdata <- rbind(newdata,
                         setNames(cbind(locations, n_id=i, locations[min.d[i,],],
                                        apply(d, 1, function(x) sort(x, decreasing=F)[(i+1)])),
                                  names(newdata)))
    }
}
colnames(newdata) <- c(colnames(locations),
                       'n_id', 'n_fips', 'n_county_name', 'n_state_name',
                       'n_population','n_latitude','n_longitude','dist')
newdata <- newdata[order(newdata$fips),]
newdata[1:12,]
