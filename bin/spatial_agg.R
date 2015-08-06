## Computing distances
## Preliminaries
library(sp)
library(rgeos)
library(RCurl)
packages <- c("proxy", "MASS", "Zelig")
lapply(packages, library, character.only = TRUE)
dir <- '~/google_drive/research/data/weather/spatial_weather/bin/'

## Download weather data



## Clean county centroid data
county_file <- 'county_test'
locations <- read.delim(paste0(dir,county_file,'.txt'),header=T, sep=",")

## Make string FIPS codes
locations$state_fips <- sprintf("%02d",locations$state_fips)
locations$county_fips <- sprintf("%03d",locations$county_fips)
locations$fips <- paste0(locations$state_fips,locations$county_fips)
locations$state_fips <- locations$fips
locations$county_fips <- NULL
locations$fips <- NULL
names(locations)[1] <- 'fips'
## Saving
saveRDS(locations, file=paste0(dir,county_file,'.Rda'))

## Weather station locations


## Minimum working example of matching point to nearest X weather stations
## Load county centroids
locations <- readRDS(file=paste0(dir,county_file,'.Rda'))
## Load weather station locations


## Set spatial parameters
sp_locations <- locations
coordinates(sp_locations) <- ~longitude+latitude

## Here is a neat trick: calculate the k nearest neighbors for every point!
k <- 2
d <- gDistance(sp_locations, byid=T)
min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2:(k+1)])

## Brute force looping -- this could be improved
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

## Test it out!
newdata[1:12,]

## Now find the nearest weather stations
## Memory intensive way


## Memory non-intensive way



