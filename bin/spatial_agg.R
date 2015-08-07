## Computing distances
## Preliminaries
library(sp)
library(rgeos)
library(RCurl)
packages <- c("proxy", "MASS", "Zelig")
lapply(packages, library, character.only = TRUE)
dir <- '~/google_drive/research/data/weather/spatial_weather/bin/'

## Download weather data
station <- 'AM000037897'
url <- paste0('ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/',station,'.dly')
data <- getURL(url = url)


## Clean county centroid data
## county_file <- 'county_test'
county_file <- 'county_centroid'
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
station_file <- 'ghcnd-stations'
## station_file <- 'ghcnd-test'
cols <- c(11,9,10,7,34,5,10)
laf <- laf_open_fwf(paste0(dir,station_file,'.txt'), column_widths = cols, 
                    column_types=rep("character", length(cols)))
stations <- laf[,]
rm(laf)
names(stations) <- c('id','latitude','longitude','elevation','name','gsn flag','wmo id')
stations$latitude <- as.numeric(stations$latitude)
stations$longitude <- as.numeric(stations$longitude)
stations$elevation <- as.numeric(stations$elevation)

## vars <- c('stations$latitude','stations$longitude','stations$elevation')
## lapply(lapply(packages, library, character.only = TRUE), '<-')
saveRDS(stations, file=paste0(dir,station_file,'.Rda'))

#########################################################
## Load county centroids
counties <- readRDS(file=paste0(dir,county_file,'.Rda'))
## Load weather station locations
stations <- readRDS(file=paste0(dir,station_file,'.Rda'))
## Subsetting stations to just US
stations <- stations[stations$latitude > 25 & stations$latitude < 75
                     & stations$longitude < -60,]

## Set spatial parameters
sp_counties <- counties
coordinates(sp_counties) <- ~longitude+latitude
sp_stations <- stations
coordinates(sp_stations) <- ~longitude+latitude

########################################################
## Now find the nearest weather stations
## Memory intensive way
## This will make a matrix MxK, where M is the length of the
## second dataset and K is the length of the first.
d <- gDistance(sp_stations, sp_counties, byid=TRUE)
## d
min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2])

nearest_station <- cbind(counties, stations[min.d,],
                         apply(d, 1, function(x) sort(x, decreasing=F)[2]))
colnames(nearest_station) <- c(colnames(counties), colnames(stations),'dist')
## Output data
saveRDS(nearest_station, file=paste0(dir,'county_with_station.Rda'))


## Memory non-intensive way
## Loop over the small dataset, finding nearest station for each one.






########################################################
## Here is a neat trick: calculate the k nearest neighbors for every point!
k <- 2
d <- gDistance(sp_counties, byid=T)
min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2:(k+1)])

## Brute force looping -- this could be improved
for (i in 1:k) {
    if (i == 1) {
        counties_knn <- cbind(counties, n_id=1, counties[min.d[i,],],
                              apply(d, 1, function(x) sort(x, decreasing=F)[2]))
    } else {
        counties_knn <- rbind(counties_knn,
                              setNames(cbind(counties, n_id=i,
                                             counties[min.d[i,],],
                                             apply(d, 1, function(x) sort(x, decreasing=F)[(i+1)])),
                                       names(counties_knn)))
    }
}
colnames(counties_knn) <- c(colnames(counties),
                       'n_id', 'n_fips', 'n_county_name', 'n_state_name',
                       'n_population','n_latitude','n_longitude','dist')
counties_knn <- counties_knn[order(counties_knn$fips),]

## Test it out!
counties_knn[1:12,]

