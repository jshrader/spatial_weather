## Matching twitter data with pollution and weather data
## Preliminaries
rm(list = ls())
packages <- c("proxy", "MASS", "Zelig","sp","rgeos","RCurl")
lapply(packages, library, character.only = TRUE)

## Debug switch
debug <- TRUE

## Directories
dir <- '~/google_drive/research/data/weather/spatial_weather/bin/'
weather_dir <- '/Volumes/Data 2TB/data/weather/'
twitter_dir <- '/Volumes/Data 2TB/data/twitter/'
pollution_dir <- '~/google_drive/research/data/pollution/data/raw/hourly/'

if( debug == TRUE ){
    dir <- '~/google_drive/research/data/weather/spatial_weather/bin/'
    weather_dir <- '/Volumes/Data 2TB/data/weather/test/'
    twitter_dir <- '/Volumes/Data 2TB/data/twitter/test/'
    pollution_dir <- '~/google_drive/research/data/pollution/data/raw/hourly/test/'

    ## To make these test files, I used sed:
    ## cd /Volumes/Data\ 2TB/data/twitter/test
    ## sed -n -e '1,10p' ../frequent/2014_03_22_17_usgeo.csv > 2014_03_22_17_usgeo.csv
    ## cd ../../weather
    ## mkdir test
    ## sed -n -e '1,1000p' 201403hourly_proc.csv > test/201403hourly_proc.csv
    ## sed -n -e '1,1000p' 201404hourly_proc.csv > test/201404hourly_proc.csv
}

## Spatial files
ws_file <- 'qclcd_stations'
ps_file <- 'epa_site_list'

## Step 1: Spatial match between twitter data, pollution monitors, and weather stations
## Weather station locations
if( ws_file != 'qclcd_stations'){
    cols <- c(11,9,10,7,34,5,10)
    laf <- laf_open_fwf(paste0(dir,ws_file,'.txt'), column_widths = cols, 
                        column_types=rep("character", length(cols)))
    ws <- laf[,]
    rm(laf)
    names(ws) <- c('id','latitude','longitude','elevation','name','gsn flag','wmo id')
    ws$latitude <- as.numeric(ws$latitude)
    ws$longitude <- as.numeric(ws$longitude)
    ws$elevation <- as.numeric(ws$elevation)
    saveRDS(ws, file=paste0(dir,ws_file,'.Rda'))
    ## Subsetting ws to just US
    ws <- ws[ws$latitude > 25 & ws$latitude < 75
             & ws$longitude < -60,]
} else {
    ws <- read.csv(paste0(dir,ws_file,'.csv'))
}

## Pollution monitor locations
ps <- read.csv(paste0(dir,ps_file,'.csv'))

## Set spatial parameters
sp_ws <- ws
coordinates(sp_ws) <- ~longitude+latitude
sp_ps <- ps
coordinates(sp_ps) <- ~longitude+latitude

## Create the hourly pollution data

p <- read.csv(paste0(pollution_dir,'pollution_hourly.csv'))

########################################################
## Now loop over each twitter file, finding nearest weather station and
## pollution monitor, and merging the data with those locations.
## TO DO:
## . Get hourly weather and pollution data ready for merge.
## . Move twitter data over to external HD and save new data there.
twitter_files <- list.files(twitter_dir)
twitter_files <- twitter_files[1:2]
month_last <- "02"
for( fl in twitter_files ){
    ## Get correct files
    f <- substr(fl, 1,19)
    year <- substr(f, 1, 4)
    month <- substr(f, 6, 7)
    day <- substr(f, 9, 10)
    hour <- substr(f, 12, 13)
    ## Weather
    if( month_last != month ){
        year <- "2014"
        month <- "03"
        w <- read.csv(paste0(weather_dir,year,month,'hourly_proc.csv'))
    }

    ## Pollution
    
    month_last <- month
    
    ## Twitter
    tw <- read.csv(paste0(twitter_dir,f,'.csv'))
    sp_tw <- tw
    coordinates(sp_tw) <- ~geo.coordinates2+geo.coordinates1

    ## Add weather stations
    w_d <- gDistance(sp_ws, sp_tw, byid=TRUE)
    min_w_d <- apply(w_d, 1, function(x) order(x, decreasing=F)[2])
    nearest_s <- cbind(tw, ws[min_w_d,],
                        apply(w_d, 1, function(x) sort(x, decreasing=F)[2]))

    ## Add pollution monitors
    p_d <- gDistance(sp_ps, sp_tw, byid=TRUE)
    min_p_d <- apply(p_d, 1, function(x) order(x, decreasing=F)[2])
    nearest_s <- cbind(nearest_s, ps[min_p_d,],
                       apply(p_d, 1, function(x) sort(x, decreasing=F)[2]))
    colnames(nearest_s) <- c(colnames(tw), 'wban', 'ws_latitude', 'ws_longitude', 'ws_dist','state_code','county_code','sitenum','ps_latitude','ps_longitude','ps_dist')

    ## Merge weather data
    w_hour <- w[
    out <- merge(nearest_s, w_hour, by='wban',all.x=TRUE)
    
    ## Merge pollution data
    out <- merge(out, p_hour, by=c('state_code','county_code','site_num'),all.x=TRUE)
    
    ## Output data
    ## saveRDS(nearest_ws, file=paste0(dir,'county_with_station.Rda'))
    write.csv(out, file=paste0(twitter_dir,f,'_wp.csv'))
}


poll <- read.csv(file=paste0(pollution_dir,'hourly_42101_2015.csv'))


