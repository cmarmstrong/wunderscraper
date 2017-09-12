library(httr)
library(jsonlite)
library(mclust)
library(sf)
library(sp)


DATADIR <- 'json' # file.path('E:', 'data', 'wu')
##          outside us , NY:=005, PR & USVI  , AP         , pacific    , AS
OCONUS <- c(00100:00499,          00600:00999, 96200:96699, 96900:96999, 96799)

DAILYCOUNT <- 500
MINUTECOUNT <- 10
PERIOD <- 7200    # sample period in seconds
SLEEP <- 600      # sleep period in seconds
H <- 5            # start hour

wuKey <- readRDS('resources/key.rds')
wuUrl <- 'http://api.wunderground.com'
wsg84String <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

## zcta <- st_read('resources/cb_2016_us_zcta510_500k')
co <- st_read('resources/cb_2016_us_county_500k')
zctaRel <- read.csv('resources/zcta_county_rel_10.txt', colClasses=c(ZCTA5='character', STATE='character', COUNTY='character', GEOID='character'))
coRel <- zctaRel[!duplicated(zctaRel $GEOID), c('ZCTA5', 'GEOID', 'COPOP')]


## functions
GETjson <- function(url, path) {
    response <- GET(url=url, path=path)
    fromJSON(rawToChar(response $content))
}

WUpath <- function(key, feature, id, format) {
    paste(paste('api', key, feature, 'q', id, sep='/'), format, sep='.')
}

count <- function(counter) { # ensures api calls remain within minute and daily limits
    repeat if(H<strftime(Sys.time(), '%H')) Sys.sleep(600) # wait till start hour
    repeat{
        d <- format(Sys.Date(), tz='America/New_York')
        if(counter $date!=d) {
            counter $count <- 0
            counter $date <- d
        }
        if(counter $count<DAILYCOUNT) break # daily limits
        Sys.sleep(600) # wait 10 minutes
    }
    Sys.sleep(60/MINUTECOUNT)               # minute limits
    counter $count <- counter $count + 1
    counter
}


counter <- list(count=0, date=format(Sys.Date(), tz='America/New_York'))
repeat{
    s <- sample(coRel $GEOID, 1, replace=TRUE, prob=coRel $COPOP)
    ## if(any(s %in% OCONUS)) next
    geolookups <- lapply(zctaRel[zctaRel $GEOID==s, ] $ZCTA5, function(query) {
        counter <- count(counter)
        GETjson(wuUrl, WUpath(wuKey, 'geolookup', query, 'json'))
    })
    geolookups <- lapply(geolookups, function(zcta) {
        if(!is.null(zcta $response $error)) return(NA)
        ## z <- zcta $location $zip
        with(zcta $location $nearby_weather_stations $pws $station,
             st_sf(geometry=st_cast(st_sfc(st_multipoint(matrix(c(lon, lat), ncol=2))), 'POINT'),
                   id=id,
                   stringsAsFactors=FALSE)) # ,
                   ## ZCTA5=z)) # zcta is unecessary
    })
    geolookups <- do.call(rbind, geolookups[!is.na(geolookups)])
    geolookups <- geolookups[!duplicated(geolookups $id), ]
    st_crs(geolookups) <- 4326 ## WU in 4326
    geolookups <- st_transform(geolookups, st_crs(co))
    geolookups <- st_intersection(geolookups, co[co $GEOID==s, ])
    m <- Mclust(st_coordinates(geolookups), modelNames='VII')
    geolookups $cluster <- as.factor(m $classification)
    geolookups $grid <- as.factor(unlist(st_intersects(geolookups, st_make_grid(geolookups, 0.01))))
    geolookups $strata <- with(geolookups, cluster:grid)
    s <- unlist(with(geolookups, tapply(id, strata, sample, 1, simplify=FALSE)))    
    dirname <- file.path(DATADIR, paste0('geoid', s, '-', as.integer(Sys.time())))
    dir.create(dirname)
    for(query in s) {
        counter <- count(counter)
        wuUrn <- WUpath(wuKey, 'conditions', paste('pws', query, sep=':'), 'json')
        write_json(toJSON(GETjson(wuUrl, wuUrn)), file.path(dirname, paste0(query, '.json')))
    }
}

## sampleRunTime <- length(s) * (60/MINUTECOUNT) # time-to-sample estimate
## dailyRemaining <- DAILYCOUNT - counter $count
## strataT <- dailyRemaining / sampleRunTime    
