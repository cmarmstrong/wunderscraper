library(httr)
library(jsonlite)
library(mclust)
library(sf)
library(sp)


DATADIR <- 'json'
##          outside us , NY:=005, PR & USVI  , AP         , pacific    , AS
OCONUS <- c(00100:00499,          00600:00999, 96200:96699, 96900:96999, 96799)

DAILYCOUNT <- 500
MINUTECOUNT <- 10
SLEEP <- 60       # sleep period in seconds

wuKey <- readRDS('resources/key.rds')
wuUrl <- 'http://api.wunderground.com'
wsg84String <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

## zcta <- st_read('resources/cb_2016_us_zcta510_500k')
co <- st_read('resources/cb_2016_us_county_500k')
zctaRel <- read.csv('resources/zcta_county_rel_10.txt', colClasses=c(ZCTA5='character', STATE='character', COUNTY='character', GEOID='character'))
coRel <- zctaRel[!duplicated(zctaRel $GEOID), c('ZCTA5', 'GEOID', 'COPOP', 'COAREA', 'COAREALAND')]


## functions
GETjson <- function(url, path) {
    response <- GET(url=url, path=path)
    fromJSON(rawToChar(response $content))
}

wuPath <- function(key, feature, id, format) {
    paste(paste('api', key, feature, 'q', id, sep='/'), format, sep='.')
}

counter <- function() {
    e <- structure(new.env(), class='counter')
    e $n <- 0
    e
}
    
scheduler <- function(counter=counter()) {
    e <- structure(new.env(), class='scheduler') # use environment for reference semantics
    e $date=format(Sys.Date(), tz='America/New_York')
    e $counter <- counter
    e
}

### generic functions
## scheduler
check <- function(x) UseMethod('check')
plan <- function(x, ...) UseMethod('plan')
sync <- function(x) UseMethod('sync')
schedule <- function(x) UseMethod('schedule')
## counter
increment <- function(x) UseMethod('increment')

### methods
## default methods
default <- function(x) function(y) warning(paste0(x, ' cannot handle class ', class(y)))
check.default <- default('check')
plan.default <- default('plan')
sync.default <- default('sync')
schedule.default <- default('schedule')
increment.default <- default('increment')

## scheduler methods
check.scheduler <- function(scheduler) ls.str(scheduler)
check.counter <- function(counter) ls.str(counter)

plan.scheduler <- function(scheduler, ...) { # convenience wrapper around seq.POSIXt
    scheduler $schedule <- seq(strptime(0, '%H'), strptime(23, '%H'), ...)
}

sync.scheduler <- function(scheduler) {
    scheduler $now <- Sys.time()
    scheduler $schedule <- with(scheduler, c(schedule[schedule>now], schedule[schedule<=now]))
}

schedule.scheduler <- function(scheduler) { # schedule and ensure api calls remain within minute and daily limits
    repeat{
        if(scheduler $schedule[1]<Sys.time()) break # wait till start time
        Sys.sleep(SLEEP)
    }
    repeat{
        d <- format(Sys.Date(), tz='America/New_York')
        if(scheduler $date<d) {
            scheduler $counter $n <- 0
            scheduler $date <- d
            sync(scheduler)
        }
        if(scheduler $counter $n<DAILYCOUNT) break # daily limits
        Sys.sleep(SLEEP)
    }
    Sys.sleep(61/MINUTECOUNT) # minute limits
    increment(scheduler $counter)
}

## counter methods
increment.counter <- function(counter) counter $n <- counter $n + 1


## main
main <- function(scheduler, sampleCo=FALSE, sampleProb='COPOP', sampleStrata='cluster:grid') {
    repeat{
        s <- sample(coRel $GEOID, 1, replace=TRUE, prob=coRel[, sampleProb, drop=TRUE)
        ## if(any(s %in% OCONUS)) next
        geolookups <- lapply(zctaRel[zctaRel $GEOID==s, ] $ZCTA5, function(query) {
            schedule(scheduler)
            GETjson(wuUrl, wuPath(wuKey, 'geolookup', query, 'json'))
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
        geolookups <- geolookups[!duplicated(geolookups $id), ] ## remove duplicate stations
        st_crs(geolookups) <- 4326 ## WU in 4326
        geolookups <- st_transform(geolookups, st_crs(co))
        geolookups <- st_intersection(geolookups, co[co $GEOID==s, ])
        m <- Mclust(st_coordinates(geolookups), modelNames='VII')
        geolookups $cluster <- as.factor(m $classification)
        geolookups $grid <- as.factor(unlist(st_intersects(geolookups, st_make_grid(geolookups, 0.01))))
        geolookups $strata <- with(geolookups, eval(parse(text=sampleStrata)))
        dirname <- file.path(DATADIR, paste0('geoid', s, '-', as.integer(Sys.time())))
        dir.create(dirname)
        repeat{
            stations <- unlist(with(geolookups, tapply(id, strata, sample, 1, simplify=FALSE)))
            for(station in sample(stations)) { # default sample reorders
                schedule(scheduler)
                wuUrn <- wuPath(wuKey, 'conditions', paste('pws', station, sep=':'), 'json')
                write_json(toJSON(GETjson(wuUrl, wuUrn)),
                           file.path(dirname, paste0(station, '-', as.integer(Sys.time()), '.json')))
            }
            sync(scheduler)
            if(sampleCo) break # sample next county
        }
    }
}
