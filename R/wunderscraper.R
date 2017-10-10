## setup
.onLoad <- function(libname, pkgname) {
    ##             outside us , NY:=005, PR & USVI  , AP         , pacific    , AS
    ## OCONUS <- c(00100:00499,          00600:00999, 96200:96699, 96900:96999, 96799)
    Sys.setenv(paste0('WUNDERSCRAPER_', c('SLEEP', 'URL', 'WSG84_PROJ'))
               c(60,                            # sleep time in seconds after failed scheduling
                 'http://api.wunderground.com', # API URL
                 '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
}

## internal functions
.GETjson <- function(url, path) {
    response <- GET(url=url, path=path)
    fromJSON(rawToChar(response $content))
}

.wuPath <- function(key, feature, id, format) {
    paste(paste('api', key, feature, 'q', id, sep='/'), format, sep='.')
}


## main
wunderscraper <- function(scheduler, sampleCo=FALSE, sampleProb='COPOP', sampleStrata='cluster:grid', o='json') {
    ## expose sampling frame
    repeat{
        ## !duplicated(zctaRel $GEOID)
        s <- sample(coRel $GEOID, 1, replace=TRUE, prob=coRel[, sampleProb], drop=TRUE)
        co <- counties()
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
