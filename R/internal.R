.onLoad <- function(libname, pkgname) {
    ##             outside us , NY:=005, PR & USVI  , AP         , pacific    , AS
    ## OCONUS <- c(00100:00499,          00600:00999, 96200:96699, 96900:96999, 96799)
    wuParameters <- setNames(list(60, # sleep time in seconds after failed scheduling
                                  'http://api.wunderground.com',
                                  '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'),
                             paste0('WUNDERSCRAPER_', c('SLEEP', 'URL', 'WSG84_PROJ')))
    do.call(Sys.setenv, wuParameters)
}

.GETjson <- function(url, path) {
    response <- httr::GET(url=url, path=path)
    jsonlite::fromJSON(rawToChar(response $content))
}

.getGeolookup <- function(queries, espg) {
    ## returns sf with station id and POINT geometry columns
    geolookups <- lapply(unique(queries), function(query) {
        .schedule(scheduler)
        geolookup <- .GETjson(Sys.getenv('WUNDERSCRAPER_URL'),
                              .wuPath(.getApiKey(), 'geolookup', query, 'json'))
        if(!is.null(geolookup $response $error)) return(NA)
        ## zcta <- query $location $zip
        with(geolookup $location $nearby_weather_stations $pws $station,
             sf::st_sf(geometry=sf::st_cast(sf::st_sfc(sf::st_multipoint(
                                                               matrix(c(lon, lat), ncol=2))), 'POINT'),
                   id=id, stringsAsFactors=FALSE))
    })
    geolookups <- do.call(rbind, geolookups[!is.na(geolookups)])
    geolookups <- geolookups[!duplicated(geolookups $id), ] # remove duplicate stations
    sf::st_crs(geolookups) <- 4326 # WU in 4326
    sf::st_transform(geolookups, espg)
}

.getTIGER <- function(state=NULL, county=NULL, cb=TRUE, resolution='20m') {
    ## state and county must be either NULL or vector valued.
    if(is.null(county)) {
        if(is.null(state)) tigris::states(cb=cb, resolution=resolution, class='sf')
        else tigris::counties(state=state, cb=cb, resolution=resolution, class='sf')
    } else tigris::blocks(state=state, county=county) # if !is.null(county) state cannot be null
}

.getGeometry <- function(state, county, cellsize) {
    ## TIGER geometries with a factor for grid membership
    geom <- .getTIGER(state, county)
    if(!is.na(cellsize)) sf::st_geometry(geom) <- sf::st_union(geom, sf::st_make_grid(geom, cellsize))
    ## if(cellsize=<0) geom $grid <- 1
    ## else geom $grid <- as.factor(unlist(st_intersects(geom, st_make_grid(geom, cellsize))))
    geom
}

.getStations <- function(sampleSize, id, strata, query, weight, geometry, cellsize, sampleFrame) {
    browser()
    geom <- .getTIGER() # defaults to states
    sampleParams <- list(sampleSize=sampleSize, id=id, strata=strata,
                         weight=weight, geometry=geometry, cellsize=cellsize)
    nstages <- max(lengths(sampleParams)) # number of sampling stages
    sampleParams <- lapply(sampleParams, `length<-`, nstages) # args are equal length
    list2env(sampleParams, environment()) # "attach" sampleParams to environment
    for(i in 1:nstages) { # index the arg vectors by i
        idFrame <- sampleFrame[!duplicated(sampleFrame[, id[i]]), ]
        if(is.na(sampleSize[i])) idSample <- unique(idFrame $id) # complete sampling
        else if(is.na(strata[i])) { # simple sampling
            idSample <- sample(idFrame[, id[i]], sampleSize[i],
                               replace=FALSE, prob=idFrame[, weight[i]])
        } else { # stratified sampling
            idSample <- unlist(tapply(sampleFrame, sampleFrame[, strata[i], drop=TRUE],
                               function(strataFrame) {
                                   with(strataFrame,
                                        sample(as.name(id[i]), sampleSize[i],
                                               replace=FALSE, prob=as.name(weight[i])))}))
        }
        sampleFrame <- sampleFrame[sampleFrame[, id[i]]%in%idSample, ]
        ## need to get state and county from id?  when id is not GEOID, then what?
        ## when id is GEOID, get STATE and COUNTY and save them?
        geom <- switch(geometry[i],
                       county=with(sampleFrame, .getGeometry(unique(STATE), NA, cellsize[i])),
                       block={
                           fipsFrame <- unique(sampleFrame[, c('COUNTY', 'STATE')])
                           with(fipsFrame, .getGeometry(STATE, COUNTY, cellsize[i]))
                       },
                       `NA`=geom)
        sampleFrame <- merge(geom, sampleFrame, by='GEOID') # state GEOID == STATEFP
        if(id[i]==query) {
            geolookups <- .getGeolookup(sampleFrame[, query, drop=TRUE], espg=sf::st_crs(geom))
            geolookups <- sf::st_intersection(geolookups, geom)
            geolookups $query <- query
            ## intersection with grid?
            sampleFrame <- merge(geolookups, sampleFrame, by.x='query', by.y=as.name(query))
        }
    }
    sampleFrame # wunderscraper will use '[['(sampleFrame, query)
}

.wuPath <- function(key, feature, id, format) {
    paste(paste('api', key, feature, 'q', id, sep='/'), format, sep='.')
}
