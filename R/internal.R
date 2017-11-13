.onLoad <- function(libname, pkgname) {
    ##             outside us , NY:=005, PR & USVI  , AP         , pacific    , AS
    ## OCONUS <- c(00100:00499,          00600:00999, 96200:96699, 96900:96999, 96799)
    wuParameters <- stats::setNames(list(60, # sleep time in seconds after failed scheduling
                                         'http://api.wunderground.com',
                                         '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'),
                                    paste0('WUNDERSCRAPER_', c('SLEEP', 'URL', 'WSG84_PROJ')))
    do.call(Sys.setenv, wuParameters)
}

.GETjson <- function(url, path) {
    response <- GET(url=url, path=path)
    fromJSON(rawToChar(response $content))
}

.getGeolookup <- function(queries, espg) {
    ## returns sf with station id and POINT geometry columns
    geolookups <- lapply(unique(queries), function(query) {
        schedule(scheduler)
        geolookup <- .GETjson(Sys.getenv('WUNDERSCRAPER_URL'),
                              .wuPath(.getApiKey(), 'geolookup', query, 'json'))
        if(!is.null(geolookup $response $error)) return(NA)
        ## zcta <- query $location $zip
        with(geolookup $location $nearby_weather_stations $pws $station,
             st_sf(geometry=st_cast(st_sfc(st_multipoint(matrix(c(lon, lat), ncol=2))), 'POINT'),
                   id=id, stringsAsFactors=FALSE))
    })
    geolookups <- do.call(rbind, geolookups[!is.na(geolookups)])
    geolookups <- geolookups[!duplicated(geolookups $id), ] # remove duplicate stations
    st_crs(geolookups) <- 4326 # WU in 4326
    st_transform(geolookups, espg)
}

.getGeometry <- function(state, county, cellsize) {
    ## TIGER geometries with a factor for grid membership
    geom <- .getTIGRE(state, county)
    if(!is.na(cellsize)) st_geometry(geom) <- st_union(geom, st_make_grid(geom, cellsize))
    ## if(cellsize=<0) geom $grid <- 1
    ## else geom $grid <- as.factor(unlist(st_intersects(geom, st_make_grid(geom, cellsize))))
    geom
}

.getTIGER <- function(state=NULL, county=NULL, cb=TRUE, resolution='20m') {
    ## state and county must be either NULL or vector valued.
    if(is.null(county)) {
        if(is.null(state)) states(cb=cb, resolution=resolution, class='sf')
        else counties(state=state, cb=cb, resolution=resolution, class='sf')
    } else blocks(state=state, county=county) # if !is.null(county) state cannot be null
}

.getStations <- function(sampleSize, id, strata, query, weight, geometry, cellsize, sampleFrame) {
    geom <- .getTIGER() # defaults to states
    sampleParams <- list(sampleSize, id, strata, weight, geometry, cellsize, sampleFrame)
    nstages <- max(lengths(sampleParams)) # number of sampling stages
    sampleParams <- lapply(sampleParams, `length<-`, nstages) # args are equal length
    list2env(sampleParams, environment()) # "attach" sampleParams to environment
    for(i in nstages) { # index the arg vectors by i
        idFrame <- sampleFrame[!duplicated(sampleFrame[, id[i]]), ]
        if(is.na(sampleSize[i])) idSample <- unique(idFrame $id) # complete sampling
        else if(is.na(strata[i])) { # simple sampling
            idSample <- with(idFrame,
                             sample(as.name(id[i]), sampleSize[i], # as.name with idFrame
                                    replace=FALSE, prob=as.name(weight[i])))
        } else { # stratified sampling
            idSample <- unlist(tapply(sampleFrame, sampleFrame[, strata[i], drop=TRUE],
                               function(strataFrame) {
                                   with(strataFrame,
                                        sample(as.name(id[i]), sampleSize[i],
                                               replace=FALSE, prob=as.name(weight[i])))}))
        }
        sampleFrame <- sampleFrame[sampleFrame $id%in%idSample, ]
        geom <- switch(geometries[i],
                       county=with(sampleFrame, .getGeometry(STATE, NA, cellsize[i])),
                       block=with(sampleFrame, .getGeometry(STATE, COUNTY, cellsize[i])),
                       `NA`=geom)
        sampleFrame <- merge(geom, sampleFrame, by='GEOID') # state GEOID == STATEFP
        if(id[i]==query) {
            geolookups <- .getGeolookup(sampleFrame[, query, drop=TRUE], espg=st_crs(geom))
            geolookups <- st_intersection(geolookups, geom)
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
