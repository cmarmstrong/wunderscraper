.onLoad <- function(libname, pkgname) {
    ##             outside us , NY:=005, PR & USVI  , AP         , pacific    , AS
    ## OCONUS <- c(00100:00499,          00600:00999, 96200:96699, 96900:96999, 96799)
    Sys.setenv(paste0('WUNDERSCRAPER_', c('SLEEP', 'URL', 'WSG84_PROJ')),
               c(60,                            # sleep time in seconds after failed scheduling
                 'http://api.wunderground.com', # API URL
                 '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
}

.GETjson <- function(url, path) {
    response <- GET(url=url, path=path)
    fromJSON(rawToChar(response $content))
}

.getGeolookup <- function(queries, espg) {
        geolookups <- lapply(queries, function(query) {
            schedule(scheduler)
            geolookup <- GETjson(wuUrl, wuPath(wuKey, 'geolookup', query, 'json'))
            if(!is.null(geolookup $response $error)) return(NA)
            ## zcta <- query $location $zip
            with(geolookup $location $nearby_weather_stations $pws $station,
                 st_sf(geometry=st_cast(st_sfc(st_multipoint(
                           matrix(c(lon, lat), ncol=2))), 'POINT'),
                       id=id,
                       stringsAsFactors=FALSE)) # ,
                       ## ZCTA5=zcta)) # unecessary
        })
        geolookups <- do.call(rbind, geolookups[!is.na(geolookups)])
        geolookups <- geolookups[!duplicated(geolookups $id), ] # remove duplicate stations
        st_crs(geolookups) <- 4326 # WU in 4326
        st_transform(geolookups, espg)
}

.getGeometry <- function(state, county, cellsize) {
    geom <- .getTIGRE(state, county)
    if(cellsize=<0) geom $grid <- 1
    else geom $grid <- as.factor(unlist(st_intersects(geom, st_make_grid(geom, cellsize))))
    geom
}

.getTIGRE <- function(state=NULL, county=NULL, cb=TRUE, resolution=20m) {
    if(is.null(county)) {
        if(is.null(state)) states(cb=cb, resolution=resolution, class='sf')
        else counties(state=state, cb=cb, resolution=resolution, class='sf')
    } else blocks(state=state, county=county) # if !is.null(county) then state cannot be null
}

.getStations <- function(sampleSize, id, strata, query, weight, geometry, cellsize, sampleFrame) {
    sampleParams <- list(sampleSize, id, strata, weight, geometry, cellsize, sampleFrame)
    nstages <- max(lengths(sampleParams)) ## number of sampling stages
    sampleParams <- lapply(sampleParams, `length<-`, nstages) # args equal length
    list2env(sampleParams, environment())
    for(i in nstages) { # index the arg vectors by i
        idFrame <- sampleFrame[!duplicated(sampleFrame[, id[i], drop=TRUE]), ]
        if(is.na(sampleSize[i])) next # complete sampling
        if(is.na(strata[i])) {        # simple sampling
            idSample <- with(idFrame,
                             sample(as.name(id[i]), sampleSize[i],
                                    replace=FALSE, prob=as.name(weight[i])))
            sampleFrame <- sampleFrame[sampleFrame $id%in%idSample, ]
        } else {                   # stratified sampling
            idSample <- tapply(sampleFrame, sampleFrame[, strata[i], drop=TRUE], function(strataFrame) {
                with(strataFrame, sample(as.name(id[i]), sampleSize[i],
                                         replace=FALSE, prob=as.name(weight[i])))})
            sampleFrame <- sampleFrame[sampleFrame $id%in%idSample, ]
        }
        geom <- switch(geometries[i],
                       state=with(sampleFrame, getGeometries(NA, NA, cellsize[i])),
                       county=with(sampleFrame, getGeometries(STATE, NA, cellsize[i])),
                       block=with(sampleFrame, getGeometries(STATE, COUNTY, cellsize[i])))
        sampleFrame <- merge(sampleFrame, geom, by='')
        if(id[i]==query) {
            geolookups <- getGeolookup(sampleFrame[, query], espg=st_crs(geom))
            geolookups <- st_intersection(geolookups, geom)
            sampleFrame <- merge(sampleFrame, geolookups, by='')
        }
    }
    sampleFrame # wunderscraper will use '[['(sampleFrame, query)
}

.wuPath <- function(key, feature, id, format) {
    paste(paste('api', key, feature, 'q', id, sep='/'), format, sep='.')
}
