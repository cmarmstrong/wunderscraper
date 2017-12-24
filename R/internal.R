.onLoad <- function(libname, pkgname) {
    ## NOT USED outside us , NY:=005, PR & USVI  , AP         , pacific    , AS
    OCONUS <- c(00100:00499,          00600:00999, 96200:96699, 96900:96999, 96799)
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

.ringmaster <- function(state=NULL, county=NULL, blocks=FALSE, cb=TRUE, resolution='20m') {
    ## state and county must be either NULL or vector valued.
    if(is.null(state)) {
        geom <- tigris::states(cb=cb, resolution=resolution, class='sf')
        geom $STAREA <- with(geom, ALAND+AWATER)
        geom $STLAND <- geom $ALAND
        geom $STWATER <- geom $AWATER
        geom
    }
    else if(blocks) tigris::blocks(state=state, county=county, class='sf')
    else {
        geom <- tigris::counties(state=state, cb=cb, resolution=resolution, class='sf')
        geom $COAREA <- with(geom, ALAND+AWATER)
        geom $COLAND <- geom $ALAND
        geom $COWATER <- geom $AWATER
        geom
    }
}

.wuPath <- function(key, feature, id, format) {
    paste(paste('api', key, feature, 'q', id, sep='/'), format, sep='.')
}

.wuConditions <- function(station) {
    wuUrn <- .wuPath(.getApiKey(), 'conditions', paste('pws', station, sep=':'), 'json')
    .GETjson(Sys.getenv('WUNDERSCRAPER_URL'), wuUrn)
}

.wuGeolookup <- function(scheduler, queries) {
    ## returns sf with station id and POINT geometry columns
    geolookups <- lapply(unique(queries), function(query) {
        .schedule(scheduler)
        geolookup <- .GETjson(Sys.getenv('WUNDERSCRAPER_URL'),
                              .wuPath(.getApiKey(), 'geolookup', query, 'json'))
        if(!is.null(geolookup $response $error)) return(NA)
        ## convert latlon to sf geometry
        with(geolookup $location $nearby_weather_stations $pws $station,
             sf::st_sf(geometry=sf::st_cast(sf::st_sfc(sf::st_multipoint( # reset indent!
                       matrix(c(lon, lat), ncol=2))), 'POINT'), id=id, stringsAsFactors=FALSE))
    })
    geolookups <- do.call(rbind, geolookups[!is.na(geolookups)])
    geolookups <- geolookups[!duplicated(geolookups $id), ] # remove duplicate stations
    sf::st_crs(geolookups) <- 4326 # WU in 4326
    geolookups
}

.getGeometry <- function(geoid, cellsize, blocks=FALSE) {
    ## TIGER geometries with a factor for grid membership
    states <- substr(geoid, 1, 2)
    counties <- substr(geoid, 3, 5)
    geom <- do.call(.ringmaster, list(states, counties))
    if(!blocks) geom <- geom[geom $COUNTYFP%in%counties, ]
    if(!is.na(cellsize)) {
        if(cellsize<=0) geom $GRID <- 1
        else { # TODO: generate random offset for make_grid
            cells <- sf::st_make_grid(geom, cellsize)
            cells <- sf::st_sf(data.frame(geometry=cells, GRID=1:length(cells)))
            geom <- sf::st_intersection(cells, geom)
        }
    }
    geom $GEOID <- paste0(geom $STATEFP, geom $COUNTYFP) # safety
    geom
}

.getSampleFrame <- function(dfr, id, weight) {
    dfr[, 'id'] <- dfr[, id, drop=TRUE]
    dfr[, 'weight'] <- ifelse(is.na(weight), 1, dfr[, weight])
    sf::st_geometry(dfr) <- NULL # dfr is sf
    dfr[!duplicated(dfr[, id]), c('id', 'weight')]
}

.wuSample <- function(scheduler, id, size, strata, weight, cellsize) {
    ## enact a sampling strategy upon wunderground API
    utils::data('zctaRel') # see data.R
    dfr <- zctaRel
    geom <- .ringmaster() # defaults to state geometries
    geom $GEOID <- NULL # state GEOID == STATEFP
    dfr $GRID <- 1 # initialize GRID and geometry
    dfr <- merge(geom, dfr, by='STATEFP') # merge.sf
    sampleParams <- list(size=size, id=id, strata=strata, weight=weight, cellsize=cellsize)
    nstages <- max(lengths(sampleParams)) # number of sampling stages
    ## error checking
    if(length(id)<nstages-1) stop('id must exist for all stages; last stage equals "id" or nothing')
    else if(length(id)<nstages) sampleParams $id <- c(id, 'id')
    else if(id[nstages]!='id') stop('id of last stage must equal "id" or nothing')
    sampleParams <- lapply(sampleParams, `length<-`, nstages) # args are equal length
    list2env(sampleParams, environment()) # "attach" sampleParams to environment
    for(i in 1:nstages) { # index the arg vectors by i
        sampleFrame <- .getSampleFrame(dfr, id[i], weight[i]) # drops geometry
        if(is.na(size[i])) selection <- sampleFrame $id # complete sampling
        else if(is.na(strata[i])) { # simple sampling
            selection <- with(sampleFrame, sample(id, size[i], prob=weight))
        } else { # stratified sampling
            getStrataFrame <- function(strataFrame) { # .getSampleFrame for each strata
                with(.getSampleFrame(strataFrame, id[i], weight[i]),
                     sample(id, size[i], prob=weight)) # stratified sampling must have size
            }
            selection <- unlist(by(dfr, dfr[, strata[i], drop=TRUE], getStrataFrame))
        }
        dfr <- dfr[dfr[, id[i], drop=TRUE] %in% selection, ] # has geometry
        if(!is.na(cellsize[i])) { # get new geometries and add grids of cellsize
            geom <- with(dfr, .getGeometry(unique(GEOID), cellsize[i]))
            sf::st_geometry(dfr) <- NULL
            dfr <- merge(geom, dfr, by='GEOID', suffixes=c('', paste0('.', i)))
        }
        if(i==nstages-1) { # wunderground geolookup
            geolookups <- .wuGeolookup(scheduler, dfr[, id[i], drop=TRUE])
            geolookups <- sf::st_transform(geolookups, sf::st_crs(dfr))
            dfr <- sf::st_intersection(geolookups, dfr)
        }
    }
    unique(dfr $id)
}

.writeResponse <- function(response, form, o) {
    if(is.na(o)) writeLines(jsonlite::toJSON(response))
    else if(form=='json') {
        response <- jsonlite::toJSON(response)
        fpath <- file.path(o, paste0(response $current_observation $station_id,
                                     '-', as.integer(Sys.time()), '.json'))
        jsonlite::write_json(response, fpath)
    } else if(form=='data.frame') {
        stop('not implemented')
    }
}
