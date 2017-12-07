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

.scrapeOut <- function(form, o, station) {
    wuUrn <- .wuPath(.getApiKey(), 'conditions', paste('pws', station, sep=':'), 'json')
    if(is.na(o)) writeLines(jsonlite::toJSON(.GETjson(Sys.getenv('WUNDERSCRAPER_URL'), wuUrn)))
    else if(form=='json') {
        content <- jsonlite::toJSON(.GETjson(Sys.getenv('WUNDERSCRAPER_URL'), wuUrn))
        fpath <- file.path(o, paste0(station, '-', as.integer(Sys.time()), '.json'))
        jsonlite::write_json(content, fpath)
    } else if(form=='data.frame') {
        stop('not implemented')
    }   
}

.wuPath <- function(key, feature, id, format) {
    paste(paste('api', key, feature, 'q', id, sep='/'), format, sep='.')
}

.getGeolookup <- function(scheduler, queries) {
    ## returns sf with station id and POINT geometry columns
    geolookups <- lapply(unique(queries), function(query) {
        wunderscraper::.schedule(scheduler)
        geolookup <- .GETjson(Sys.getenv('WUNDERSCRAPER_URL'),
                              .wuPath(.getApiKey(), 'geolookup', query, 'json'))
        if(!is.null(geolookup $response $error)) return(NA)
        with(geolookup $location $nearby_weather_stations $pws $station,
             sf::st_sf(geometry=sf::st_cast(sf::st_sfc(sf::st_multipoint( # reset st_sf indent!
                       matrix(c(lon, lat), ncol=2))), 'POINT'), id=id, stringsAsFactors=FALSE))
    })
    geolookups <- do.call(rbind, geolookups[!is.na(geolookups)])
    geolookups <- geolookups[!duplicated(geolookups $id), ] # remove duplicate stations
    sf::st_crs(geolookups) <- 4326 # WU in 4326
    geolookups
}

.getTIGER <- function(state=NULL, county=NULL, blocks=FALSE, cb=TRUE, resolution='20m') {
    ## state and county must be either NULL or vector valued.
    if(is.null(state)) tigris::states(cb=cb, resolution=resolution, class='sf')
    else if(blocks) tigris::blocks(state=state, county=county, class='sf')
    else tigris::counties(state=state, cb=cb, resolution=resolution, class='sf')
}

.getGeometry <- function(state, county, cellsize, blocks=FALSE) {
    ## TIGER geometries with a factor for grid membership
    geom <- .getTIGER(state, county) # TIGER always has STATEFP & COUNTYFP
    if(!blocks) geom <- geom[geom $COUNTYFP%in%county, ]
    if(!is.na(cellsize)) {
        if(cellsize<=0) geom $GRID <- 1
        else {
            geom <- sf::st_intersection(geom, sf::st_make_grid(geom, cellsize))
            geom $GRID <- rownames(geom)
        }
    }
    geom $GEOID <- paste0(geom $STATEFP, geom $COUNTYFP) # safety
    geom
}

.getSampleFrame <- function(sampleFrame, id, weight) {
    sampleFrame[, 'id'] <- sampleFrame[, id, drop=TRUE]
    sampleFrame[, 'weight'] <- ifelse(is.na(weight), 1, sampleFrame[, weight])
    sf::st_geometry(sampleFrame) <- NULL # sampleFrame is sf
    sampleFrame[!duplicated(sampleFrame[, id]), c('id', 'weight')]
}

.getStations <- function(scheduler, id, size, strata, weight, cellsize) {
    sampleFrame <- wunderscraper::zctaRel
    geom <- .getTIGER() # default TIGER state geometries
    geom $GEOID <- NULL # state GEOID == STATEFP
    sampleFrame $GRID <- 1 # initialize GRID and geometry
    sampleFrame <- merge(geom, sampleFrame, by.x='STATEFP', by.y='STATE') # merge.sf
    names(sampleFrame)[names(sampleFrame)=='STATEFP'] <- 'STATE'
    sampleParams <- list(size=size, id=id, strata=strata, weight=weight, cellsize=cellsize)
    nstages <- max(lengths(sampleParams)) # number of sampling stages
    ## error checking
    if(length(id)<nstages-1) stop('id must exist for all stages; last stage equals "id" or nothing')
    else if(length(id)<nstages) sampleParams $id <- c(id, 'id')
    else if(id[nstages]!='id') stop('id of last stage must equal "id" or nothing')
    sampleParams <- lapply(sampleParams, `length<-`, nstages) # args are equal length
    list2env(sampleParams, environment()) # "attach" sampleParams to environment
    for(i in 1:nstages) { # index the arg vectors by i
        browser()
        idFrame <- .getSampleFrame(sampleFrame, id[i], weight[i]) # drops geometry
        if(is.na(size[i])) idSample <- idFrame $id # complete sampling
        else if(is.na(strata[i])) { # simple sampling
            idSample <- with(idFrame, sample(id, size[i], prob=weight))
        } else { # stratified sampling
            getStrataFrame <- function(strataFrame) { # .getSampleFrame for each strata
                with(.getSampleFrame(strataFrame, id[i], weight[i]),
                     sample(id, size[i], prob=weight)) # stratified sampling must have size
            }
            idSample <- by(sampleFrame, sampleFrame[, strata[i], drop=TRUE], getStrataFrame)
        }
        sampleFrame <- sampleFrame[sampleFrame[, id[i], drop=TRUE]%in%idSample, ] # has geometry
        if(!is.na(cellsize[i])) { # add grids of cellsize
            geom <- with(sampleFrame, .getGeometry(unique(STATE), unique(COUNTY), cellsize[i]))
            sampleFrame <- sf::st_intersection(geom, sampleFrame)
        }
        if(i==nstages-1) { # wunderground geolookup
            geolookups <- .getGeolookup(scheduler, sampleFrame[, id[i], drop=TRUE])
            geolookups <- sf::st_transform(geolookups, sf::st_crs(sampleFrame))
            sampleFrame <- sf::st_intersection(geolookups, sampleFrame)
        }
    }
    unique(sampleFrame $id)
}
