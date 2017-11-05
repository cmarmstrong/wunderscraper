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

.getGeolookup <- function() {
        geolookups <- lapply(phase1Frame[, query], function(query) {
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
        geolookups <- st_transform(geolookups, st_crs(geom1))
        geolookups <- st_intersection(geolookups, geom1[geom1 $id%in%phase1Frame[, id[[1]][length(id[[1]])]], ])
        ## phase 2
        geom2 <- getGeometry(state[[2]], county[[2]], cellsize[[2]]) # this normally gets grid
        geolookups $grid <- as.factor(unlist(  # geolookups are an st object; should I add grid to it?
            st_intersects(geolookups, st_make_grid(geolookups, 0.01))))

        geolookups $strata <- with(geolookups, eval(parse(text=sampleStrata)))
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

.wuPath <- function(key, feature, id, format) {
    paste(paste('api', key, feature, 'q', id, sep='/'), format, sep='.')
}
