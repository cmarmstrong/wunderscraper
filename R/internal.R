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
