.onLoad <- function(libname, pkgname) {
    ## NOT USED outside us , NY:=005, PR & USVI  , AP         , pacific    , AS
    OCONUS <- c(00100:00499,          00600:00999, 96200:96699, 96900:96999, 96799)
    wuParameters <- setNames(list(60, # sleep time in seconds after failed scheduling
                                  'http://api.wunderground.com',
                                  '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'),
                             paste0('WUNDERSCRAPER_', c('SLEEP', 'URL', 'WSG84_PROJ')))
    do.call(Sys.setenv, wuParameters)
}
