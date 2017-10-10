#' Set API key
#'
#' Sets the API key to the value of the argument
#'
#' @param key Valid Wunderground API key
#' @examples
#' setApiKey('1a2b3c4d5e6f7g8h9i0j')
#' @return TRUE if setApiKey succeded
#' @export
setApiKey <- function(key) {
    Sys.setenv(WUNDERSCRAPER_KEY=key)
    TRUE
}

.getApiKey <- function() {
    if(identical(key <- Sys.getenv('WUNDERSCRAPER_KEY'), '')) stop('use setApiKey to set key (see ?setApiKey)')
    key
}
