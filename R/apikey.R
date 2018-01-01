#' Set API key.
#'
#' Sets the API key to the value of the argument.
#'
#' If \code{f} is not missing, then reads the API key from a file, else sets
#' key to the \code{key} parameter.  Does not check if key is valid.
#'
#' @param key A valid Wunderground API key
#' @param f A string indicating a text file containing only an API key
#' @return The API key
#' @examples
#' setApiKey('1a2b3c4d5e6f7g8h9i0j')
#' @export
setApiKey <- function(key, f=NULL) {
    if(!is.null(f)) key <- trimws(readChar(f, file.info(f)$size))
    Sys.setenv(WUNDERSCRAPER_KEY=key)
    key
}

.getApiKey <- function() {
    if(identical(key <- Sys.getenv('WUNDERSCRAPER_KEY'), '')) {
        stop('use setApiKey to set key (see ?setApiKey)')
    }
    key
}
