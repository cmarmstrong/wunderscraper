#' Scrape wunderground API
#'
#' Uses a sampling strategy to scrape wunderground API.
#'
#' Wunderscrape scrapes wunderground API with a user provided sampling strategy.
#' The sampling strategy has two necessary components:
#' \enumerate{
#'   \item A sampling frame defining the spatial area or units from which a sample
#'     will be taken.
#'   \item A possibly multistage sampling strategy for selecting weather stations
#'     within the spatial sampling frame.
#' }
#'
#' The sampling strategy has one constraint:
#' \enumerate{
#'   \item The data in the sampling frame must have a relationship for at least
#'     one of: zip code, city name, or latitude/longitude; named in the query
#'     parameter.
#' }
#' This is because these are the units by which the wunderground API queries
#' weather stations within a spatial area.
#'
#' In addition to these requirements and constraints, users can specify sampling
#' weights, strata, and spatial grids to use as stages or strata.
#'
#' Users specify a sampling strategy through a set of vector-valued arguments
#' that indicate the sampling sizes, stages, strata, weights, geometries, and
#' frame.  All sampling parameter vectors are in stage order, from largest scale
#' to the smallest, and must be fully nested.
#' 
#' Stages before and after Wunderground's lookup may consist of any spatial unit
#' the user deems desirable.  Wunderscraper has built in support for sampling by
#' states, counties, census blocks, and arbitrary spatial grids.  Using
#' administrative boundaries to sample to the step of Wunderground's lookup is
#' convenient due to their large size and, occasionally, coincidence with
#' geographic features.  When sampling stations, after the Wunderground lookup,
#' it's often more useful to use a spatial grid for sampling IDs or strata to
#' ensure the sample acheives sufficient coverage.  Users may also opt for
#' grid-sampling throughout all sample stages.  Wunderscraper uses tigris to
#' fetch state and county geometries during the sampling stages.  The sampling
#' frame must contain columns \code{STATE} and \code{COUNTY} to specify to
#' tigris the appropriate geometries.
#'
#' Sampling strategies may specify a variable for weighting the sample
#' probabilities.  Wunderscraper provides state and county populations and land
#' areas.  See \code{\link{zctaRel}} for details on available weighting
#' variables.
#'
#' The sampling parameter vectors will be padded on the right with NA values to
#' the length of the longest parameter vector.
#'
#' wunderscrape uses the following template for building api queries:
#' \code{http://api.wunderground.com/conditions/q/<query>.json}
#' wunderscrape returns the value of each query, and can either write the raw
#' json to a file or convert each complete sample to a dataframe.
#'
#' @param scheduler A scheduler object.
#' @param sampleSize A vector of integers specifying sample size at each stage.
#'   NA values specify complete sampling.  When missing the top stage is assumed
#'   sampling with replacement and subsequent stages are complete sampling.  If
#'   not specified for all stages then unspecified stages are assumed complete
#'   sampling.
#' @param id A vector of strings specifying variable names for cluster ids.  One
#'   of the stages must match the query parameter, and the last stage must be
#'   "id"
#' @param strata A vector of strings specifying variable names for strata.  NA
#'   values indicate simple sampling.
#' @param query A string specifying the `q' parameter within a Wunderground
#'   geolookup.  The query must also be specified as a stage in the id vector.
#' @param weight A vector of strings specifying variable names for numeric
#'   variables that indiciate sampling weights.  NA values specify unweighted
#'   sampling.
#' @param cellsize A vector of numerics specifying cellsize for adding grids to
#'   TIGER geometries.  TIGER geometries are in the unit of latitude-longitude
#'   degrees.  value of NA specifies no grid.  The grids will be available to
#'   the next stage.
#' @param form A character string specifying output format.  Output to standard out
#'   will always be written in JSON.
#' @param o A character string specifying output file.
#' @return Wunderscrape may output the data directly to a file or to standard
#'   out.  The output can be the JSON payload as recieved from Wunderground, or
#'   converted to a dataframe, with each complete sample comprising one
#'   dataframe, and each dataframe saved in rds format.  Whether in rds or json,
#'   all file output is named by the selected sampling units and date in epoch
#'   time.
#' @seealso \code{\link[rwunderground]{conditions}}
#' @examples
#' \dontrun{
#' wunderscrape(scheduler(counter()))
#' }
#' @export
wunderscrape <- function(scheduler, sampleSize=1, id=c('GEOID', 'ZCTA5', 'id'), strata=c(NA, NA, 'GRID'), query='ZCTA5', weight='COPOP', cellsize=0.01, form='json', o) {
    stations <- .getStations(sampleSize, id, strata, query, weight, cellsize)
    dirname <- file.path(o, paste0(id[1], stations[, id[1]], '-', as.integer(Sys.time())))
    dir.create(dirname)
    for(station in sample(stations)) { # default sample reorders
        .schedule(scheduler)
        wuUrn <- .wuPath(.getApiKey(),
                         'conditions', paste('pws', station, sep=':'), 'json')
        jsonlite::write_json(jsonlite::toJSON(.GETjson(Sys.getenv('WUNDERSCRAPER_URL'), wuUrn)),
                             file.path(dirname, paste0(station, '-', as.integer(Sys.time()), '.json')))
    }
    ## sync(scheduler)
}
