#' Scrape wunderground API
#'
#' Scrape wunderground API with a sampling strategy based on states, counties,
#' zip codes, or a grid.
#'
#' Wunderscrape scrapes wunderground API with a possibly multistage sampling
#' strategy.  The sampling strategy has one constraint: the last stage of the
#' strategy must be: zip code, city name, or latitude/longitude.  Wunderscraper
#' sends the values of the final stage identifier as queries to the wunderground
#' API.  In addition to stages users may specify weights or strata, and may also
#' generate spatial grids to use as stages or strata.
#'
#' Users specify a sampling strategy through a set of vector-valued arguments
#' that indicate the sampling stages, sizes, strata, and weights.  All sampling
#' parameter vectors are in stage order, from the first to the last, and must be
#' fully nested.
#' 
#' Wunderscraper is limited to the following stage and strata identifiers:
#' states, counties, and arbitrary spatial grids; indicated in sampling
#' parameter vectors as \code{"STATEFP"}, \code{"GEOID"}, and \code{"GRID"}
#' respectively.
#'
#' Wunderscraper may use population or land area as a weighting variable.
#' County population and state population are \code{"COPOP"} and \code{"STPOP"}
#' respectively.  Similarly, county and state area are \code{"COAREA"} and
#' \code{"STAREA"}, respectivley, where \code{"COLAND"} and \code{"STLAND"} are
#' land areas without water.  See \code{link{zctaRel}} for more details on
#' available weighting variables.
#'
#' The sampling parameter vectors will be padded on the right with NA values to
#' the length of the longest parameter vector.  NA for all sampling parameters
#' results in a complete unweighted unstratified sample for that stage.
#'
#' wunderscrape uses the following template for building api queries:
#' \code{http://api.wunderground.com/conditions/q/<query>.json}
#' wunderscrape returns the value of each query, and can either write the raw
#' json to a file or convert each complete sample to a dataframe.
#'
#' @param scheduler A scheduler object.
#' @param id A vector of strings specifying variable names for cluster
#'   identifiers.  The unit identifiers of the last stage will also supply the
#'   `q' parameters for Wunderground geolookups.
#' @param size A vector of integers specifying sample size at each stage. NA
#'   values specify complete sampling.  If not specified for all stages then
#'   unspecified stages are assumed complete sampling.
#' @param strata A vector of strings specifying variable names for strata.  NA
#'   values indicate simple sampling.
#' @param weight A vector of strings specifying variable names for numeric
#'   variables that indiciate sampling weights.  NA values specify unweighted
#'   sampling.
#' @param cellsize A vector of numerics specifying cellsize for adding grids to
#'   TIGER geometries.  TIGER geometries are in the unit of latitude-longitude
#'   degrees.  value of NA specifies no grid.  The grids will be available to
#'   the next stage with the identifying variable GRID.
#' @param form A character string specifying output format.  An NA value sends
#'   output to standard out and will always be in JSON format.
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
#' schedulerMMDD <- scheduler(counter())
#' wunderscrape(schedulerMMDD, c("GEOID", "ZCTA5"), size=c(1, NA, 1), strata=c(NA, NA, "GRID"), weight="COPOP", cellsize=c(NA, 0.01))
#' wunderscrape(schedulerMMDD, c("STATEFP", "GRID", "ZCTA5"), size=c(2, 1, 5, 1), strata=c(NA, NA, NA, "GRID"), cellsize=c(1, NA, 0.01))
#' }
#' @export
wunderscrape <- function(scheduler, id, size=NA, strata=NA, weight=NA, cellsize=NA, form='json', o=NA) {
    stations <- .getStations(scheduler, id, size, strata, weight, cellsize)
    if(!is.na(o)) dir.create(o)
    for(station in sample(stations)) { # default sample reorders
        .schedule(scheduler)
        .scrapeOut(form, o, station)
    }
    ## sync(scheduler)
}
