#' Scrape wunderground API
#'
#' Uses a sampling strategy to scrape wunderground API.
#'
#' Wunderscraper scrapes wunderground API with a user provided sampling strategy.
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
#'   \item The data in the sampling frame must have a realtionship for at least
#'     one of: zip code, city name, or latitude/longitude; named in the query
#'     parameter.
#'  }
#' This is because these are the units by which the wunderground API queries
#' weather stations within a spatial area.
#'
#' In addition to these requirements and constraints, users can specify sampling
#' weights, strata, and spatial grids to use as stages or strata.
#'
#' Users specify a sampling strategy through a set of vector-valued arguments
#' that indicate the sampling sizes, stages, strata, weights, geometries, and
#' frame.  All sampling parameter vectors are in stage order, from largest scale
#' to the smallest, and must be fully nested--eg zip codes sometimes cross
#' county boundaries, but if county was a previous stage then the zip code
#' sampling stage will be constrained to the geographic boundaries of the
#' counties selected in prior sampling stages.  One of the stages must match the
#' Wunderground lookup value.
#' 
#' Stages before and after Wunderground's lookup may consist of any spatial unit
#' the user deems desirable.  Wunderscraper has built in support for sampling by
#' states, counties, census blocks, and arbitrary spatial grids.  Using
#' administrative boundaries to sample to the step of Wunderground's lookup is
#' convenient due to their large size and, occasionally, coincidence with
#' geographic features.  When sampling stations, after the Wunderground lookup,
#' it's often more useful to use a spatial grid for sampling IDs or strata to
#' ensure the sample acheives sufficient coverage.  Users may also opt for
#' grid-sampling throughout all sample stages, or use smaller vector-based
#' boundaries for sampling from weather stations after Wunderground's lookup--
#' eg streets or neighborhood boundaries.
#'
#' NOTE: is it possible for users to supply their own geometries?
#'
#' Sampling strategies may specify a variable for weighting the sample
#' probabilities.  Wunderscraper provides state and county populations and land
#' areas.
#'
#' The sampling parameter vectors will be padded on the right with NA values to
#' the length of the longest parameter vector.
#'
#' @seealso \code{\link[rwunderground]}
#' @param scheduler A scheduler object.
#' @param sampleSize A vector of integers specifying sample size at each stage.
#'   When missing the top stage is assumed sampling with replacement and
#'   subsequent stages are complete sampling.  If not specified for all stages
#'   then unspecified stages are assumed complete sampling.
#' @param id A vector of strings specifying variable names for cluster ids.  One
#'   of the stages must match the query parameter, and the last stage must be
#'   "id"
#' @param strata A vector of strings specifying variable names for strata.
#' @param query A string specifying the `q' parameter within a Wunderground
#'   geolookup.  The query  must be specified as a stage in the id vector.
#' @param weight A vector of strings specifying numeric variables that indiciate
#'   sampling weights.
#' @param geometry A vector of strings specifying tigris geometry functions;
#'   possible values are states, counties, or blocks.  The geometries will be
#'   available to the next stage.
#' @param cellsize A vector of numerics indicating cellsize for adding grids to
#'   tigris geometries.  A value of NA indicates no grid.
#' @param sampleFrame A dataframe relating the queries, weights, and strata to
#'   each other.
#' @param o A character string indicating output format.  Output to standard out
#'   will always be written in JSON.
#' @return Wunderscraper may output the data directly to a file or to standard
#'   out.  The output can be the JSON payload as recieved from Wunderground, or
#'   converted to a dataframe, with each complete sample comprising one
#'   dataframe, and each dataframe saved in rds format.  Whether in rds or json,
#'   all file output is named by the selected sampling units and date in epoch
#'   time.  Wunderscraper will collect data indefinitely or until it meets a
#'   user specified time or sampling quota.  Wunderscraper returns TRUE if it
#'   finishes normally, else FALSE.
#' @examples
#' \dontrun{
#' wunderscraper(scheduler())
#' }
#' @export
wunderscraper <- function(scheduler, sampleSize=c(1, NA, 1), id=c('GEOID', 'ZCTA5', 'id'), strata=c(NA, NA, 'grid'), query='ZCTA5', weight='COPOP', geometry='county', cellsize=0.01, sampleFrame=zctaRel, o='json') {
    repeat{ # ? if(any(s %in% OCONUS)) next ?
        stations <- .getStations(sampleSize, id, strata, query, weight, geometry, cellsize, sampleFrame)
        dirname <- file.path(DATADIR, paste0(id[1], stations[, id[1]], '-', as.integer(Sys.time())))
        dir.create(dirname)
        repeat{
            for(station in sample(stations)) { # default sample reorders
                schedule(scheduler)
                wuUrn <- .wuPath(getApiKey(), 'conditions', paste('pws', station, sep=':'), 'json')
                write_json(toJSON(.GETjson(Sys.getenv('WUNDERSCRAPER_URL', wuUrn)),
                           file.path(dirname,
                                     paste0(station, '-', as.integer(Sys.time()), '.json')))
            }
            sync(scheduler)
        }
    }
}
