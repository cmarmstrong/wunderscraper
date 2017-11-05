#' Scrape wunderground API
#'
#' Uses a sampling strategy to scrape wunderground API.
#'
#' Wunderscraper scrapes wunderground API with a user provided sampling strategy.
#' The sampling strategy has three components:
#' \enumerate{
#'   \item A sampling frame defining the spatial area or units from which a sample
#'     will be taken.  The is defined by the dataframe supplied via the dat
#'     parameter.
#'   \item A possibly multistage sampling strategy for selecting weather stations
#'     within the spatial sampling frame.
#'   \item A set of stratifying factors.
#' }
#'
#' The sampling strategy has one constraint:
#' \enumerate{
#'   \item The data in the dat parameter must have a realtionship for at least
#'     one of: zip code, city name, or latitude/longitude; named in the query
#'     parameter.
#'  }
#' This is because these are the units by which the wunderground API queries
#' weather stations within a spatial area.
#' 
#' Stages before and after Wunderground's lookup may consist of any spatial unit
#' the user deems desirable.  Wunderscraper has built in support for sampling by
#' counties or states prior to the Wunderground lookup, and for sampling by grid
#' and spatial cluster after the Wunderground lookup.  Using administrative
#' boundaries to sample to the step of Wunderground's lookup is convenient due
#' to their large size and, occasionally, coincidence with geographic features.
#' Users may opt for grid-sampling througout all sample stages, or use smaller
#' vector-based boundaries for sampling from weather stations after
#' Wunderground's lookup--eg streets or neighborhood boundaries.
#'
#' Sampling strategies can also use a variable for weighting the sample
#' probabilities.  Wunderscraper provides state and county populations and land
#' areas.  If using a grid-based sampling strategy, then Landscan
#' \url{http://web.ornl.gov/sci/landscan/} or Gridded Population of the World
#' \url{http://sedac.ciesin.columbia.edu/data/collection/gpw-v4} can provide
#' population rasters at about a 1km resolution.
#'
#' @seealso \code{\link[rwunderground]}
#' @param scheduler A scheduler object.
#' @param sampleSize A vector of integers specifying sample size at each stage.
#'   When missing the top stage is assumed sampling with replacement and
#'   subsequent stages are complete sampling.  If not specified for all stages
#'   then unspecified stages are assumed complete sampling.
#' @param id A vector of cluster ids.
#' @param strata A vector of strata
#' @param query The `q' parameter within a Wunderground geolookup.  A query that
#'   is also included in the id parameter will be sampled by strata.
#' @param weight A numeric variable indiciating sampling weights.
#' @param o A character string indicating output format.  Output to standard out
#'   will always be written in JSON.
#' @param geom A function name indicating the tigris function for quering
#'   geometries
#' @param dat A dataframe relating the queries, weights, and strata to each
#'   other.
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
#' wunderscraper(scheduler(), id='GEOID', strata='grid', weight='COPOP', sampleFrame=zctaRel, o='json')
#' query='grid(0.1)', strata=c('STATE')
#' }
#' @export
wunderscraper <- function(scheduler, # a latlon query would not be unlike a grid
                          sampleSize,
                          id='GEOID', strata='grid', query='ZCTA5', weight='COPOP',
                          geometries, sampleFrame=zctaRel, o='json') {
    repeat{
        ## should frames be made st objects from start?
        stations <- getStations(sampleSize, id, strata, query, weight, geometries,
                                sampleFrame)
        ## zctaRel[zctaRel $GEOID==s, ] $ZCTA5

        dirname <- file.path(DATADIR, paste0('geoid', s, '-', as.integer(Sys.time())))
        dir.create(dirname)
        repeat{
            ## stations will be stratified in getStations, not here
            stations <- unlist(with(geolookups,
                                    tapply(id, strata, sample, 1, simplify=FALSE)))
            for(station in sample(stations)) { # default sample reorders
                schedule(scheduler)
                wuUrn <- wuPath(
                    wuKey, 'conditions', paste('pws', station, sep=':'), 'json')
                write_json(toJSON(GETjson(wuUrl, wuUrn)),
                           file.path(dirname,
                                     paste0(station, '-', as.integer(Sys.time()), '.json')))
            }
            sync(scheduler)
            ## sample strat will always be implemented in getStations, not here
            if(sampleCo) break # sample next county
        }
    }
}

## proposed form:
## dat1 <- sample(id[1], strata[1], weight[1], dat)
## dat1q <- queryWU(query, dat1)
## dat2 <- sample(id[2], strata[2], weight[2], dat1q)
## repeat{ station query procedure }
## can specify state for counties, or county for blocs.
## state requires nothing, county state, bloc county?

## two phase design: first phase returns query parameter(s),
##   second phase samples stations
## two functions: one for first phase another for second


## if(any(s %in% OCONUS)) next
## could reuse for both phases.  last stage of phase 1 must be query.  could make this simple multistage
## if set which id variable will be used for querying too, and after that stage do the geolookup, merge
## in the stations, then continue?  only problem: when to do second geometry?
