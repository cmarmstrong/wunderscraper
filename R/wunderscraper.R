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


phase1 <- function(sampleSize, id, strata, weight, sampleFrame) { # returns query values
    sampleParams <- list(sampleSize, id, strata, weight) # arg vectors must be equal length
    sampleParams <- lapply(sampleParams, `length<-`, max(lengths(sampleParams)))
    sampleFrame <- as.environment(sampleFrame) # data with referenc semantics
    ## imperative not functional, maybe rewrite with loop to be explicit
    mapply(function(sampleSize, id, strata, weight) {
        idFrame <- as.environment(eapply(sampleFrame, '[', !duplicated(sampleFrame $id)))
        if(is.na(sampleSize)) return(idFrame $id) # complete sampling
        if(is.na(strata)) {                       # simple sampling
            idSample <- with(idFrame, sample(id, sampleSize, replace=FALSE, prob=weight))
            sampleFrame <- as.environment(eapply(sampleFrame, '[', # define [.envFrame?
                                                 sampleFrame $id%in%idSample $id))
            return(idSample $id)
        } # else (implied return)
        idSample <- tapply(sampleFrame, sampleFrame $strata, function(strataFrame) {
            sample(strataFrame, sampleSize, replace=FALSE, prob=strataFrame $weight)})
        sampleFrame <- as.environment(eapply(sampleFrame, '[',
                                             sampleFrame $id%in%idSample $id))
        return(idSample $id)
    }, c(sampleParams, sampleFrame))
    as.data.frame(sampleFrame) # wunderscraper will use '[['(sampleFrame, query)
}

## two problems to solve in specifying sample desing:
##   1. getting geometries
##   2. getting stations
## solutions:
##   specify station lookup as a stage in the sample id vector
##   use a multiphase syntax with a list of two lists containing the sample parameters
##   for before and after the station lookup
## dl geometries once for each phase
##   after dl'ing geom, create sf object from relationship table and geometries?
##   the conditions for downloading are set in wunderscraper signature?
##     and these are if dl'ing states, counties, or blocs?
##     states immediately, counties after a single state identified in relations table
##     blocs after single county identified in relations table
## when are grids or other geometric features or clusters generated?
##   grids can be generated as soon as geometries are available.
##   so, I need a function for getting the geometries that also adds grids and other stuff?
##   will get geometries once for each phase (or not if set not to?)
## similar to when does function get geometries, when does it get stations with geolookup?
##   the query must happen after geometries dl'ed?
##   yes, because all the queries, zip code lat lon or city name, are all sub county, so
##   will identify a county and thus meet the condition for dl'ing blocs, the smallest geometry.
##   what about zip codes or cities that cross bloc, county, or even state lines?
##   should there be if id==query then send sample results to wunderlookup to get stations?
##     if so, should this process also be stratafied as the other sampling?
##     (divide into strata then sample within each strata?  must avoid redundant geolookups,
##     so this process might have to be a little different implementation than other sampling steps)
getTIGRE <- function(state=NULL, county=NULL, cb=TRUE, resolution=20m) {
    if(is.null(county)) {
        if(is.null(state)) states(cb=cb, resolution=resolution)
        else counties(state=state, cb=cb, resolution=resolution)
    } else blocks(state=state, county=county) # if !is.null(county) then state cannot be null
}

getGeometry <- function() {
    geom <- getTIGRE()
    ## add grids
}


wunderscraper <- function(scheduler, ## a latlon query would not be unlike a grid
                          sampleSize,
                          id='GEOID', strata='grid', query='ZCTA5', weight='COPOP',
                          sampleFrame=zctaRel, geometries=counties, o='json') {
    repeat{
        ## !duplicated(zctaRel $GEOID)
        ## how to handle multiple sample stages?
        ## sampling stages loop that checks if a unique state or county are identified by
        ## sampling and get TIGRE geometry when ready.  After getting geometries do any
        ## grid stuff and continue sampling.
        s <- sample(sampleFrame, 1, replace=TRUE, prob=sampleFrame[, weight], drop=TRUE)
        geom <- st_tigris(state=substr(s $GEOID, 1, 2), cb=TRUE, resolution='20m')
        ## if(any(s %in% OCONUS)) next
        geolookups <- lapply(zctaRel[zctaRel $GEOID==s, ] $ZCTA5, function(query) {
            schedule(scheduler)
            GETjson(wuUrl, wuPath(wuKey, 'geolookup', query, 'json'))
        })
        geolookups <- lapply(geolookups, function(zcta) {
            if(!is.null(zcta $response $error)) return(NA)
            ## z <- zcta $location $zip
            with(zcta $location $nearby_weather_stations $pws $station,
                 st_sf(geometry=st_cast(st_sfc(st_multipoint(
                           matrix(c(lon, lat), ncol=2))), 'POINT'),
                       id=id,
                       stringsAsFactors=FALSE)) # ,
                       ## ZCTA5=z)) # zcta is unecessary
        })
        geolookups <- do.call(rbind, geolookups[!is.na(geolookups)])
        geolookups <- geolookups[!duplicated(geolookups $id), ] ## remove duplicate stations
        st_crs(geolookups) <- 4326 ## WU in 4326
        geolookups <- st_transform(geolookups, st_crs(co))
        geolookups <- st_intersection(geolookups, co[co $GEOID==s, ])
        m <- Mclust(st_coordinates(geolookups), modelNames='VII')
        geolookups $cluster <- as.factor(m $classification)
        geolookups $grid <- as.factor(unlist(
            st_intersects(geolookups, st_make_grid(geolookups, 0.01))))
        geolookups $strata <- with(geolookups, eval(parse(text=sampleStrata)))
        dirname <- file.path(DATADIR, paste0('geoid', s, '-', as.integer(Sys.time())))
        dir.create(dirname)
        repeat{
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
            if(sampleCo) break # sample next county
        }
    }
}
