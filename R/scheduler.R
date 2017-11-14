#' Schedules wunderscraper
#'
#' Uses a schedule and counter to schedule sampling and ensure wunderscraper
#' remains within API usage limits.
#'
#' Scheduler is a constructor function that returns a scheduler object for use
#' in a wunderscrape process.
#'
#' Scheduler has methods for managing the schedule: \code{\link{plan}}, and
#' \code{\link{sync}}.
#' @param counter A \code{\link{counter}} object
#' @return Returns a scheduler object.
#' @seealso \code{\link{plan.scheduler}}, \code{\link{sync.scheduler}}
#' @examples
#' scheduler(counter(plan='drizzle'))
#' @export
scheduler <- function(counter) {
    e <- structure(new.env(), class='scheduler') # use environment for reference semantics
    e $date=format(Sys.Date(), tz='America/New_York')
    e $counter <- counter
    e
}

.schedule <- function(scheduler) UseMethod('schedule')
.scehdule.default <- function(x) warning(paste0('schedule cannot handle class ', class(x)))

.schedule.scheduler <- function(scheduler) {
    ## schedule and ensure api calls remain within minute and daily limits
    limits <- with(scheduler $counter, limits[[plan]])
    repeat{
        if(scheduler $schedule[1]<Sys.time()) break # wait till start time
        Sys.sleep(Sys.getenv('WUNDERSCRAPER_SLEEP'))
    }
    repeat{
        d <- format(Sys.Date(), tz='America/New_York')
        if(scheduler $date<d) {
            scheduler $counter $n <- 0
            scheduler $date <- d
            sync(scheduler)
        }
        if(scheduler $counter $n<limits[1]) break # daily limits
        Sys.sleep(Sys.getenv('WUNDERSCRAPER_SLEEP'))
    }
    Sys.sleep(61/limits[2]) # minute limits
    .increment(scheduler $counter)
}
