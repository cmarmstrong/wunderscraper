#' Schedules wunderscraper
#'
#' Uses a schedule and counter to schedule sampling and ensure wunderscraper
#' remains within API usage limits.
#'
#' Scheduler is a constructor function that returns a scheduler object for use
#' in a wunderscraper process.  Scheduler has a default counter object, or users
#' may use the \code{\link{counter}} constructor function for more control over
#' API usage limits.
#'
#' Scheduler has methods for managing the schedule: \code{\link{check}},
#'   \code{\link{plan}}, and \code{\link{sync}}.
#' @param counter A \code{\link{counter}} object
#' @return A scheduler object
#' @seealso \code{\link{check.scheduler}}, \code{\link{plan.scheduler}}, \code{\link{sync.scheduler}}
#' @examples
#' scheduler(counter(plan='drizzle'))
#' @export
scheduler <- function(counter=counter()) {
    e <- structure(new.env(), class='scheduler') # use environment for reference semantics
    e $date=format(Sys.Date(), tz='America/New_York')
    e $counter <- counter
    e
}

.schedule <- function(x) UseMethod('schedule')
.scehdule.default <- function(x) warning(paste0('schedule cannot handle class ', class(x)))

## schedule and ensure api calls remain within minute and daily limits
.schedule.scheduler <- function(scheduler) {
    repeat{
        if(scheduler $schedule[1]<Sys.time()) break # wait till start time
        Sys.sleep(SLEEP)
    }
    repeat{
        d <- format(Sys.Date(), tz='America/New_York')
        if(scheduler $date<d) {
            scheduler $counter $n <- 0
            scheduler $date <- d
            sync(scheduler)
        }
        if(scheduler $counter $n<DAILYCOUNT) break # daily limits
        Sys.sleep(SLEEP)
    }
    Sys.sleep(61/MINUTECOUNT) # minute limits
    increment(scheduler $counter)
}
