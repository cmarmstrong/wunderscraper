scheduler <- function(counter=counter()) {
    e <- structure(new.env(), class='scheduler') # use environment for reference semantics
    e $date=format(Sys.Date(), tz='America/New_York')
    e $counter <- counter
    e
}

### generic functions
check <- function(x) UseMethod('check')
plan <- function(x, ...) UseMethod('plan')
sync <- function(x) UseMethod('sync')
schedule <- function(x) UseMethod('schedule')

## defaults
default <- function(x) function(y) warning(paste0(x, ' cannot handle class ', class(y)))
check.default <- default('check')
plan.default <- default('plan')
sync.default <- default('sync')
schedule.default <- default('schedule')

## methods
check.scheduler <- function(scheduler) ls.str(scheduler)

plan.scheduler <- function(scheduler, ...) { # convenience wrapper around seq.POSIXt
    scheduler $schedule <- seq(strptime(0, '%H'), strptime(23, '%H'), ...)
}

sync.scheduler <- function(scheduler) {
    scheduler $now <- Sys.time()
    scheduler $schedule <- with(scheduler, c(schedule[schedule>now], schedule[schedule<=now]))
}

schedule.scheduler <- function(scheduler) { # schedule and ensure api calls remain within minute and daily limits
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
