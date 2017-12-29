#' Counter.
#'
#' Counts API calls and tracks limits
#'
#' Counter is a constructor function that returns a counter object for use by a
#' scheduler object.  The counter uses reference semantics so that multiple
#' schedulers may read and write to the counter object.
#'
#' @return Returns a counter object.
#' @examples
#' counter(plan='drizzle')
#' @export
counter <- function(plan='developer', day=NA, minute=NA) {
    e <- structure(new.env(), class='counter')
    e $n <- 0
    e $plan <- plan
    ## daily and minute counts
    e $limits <- list(developer=c(500, 10), drizzle=c(5000, 100), shower=c(1e5, 1e3),
                      custom=c(day, minute))
    e
}

.increment <- function(x) UseMethod('.increment')
.increment.default <- function(x) warning(paste0('.increment cannot handle class ', class(x)))
.increment.counter <- function(counter, x=1) counter $n <- counter $n + x
