#' Counter.
#'
#' Counts API calls and tracks limits
#'
#' Counter is a constructor function that returns a counter object for use by a
#' scheduler object.  The counter uses reference semantics so that multiple
#' schedulers may read and write to the counter object.
#'
#' @param plan API usage plan.  Possible values are developer (500 calls a day 10
#' calls a minute, drizzle (5000 calls a day 100 calls a minute) shower (100000
#' calls a day 1000 a minute), or custom (see parameters day and minute).
#' @param day Custom daily API usage limit.
#' @param minute Custom minute API usage limit.
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

.increment <- function(x) UseMethod('increment')
.increment.default <- function(x) warning(paste0('increment cannot handle class ', class(x)))
.increment.counter <- function(counter, x=1) counter $n <- counter $n + x
