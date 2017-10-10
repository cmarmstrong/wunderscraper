#' Counter
#'
#' Counts API calls and tracks limits
#'
#' @param plan API usage plan
counter <- function(plan='developer') {
    e <- structure(new.env(), class='counter')
    e $n <- 0
    e $plan <- plan
    ## daily and minute counts
    e $limits <- list(developer=c(500, 10), drizzle=c(5000, 100), shower=c(1e5, 1e3))
    e
}


## generic functions
#' increment counter objects
#'
#' increment counter objects by x, default 1
#'
#' @title increment: the increment function
#' @param x numeric
#' 
increment <- function(x) UseMethod('increment')

## defaults
increment.default <- function(x) warning(paste0('increment cannot handle class ', class(x)))

## methods
## check.counter <- function(counter) ls.str(counter)
increment.counter <- function(counter) counter $n <- counter $n + 1
