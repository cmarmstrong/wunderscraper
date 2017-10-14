#' Check the contents of an environment
#'
#' Print the contents of an environment; a wrapper around \code{\link{ls.str}}
#'
#' @seealso \code{\link{ls.str}}
#' @export
check <- function(x) UseMethod('check')
check.default <- function(x) warning(paste0('check cannot handle class ', class(x)))

#' @export
check.scheduler <- function(scheduler) ls.str(scheduler)
## check.counter <- function(counter) ls.str(counter)
