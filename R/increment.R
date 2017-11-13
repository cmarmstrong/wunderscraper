#' increment counter objects
#'
#' increment counter objects by x, default 1
increment <- function(x) UseMethod('increment')
increment.default <- function(x) warning(paste0('increment cannot handle class ', class(x)))

#' @describeIn increment increment counter by x
#' @param x Numeric
increment.counter <- function(counter, x=1) counter $n <- counter $n + x
