#' Plan a schedule for executing a task
#'
#' Plans a schedule for executing a task using strptime
#'
#' @seealso \code{\link{strptime}}
#' @export
plan <- function(x, ...) UseMethod('plan')
plan.default <- function(x) warning(paste0('plan cannot handle class ', class(x)))

#' describeIn plan convenience wrapper around seq.POSIXt
#' @export
plan.scheduler <- function(scheduler, ...) {
    scheduler $schedule <- seq(strptime(0, '%H'), strptime(23, '%H'), ...)
}
