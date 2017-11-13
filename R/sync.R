#' Sync scheduler's schedule to current time
#'
#' Syncs scheduler's schedule to current time
#'
#' @seealso \code{\link{Sys.time}}
#' @examples
#' sync(scheduler)
#' @export
sync <- function(x) UseMethod('sync')
sync.default <- function(x) warning(paste0('sync cannot handle class ', class(x)))

#' @describeIn sync Sync scheduler's schedule to current time.
#' @export
sync.scheduler <- function(scheduler) {
    scheduler $now <- Sys.time()
    scheduler $schedule <- with(scheduler, c(schedule[schedule>now], schedule[schedule<=now]))
}
