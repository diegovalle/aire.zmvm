#' Deprecated functions in package \pkg{aire.zmvm}.
#'
#' The functions listed below are deprecated and will be defunct in
#' the near future. When possible, alternative functions with similar
#' functionality are also mentioned.
#'
#' @rdname aire.zmvm-deprecated
#' @name aire.zmvm-deprecated
#' @keywords internal
#' @param ... Parameters to be passed to the modern version of the function
#' @docType package
#' @export get_latest_data get_zone_data get_station_single_month
#' @aliases get_latest_data get_zone_data get_station_single_month
#' @usage get_zone_data(...)
#' get_latest_data(...)
#' get_station_single_month(...)
#' @section Details:
#' \tabular{rl}{
#'   \code{get_zone_data} \tab now a synonym for
#'   \code{\link{get_zone_imeca}}\cr
#'   \code{get_latest_data} \tab now a synonym for
#'   \code{\link{get_latest_imeca}}\cr
#'   \code{get_station_single_month} \tab now a synonym for
#'   \code{\link{get_station_month_data}(criterion = "HORARIOS", ...)}\cr
#' }
#'
get_latest_data <- function(...) {
  .Deprecated("get_latest_imeca", package = "aire.zmvm")
  get_latest_imeca(...)
}
get_zone_data <- function(...) {
  .Deprecated("get_zone_imeca", package = "aire.zmvm")
  get_zone_imeca(...)
}
get_station_single_month <- function (...) {
  .Deprecated('get_station_month_data(criterion = "HORARIOS", ...)',
              package = "aire.zmvm")
  get_station_month_data(criterion = "HORARIOS", ...)
}
NULL
