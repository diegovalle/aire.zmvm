## aire.zmvm-deprecated.r
#' @title Deprecated functions in package \pkg{aire.zmvm}.
#' @description The functions listed below are deprecated and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned. Help pages for deprecated functions are
#'   available at \code{help("<function>-deprecated")}.
#' @name aire.zmvm-deprecated
#' @keywords internal
#' @param ... Parameters to be passed to the modern version of the function
#' @export  get_latest_data get_zone_data
#' @aliases  get_latest_data get_zone_data
#' @section Details:
#' \tabular{rl}{
#'   \code{get_zone_data} \tab now a synonym for \code{\link{get_zone_imeca}}\cr
#'   \code{get_zone_data} \tab now a synonym for \code{\link{get_zone_imeca}}\cr
#' }
#'
get_latest_data <- function(...) {
  .Deprecated("get_latatest_imeca", package="aire.zmvm")
  get_latest_imeca(...)
}
get_zone_data <- function(...) {
  .Deprecated("get_zone_imeca", package="aire.zmvm")
  get_zone_imeca(...)
}
NULL
