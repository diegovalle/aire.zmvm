#' \code{aire.zmvm} package
#'
#' Tools for downloading data from each of the pollution, wind and temperature
#' measuring stations in the Zona Metropolitana del Valle de México
#' (greater Mexico City).
#'
#' See the README on
#' \href{https://github.com/diegovalle/aire.zmvm#readme}{GitHub}
#'
#' @docType package
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

## Avoid notes of the type:
## `no visible binding for global variable [variable name]`
utils::globalVariables(c("station_code", "value", "hour", "zone_pollutant",
                         "unit"))

