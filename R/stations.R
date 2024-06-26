#' Pollution measuring stations in Mexico City
#'
#' This dataset contains all pollution measuring stations in Mexico City. The
#' station with code SS1 was added manually since it was missing from the
#' official source dataset (its location was found in the
#' \href{http://www.aire.cdmx.gob.mx/descargas/monitoreo/GDF_2015_audit_report_final_v2.pdf}{Audit
#' of Ambient Air Monitoring Stations for the Sistema de Monitoreo Atmosférico
#' de la Ciudad de México}).
#'
#' @format A data frame with 63 rows and 7 variables:
#' \describe{
#'   \item{station_code}{abbreviation of the station}
#'   \item{station_name}{name of the station}
#'   \item{lon}{longitude of the station}
#'   \item{lat}{latitude of the station}
#'   \item{altitude}{altitude of the station}
#'   \item{comment}{comment}
#'   \item{station_id}{id of the station}
#'
#' }
#' @source \samp{http://148.243.232.112:8080/opendata/catalogos/cat_estacion.csv}
#' @examples
#' head(stations)
"stations"
