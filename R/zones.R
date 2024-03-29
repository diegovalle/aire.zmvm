#' Pollution zones in Mexico City
#'
#' @description
#' This data set contains the municipios (counties) that make up the 5
#' geographic zones into which Mexico City was divided for the purpose of
#' disseminating information about the
#' \href{https://en.wikipedia.org/wiki/Índice_Metropolitano_de_la_Calidad_del_Aire}{IMECA}.
#'
#'
#' @details
#' Note that in 2015 it was determined that the stations with codes ACO, AJU,
#' INN, MON and MPA would no longer be taken into consideration when computing
#' the pollution index because they didn't meet the
#' \href{http://www.aire.cdmx.gob.mx/aire/objetivos-monitoreo-calidad-aire.html}{objectives
#' of monitoring air quality}, and are no longer included in the index, even if
#' they are still part of the SIMAT (Sistema de Monitoreo Atmosférico de la
#' Ciudad de México). Thus, even if they are located inside a zone, they are not
#' included in the pollution values for that zone.
#'
#' A transparency request was used to determine the zone to which the
#' municipios of Acolman, Texcoco and Atenco belong.
#'
#' @format A data frame with 36 rows and 6 variables:
#' \describe{
#'   \item{region}{INEGI code of the region (state_code + municipio_code)}
#'   \item{state_code}{INEGI code of the state}
#'   \item{state_abbr}{state abbreviation}
#'   \item{municipio_code}{INEGI code of the municipio}
#'   \item{municipio_name}{name of the municipio}
#'   \item{zone}{zone}
#'
#' }
#' @source \href{http://www.aire.cdmx.gob.mx/aire/descargas/ultima-hora/calidad-aire/pcaa/Gaceta_Oficial_CDMX.pdf}{ Gaceta Oficial de la Ciudad de México}
#' No. 230, 27 de Diciembre de 2016, and
#' \emph{Solicitud de Información} FOLIO 0112000033818
#' @examples
#' head(zones)
"zones"
