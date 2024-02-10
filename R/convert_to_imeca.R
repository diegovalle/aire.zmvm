
# http://www.aire.cdmx.gob.mx/descargas/monitoreo/normatividad/NADF-009-AIRE-2017.pdf
pm10_to_imeca_2014 <- function(value){
  if (value >= 0.000 & value <= 40){
    ret <- 1.25 * value
  } else if (value > 40 & value <= 75){
    ret <- 1.4412 * (value - 41) + 51
  } else if ( value > 75 & value <= 214) {
    ret <- 0.3551 * (value - 76) + 101
  } else if (value > 214 & value <= 354) {
    ret <- 0.3525 * (value - 215) + 151
  } else if (value > 354 & value <= 424) {
    ret <- 1.4348 * (value - 355) + 201
  } else if (value > 424 & value <= 504) {
    ret <- 1.2532 * (value - 425) + 301
  } else if (value > 504 & value <= 604) {
    ret <- 1 * (value - 505) + 401
  } else
    ret <- NA
  return(round_away_from_zero(ret))
}

# http://www.aire.cdmx.gob.mx/default.php?opc=%27ZaBhnmI=&dc=%27aQ==
pm25_to_imeca <- function(value){
  if (value >= 0.000 & value <= 12){
    ret <- 4.1667 * value
  } else if (value > 12 & value <= 45){
    ret <- 1.4894 * (value - 12.1) + 51
  } else if ( value > 45 & value <= 97.4) {
    ret <- 0.9369 * (value - 45.1) + 101
  } else if (value > 97.4 & value <= 150.4) {
    ret <- 0.9263 * (value - 97.5) + 151
  } else if (value > 150.4 & value <= 250.4) {
    ret <- 0.9910 * (value - 150.5) + 201
  } else if (value > 250.4 & value <= 350.4) {
    ret <- 0.9910 * (value - 250.5) + 301
  } else if (value > 350.4 & value <= 500.4) {
    ret <- 0.6604 * (value - 350.5) + 401
  } else
    ret <- NA
  return(round_away_from_zero(ret))
}

# http://www.aire.cdmx.gob.mx/descargas/monitoreo/normatividad/NADF-009-AIRE-2017.pdf
o3_to_imeca <- function(value){
  value <- value / 1000
  if (value >= 0.000 & value <= 0.070){
    ret <- 714.29 * value
  } else if (value > 0.070 & value <= 0.095){
    ret <- 2041.67 * (value - 0.071) + 51
  } else if ( value > 0.095 & value <= 0.154) {
    ret <- 844.83 * (value - 0.096) + 101
  } else if (value > 0.154 & value <= 0.204) {
    ret <- 1000 * (value - 0.155) + 151
  } else if (value > 0.204 & value <= 0.404) {
    ret <- 497.49 * (value - 0.205) + 201
  } else if (value > 0.404 & value <= 0.504) {
    ret <- 1000 * (value - 0.405) + 301
  } else if (value > 0.504 & value <= 0.604) {
    ret <- 1000 * (value - 0.505) + 401
  } else
    ret <- NA
  return(round_away_from_zero(ret))
}

# http://www.aire.cdmx.gob.mx/descargas/monitoreo/normatividad/NADF-009-AIRE-2017.pdf
no2_to_imeca <- function(value){
  value <- value / 1000
  if (value >= 0.000 & value <= 0.105){
    ret <- 476.1905 * (value - 0) + 0
  } else if (value > 0.105 & value <= 0.210){
    ret <- 471.1538  * (value - 0.106) + 51
  } else if ( value > 0.210 & value <= 0.430) {
    ret <- 223.7443 * (value - 0.211) + 101
  } else if (value > 0.430 & value <= 0.649) {
    ret <- 224.7706 * (value - 0.431) + 151
  } else if (value > 0.649 & value <= 1.249) {
    ret <- 165.2755 * (value - 0.650) + 201
  } else if (value > 1.249 & value <= 1.649) {
    ret <- 248.1203 * (value - 1.250) + 301
  } else if (value > 1.649 & value <= 2.049) {
    ret <- 248.1203 * (value - 1.650) + 401
  } else
    ret <- NA
  return(round_away_from_zero(ret))
}

# http://www.aire.cdmx.gob.mx/descargas/monitoreo/normatividad/NADF-009-AIRE-2017.pdf
so2_to_imeca <- function(value){
  value <- value / 1000
  if (value >= 0.000 & value <= 0.025 ){
    ret <- 2000.0000 * (value - 0) + 0
  } else if (value > 0.025  & value <= 0.110){
    ret <- 583.3333  * (value - 0.026 ) + 51
  } else if ( value > 0.110 & value <= 0.207) {
    ret <- 510.4167 * (value - 0.111 ) + 101
  } else if (value > 0.207 & value <= 0.304) {
    ret <- 510.4167 * (value - 0.208) + 151
  } else if (value > 0.304  & value <= 0.604) {
    ret <- 331.1037 * (value - 0.305 ) + 201
  } else if (value > 0.604 & value <= 0.804) {
    ret <- 497.4874 * (value - 0.605 ) + 301
  } else if (value > 0.804 & value <= 1.004) {
    ret <- 497.4874 * (value - 0.805) + 401
  } else
    ret <- NA
  return(round_away_from_zero(ret))
}

# http://www.aire.cdmx.gob.mx/descargas/monitoreo/normatividad/NADF-009-AIRE-2017.pdf
co_to_imeca <- function(value){
  if (value >= 0.000 & value <=  5.50){
    ret <- 9.0909 * (value - 0) + 0
  } else if (value > 5.50  & value <= 11.00){
    ret <- 9.0741 * (value - 5.6) + 51
  } else if ( value > 11 & value <= 13.0) {
    ret <- 25.7895 * (value - 11.1) + 101
  } else if (value > 13.0 & value <= 15.4) {
    ret <- 21.3043 * (value - 13.1) + 151
  } else if (value > 15.4 & value <= 30.4) {
    ret <- 6.6443 * (value - 15.5) + 201
  } else if (value > 30.4 & value <= 40.4) {
    ret <- 10.0000 * (value - 30.5) + 301
  } else if (value > 40.4 & value <= 50.4) {
    ret <- 10.0000 * (value - 40.5) + 401
  } else
    ret <- NA
  return(round_away_from_zero(ret))
}


to_imeca <- function(contaminant, value) {
  if (is.na(value))
    return(NA)
  if (value < 0)
    return(NA)
  if (contaminant == "O3") {
    ret <- o3_to_imeca(value)
  }
  if (contaminant == "PM10") {
    ret <- pm10_to_imeca_2014(value)
  }
  if (contaminant == "PM25") {
    ret <- pm25_to_imeca(value)
  }
  if (contaminant == "NO2") {
    ret <- no2_to_imeca(value)
  }
  if (contaminant == "SO2") {
    ret <- so2_to_imeca(value)
  }
  if (contaminant == "CO") {
    ret <- co_to_imeca(value)
  }
  return(ret)
}

#' Convert pollution values to IMECA
#'
#' This function converts pollution running averages in the original
#' units (ppb, µg/m³, etc) to
#' \href{https://en.wikipedia.org/wiki/Índice_Metropolitano_de_la_Calidad_del_Aire}{IMECA}
#'
#' Air quality in Mexico City is reported in IMECAs (Índice Metropolitano de la
#' Calidad del Aire), a dimensionless scale where all pollutants can be
#' compared.
#'
#' Note that each pollutant has different averaging periods (see the arguments
#' section). Because of rounding error results may be off by a couple of points.
#'
#' @seealso For the formulas on how to convert visit:
#'   \href{http://www.aire.cdmx.gob.mx/aire/descargas/monitoreo/normatividad/NADF-009-AIRE-2017.pdf}{AVISO POR EL QUE SE DA A CONOCER EL PROYECTO DE NORMA AMBIENTAL PARA EL DISTRITO FEDERAL}
#'
#' @param pollutant type of pollutant. A vector of one or more of the following
#'   options: \itemize{ \item SO2 - Sulfur Dioxide - ppb (24 hour average)
#'   \item CO - Carbon Monoxide - ppm (8 hour average) \item NO2 - Nitrogen
#'   Dioxide - pbb (1 hour average) \item O3 - Ozone ppb (1 hour average)
#'   \item PM10 - Particulate matter 10 micrometers or less (24 hour
#'   average) \item PM25 - Particulate matter 2.5 micrometers or less (24
#'   hour average) }
#' @param value a numeric vector of values to convert to IMECAs. Note that the
#' concentration of pollutants can be measured in different ways, for NO2, and
#' O3 a 1 hour average is used, for CO, an 8 hour average, and for SO2, PM10
#' and PM25 a 24 hour average is used.
#' @param showWarnings deprecated; you can use the function
#'   \code{\link[base]{suppressWarnings}} instead.
#'
#' @return A vector containing the converted value in IMECAs
#' @export
#' @family convert functions
#' @importFrom stats na.omit
#' @examples
#' ## IMECA is a dimensionless scale that allows for the comparison of
#' ## different pollutants
#' convert_to_imeca(157, "O3")
#' convert_to_imeca(c(450, 350, 250), rep("NO2", 3))
#' ## Since this is PM10 the 80 is supposed to be the 24 hour average
#' convert_to_imeca(80, "PM10")
#'
#' ## warning about recycling elements in a vector
#' convert_to_imeca(c(157, 200), c("O3", "O3"))
#'
#' convert_to_imeca(67, "O3")
#' convert_to_imeca(77, "O3")
#' convert_to_imeca(205, "O3")
#' convert_to_imeca(72, "O3")
#' convert_to_imeca(98, "O3")
#'
convert_to_imeca <- function(value, pollutant, showWarnings = TRUE) {
  if (!missing("showWarnings")) {
    warning(paste0("`showWarnings` argument deprecated. Use the function ",
            "`suppressWarnings` instead."),
            call. = FALSE)
  }
  if (length(pollutant) < 1)
    stop("Invalid pollutant value", call. = FALSE)
  pollutant <- toupper(pollutant)
  for (i in seq_len(length(pollutant)))
    if (!(identical("O3", pollutant[i]) | identical("NO2", pollutant[i]) |
          identical("SO2", pollutant[i]) | identical("CO", pollutant[i]) |
          identical("PM10", pollutant[i]) | identical("PM25", pollutant[i])))
      stop("Invalid pollutant value", call. = FALSE)
  if (length(value) < 1)
    stop("value should be numeric", call. = FALSE)
  if (is.factor(value))
    stop("value should be numeric", call. = FALSE)
  if (!suppressWarnings(!any(is.na(as.numeric(na.omit(value))))))
    stop("value should be numeric", call. = FALSE)
  if (length(value) != length(pollutant)) {
    if ( max(length(value), length(pollutant)) %%
         min(length(value), length(pollutant)) != 0)
      stop("longer argument not a multiple of length of shorter", call. = FALSE)
    warning(paste0("The vectors are of unequal length. Recycling elements ",
                   "of the shorter vector to match the longer vector."),
            call. = FALSE)
  }

  as.vector(unname(mapply(to_imeca, contaminant = pollutant, value = value)))
}
