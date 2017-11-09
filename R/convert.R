# http://siga.jalisco.gob.mx/assets/documentos/normatividad/nadf-009-aire-2006.pdf
pm10_to_imeca_2006 <- function(value){
  if (value >= 0.000 & value <= 120){
    ret <- value * 5 / 6
  } else if (value > 120 & value <= 320){
    ret <- 40 + value * .5
  } else if (value > 320) {
    ret <- value * 5 / 8
  }
  return(round(ret))
}

# http://www.aire.cdmx.gob.mx/descargas/monitoreo/normatividad/NADF-009-AIRE-2006.pdf
pm10_to_imeca_2014 <- function(value){
  if (value >= 0.000 & value <= 40){
    ret <- 1.25 * value
  } else if (value > 40 & value <= 75){
    ret <- 1.44 * (value - 41) + 51
  } else if ( value > 75 & value <= 214) {
    ret <- 0.355 * (value - 76) + 101
  } else if (value > 214 & value <= 354) {
    ret <- 0.353 * (value - 215) + 151
  } else if (value > 354) {
    ret <- 0.567 * value
  }
  return(round(ret))
}


pm25_to_imeca_2006 <- function(value){
  if (value >= 0.000 & value <= 15.4){
    ret <- value * 50 / 15.4
  } else if (value > 15.4 & value <= 40.4){
    ret <- 20.5 + value * 49 / 24.9
  } else if ( value > 40.4 & value <= 65.4) {
    ret <- 21.3 + value * 49 / 24.9
  } else if (value > 65.4 & value <= 150.4) {
    ret <- 113.2 + value * 49 / 84.9
  } else if (value > 150.4) {
    ret <- value * 201 / 150.5
  }
  return(round(ret))
}


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
  } else if (value > 0.204) {
    ret <- 982.5 * value
  }
  return(round(ret))
}

no2_to_imeca <- function(value){
  value <- value / 1000
  if (value >= 0.000 & value <= 0.105){
    ret <- value * 50 / 0.105
  } else if (value > 0.105 & value <= 0.210){
    ret <- 1.058 + value * 49 / 0.104
  } else if( value > 0.210 & value <= 0.315) {
    ret <- 1.587 + value * 49 / 0.104
  } else if(value > 0.315 & value <= 0.420) {
    ret <- 2.115 + value * 49 / 0.104
  } else if (value > 0.420) {
    ret <- value*201/0.421
  }
  #ret <- (value * 100)/0.21
  return(round(ret))
}

so2_to_imeca <- function(value){
  if(value < 0)
    return(NA)
  if(is.na(value))
    return(NA)
  value <- value /1000
  ret <- value*100/0.13
  return(round(ret))
}

co_to_imeca <- function(value){
  if (value >= 0.000 & value <=  5.50){
    ret <-value*50/5.50
  } else if (value > 5.50  & value <= 11.00){
    ret <- 1.82+value*49/5.49
  } else if( value > 11 & value <= 16.50) {
    ret <- 2.73+value*49/5.49
  } else if(value > 16.5 & value <= 22.00) {
    ret <- 3.64+value*49/5.49
  } else if (value > 22.00) {
    ret <- value*201/22.01
  }
  return(round(ret))
}

round2 <- function(x, n = 0) {
  posneg <-  sign(x)
  z <- abs(x)*10^n
  z <- z + 0.5
  z <- trunc(z)
  z <- z/10^n
  z * posneg
}

to_imeca <- function(contaminant, value) {
  if(is.na(value))
    return(NA)
  if(value < 0)
    return(NA)
  if (contaminant == "O3") {
    ret <- o3_to_imeca(value)
  }
  if (contaminant == "PM10") {
    ret <- pm10_to_imeca_2014(value)
  }
  if (contaminant == "PM2") {
    ret <- pm25_to_imeca_2006(value)
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

#' Convert pollution values in the original units returned by sensors to
#' IMECA (Indice Metropolitano de la Calidad del Aire), a dimensionless scale
#' where all the pollutants can be compared.
#'
#' See \url{http://rama.edomex.gob.mx/contaminacion-atmosferica/imeca}
#'
#' @param pollutant type of pollutant
#' @param value value to convert to IMECAS
#' @param showWarnings show a warning about issues with performing the conversion
#'
#' @return value in IMECAS
#' @export
#' @examples
#' ## IMECAs are a dimensionless scale that allows for the comparison of
#' ## different pollutants
#' convert_to_imeca(157, "O3", showWarnings = FALSE)
#' convert_to_imeca(c(450, 350, 250), "NO2", showWarnings = FALSE)
#' convert_to_imeca(80, "PM10", showWarnings = FALSE)
#' convert_to_imeca(c(157, 200), "O3", showWarnings = FALSE)
#' convert_to_imeca(48, "O3", showWarnings = FALSE)
#' convert_to_imeca(67, "O3", showWarnings = FALSE)
#' convert_to_imeca(77, "O3", showWarnings = FALSE)
#' convert_to_imeca(205, "O3", showWarnings = FALSE)
#' convert_to_imeca(72, "O3", showWarnings = FALSE)
#' convert_to_imeca(98, "O3", showWarnings = FALSE)
#'
#' ## Should show a warning because the conversion formula is not well
#' ## defined
#' convert_to_imeca(1.5, "CO")
#'
convert_to_imeca <- function(value, pollutant, showWarnings = TRUE) {
  pollutant <- toupper(pollutant)
  stopifnot(pollutant %in% c("O3", "NO2", "PM10", "SO2", "CO"))
  if (showWarnings)
    warning("This function is beta. Converted values don't always match official ones and care should be taken to validate them.")
  as.vector(unname(mapply(to_imeca, contaminant = pollutant, value = value)))
}
