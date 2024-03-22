single_to_index <- function(value, pollutant) {
  breakup <- function(value, breaks){
    cut(value,
        c(0, 51, 101, 151, 201, 301, Inf),
        c("BUENA", "REGULAR", "MALA", "MUY MALA",
          "EXTREMADAMENTE MALA", "PELIGROSA"),
        right = FALSE)
  }
  if (is.na(value))
    return(NA_character_)
  ret <- switch(pollutant,
         PM10  = breakup(convert_to_imeca(value, pollutant)),
         PM25 = breakup(convert_to_imeca(value, pollutant)),
         SO2   = breakup(convert_to_imeca(value, pollutant)),
         NO2   = breakup(convert_to_imeca(value, pollutant)),
         O3    = breakup(convert_to_imeca(value, pollutant)),
         CO    = breakup(convert_to_imeca(value, pollutant)),
         NA
  )
  as.character(ret)
}


#' Convert a pollutant concentration to its air quality category
#'
#' This functions converts a pollutant value in its original units into one of the 5
#' categories used by the Mexican government to communicate to the public how
#' polluted the air currently is and its health risks.
#'
#' @param value a numeric vector of values to convert to index
#' @param pollutant type of pollutant. A vector of one or more of the following
#'   options: \itemize{ \item SO2 - Sulfur Dioxide - ppb (24 hour average)
#'   \item CO - Carbon Monoxide - ppm (8 hour average) \item NO2 - Nitrogen
#'   Dioxide - pbb (1 hour average) \item O3 - Ozone ppb (1 hour average)
#'   \item PM10 - Particulate matter 10 micrometers or less (24 hour
#'   average) \item PM25 - Particulate matter 2.5 micrometers or less (24
#'   hour average) }
#'
#' @return the IMECA value of the concentration indexed into 5 categories
#' \itemize{
#' \item BUENA - Good: 0-50 minimal health risk
#' \item REGULAR - Regular: 51-100 moderate health effects
#' \item MALA - Bad: 101-150 sensitive groups may suffer adverse heatlh effects
#' \item MUY MALA - Very Bad: 151-200 everyone can experience negative health effects
#' \item EXTREMADAMENTE MALA - Extremely Bad: > 200 serious health issues
#' }
#'
#' @export
#' @seealso \href{http://www.aire.cdmx.gob.mx/descargas/monitoreo/normatividad/NADF-009-AIRE-2006.pdf}{NADF-009-AIRE-2006}
#'
#' @family convert functions
#' @examples
#' convert_to_index(c(12.1, 215, 355), c("PM25", "PM10", "PM10"))
convert_to_index <- function(value, pollutant) {
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

  mapply(single_to_index, value, pollutant)
}
