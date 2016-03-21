

#' Title
#'
#' @param criterion bla
#' @param pollutant bla
#' @param year bla
#'
#' @importFrom stringr str_c  str_replace_all
#' @importFrom rvest html_nodes html_table
#' @importFrom xml2 read_html
#' @importFrom dplyr %>%
#'
clean_aire <- function(criterion, pollutant, year) {
  base_url = "http://www.aire.df.gob.mx/estadisticas-consultas/concentraciones/respuesta.php?"
  url <- str_c(base_url, "qtipo=", criterion, "&",
               "parametro=", pollutant, "&",
               "anio=", year, "&",
               "qmes=")
  poll_table <- read_html(url)
  df <- html_table(html_nodes(poll_table, "table")[[1]], header = TRUE)
  names(df) <- df[1,]
  names(df)[1] <- "date"
  names(df) <- str_replace_all(names(df), "\\s", "")
  df <- df[2:nrow(df),]

  df[df == "nr"] <- NA
  df[,2:ncol(df)] <- apply(df[,2:ncol(df)], 2, as.numeric)
  # when the data is HORARIOS the second column corresponds to the hour
  if(criterion != "HORARIOS") {
    idx = 2
  } else {
    idx = 3
    names(df)[2] <- "hour"
  }
  # df$max <- apply(df[ ,idx:ncol(df)], 1, max, na.rm = TRUE)
  # df$min <- apply(df[ ,idx:ncol(df)], 1, min, na.rm = TRUE)
  # df$mean <- apply(df[ ,idx:ncol(df)], 1, mean, na.rm = TRUE)
  # df$median <- apply(df[ ,idx:ncol(df)], 1, median, na.rm = TRUE)
  return(df)
}

#' Download pollution data
#'
#' retrieve pollution data by station from the air quality server at \url{
#' http://www.aire.df.gob.mx/estadisticas-consultas/concentraciones/index.php}
#'
#' @param criterion Type of data to download.
#' \itemize{
#'  \item{"HORARIOS"}{ - Hourly data}
#'  \item{"MAXIMOS""}{ - Daily maximums}
#'  \item{"MINIMOS"}{ - Daily minimums}
#' }
#' @param pollutant The type of pollutant to download.
#' \itemize{
#'  \item{"SO2"}{ - Dioxido de azufre (partes por billon)}
#'  \item{"CO"}{ - Monoxido de carbono (partes por millon)}
#'  \item{"NOX"}{ - Oxidos de nitrogeno (partes por billon)}
#'  \item{"NO2"}{ - Dioxido de nitrogeno (partes por billon)}
#'  \item{"NO"}{ - Oxido nitrico (partes por billon)}
#'  \item{"O3"}{ - Ozono (partes por billon)}
#'  \item{"PM10"}{ - Particulas menores a 10 micrometros (microgramos por metro cubico)}
#'  \item{"PM2"}{ - Particulas menores a 2.5 micrometros (microgramos por metro cubico)}
#'  \item{"WSP"}{ - Velocidad del viento (metros por segundo)}
#'  \item{"WDR"}{ - Direccion del viento (grados)}
#'  \item{"TMP"}{ - Temperatura ambiente (grados Celsius)}
#'  \item{"RH"}{ - Humedad relativa (porcentaje)}
#' }
#' @param year a numeric vector containing the years for which to download data (the earliest possible value is 2005)
#'
#' @return a data.frame with pollution data
#'
#' @export
#' @importFrom data.table rbindlist
#' @importFrom lubridate dmy
#' @importFrom tidyr gather_
#'
#' @examples
#' \dontrun{
#' # Download daily maximum PM10 data (particulate matter 10 micrometers or less in diameter)
#' # from 2015 to 2016
#' df <- get_station_data("MAXIMOS", "pm10", 2015:2016)
#' head(df)
#' }
get_station_data <- function(criterion, pollutant, year) {
  stopifnot(criterion %in% c("HORARIOS", "MAXIMOS", "MINIMOS"))
  pollutant <- tolower(pollutant)
  stopifnot(pollutant %in% c("so2", "co", "nox", "no2",
                             "no", "o3", "pm10", "pm2",
                             "wsp", "wdr", "tmp", "rh"))
  stopifnot(min(year)>=2005)
  ll <- mapply(clean_aire,
               criterio = rep(criterion, length(year)),
               pollutant = rep(pollutant, length(year)),
               year = year,
               SIMPLIFY = FALSE)
  df <- rbindlist(ll)
  df$date <- as.Date(dmy(df$date))
  if(criterion != "HORARIOS") {
    val_cols <- base::setdiff(names(df), c("date"))
  } else {
    val_cols <- base::setdiff(names(df), c("date", "hour"))
  }
  gather_(df, "station", "value", val_cols)
}

