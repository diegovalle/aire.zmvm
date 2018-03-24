

#' Download pollution data by station in IMECAs
#'
#' Retrieve hourly averages of pollution data, by station, measured in
#' \href{https://en.wikipedia.org/wiki/Índice_Metropolitano_de_la_Calidad_del_Aire}{IMECAs}
#'
#' Note that in 2015 it was determined that the stations with codes ACO, AJU,
#' INN, MON and MPA would no longer be taken into consideration when computing
#' the pollution index because they didn't meet the
#' \href{http://www.aire.cdmx.gob.mx/objetivos-monitoreo-calidad-aire.html}{objectives
#' of monitoring air quality}, and are no longer included in the index, even if
#' they are still part of the SIMAT (Sistema de Monitoreo Atmosférico de la
#' Ciudad de México). Thus, even if they are located inside a zone, they are not
#' included in the pollution values for that zone.
#'
#' @param pollutant The type of pollutant to download
#' \itemize{
#'  \item{"SO2"}{ - Sulfur Dioxide}
#'  \item{"CO"}{ - Carbon Monoxide}
#'  \item{"NO2"}{ - Nitrogen Dioxide}
#'  \item{"O3"}{ - Ozone}
#'  \item{"PM10"}{ - Particulate matter 10 micrometers or less}
#' }
#' @param date The date for which to download data in YYYY-MM-DD format
#' (the earliest possible date is 2009-01-01).
#' @param show_messages show a message about issues with excluded stations
#'
#' @return A data.frame with pollution data measured in IMECAs, by station.
#' The hours correspond to the \emph{Etc/GMT+6} timezone, with no daylight
#' saving time
#' @export
#' @family IMECA functions
#' @seealso \href{http://www.aire.cdmx.gob.mx/default.php?opc='aqBjnmc='}{Índice de calidad del aire por estaciones}
#' @importFrom rvest html_nodes html_table
#' @importFrom xml2 read_html
#' @importFrom tidyr gather
#' @importFrom httr POST http_error status_code http_type
#'
#' @examples
#' ## There was an ozone pollution emergency on May 15, 2017
#' df_o3 <- get_station_imeca("O3", "2017-05-15", show_messages = FALSE)
#'
#' ## Convert to local Mexico City time
#' df_o3$mxc_time <- format(as.POSIXct(paste0(df_o3$date,
#'                                            " ",
#'                                            df_o3$hour,
#'                                            ":00"),
#'                                     tz = "Etc/GMT+6"),
#'                          tz = "America/Mexico_City")
#' head(df_o3[order(-df_o3$value), ])
get_station_imeca <- function(pollutant, date,
                              show_messages = TRUE) {
  if (missing(date))
    stop("You need to specify a start date (YYYY-MM-DD)", call. = FALSE)
  if (length(date) != 1)
    stop("date should be a date in YYYY-MM-DD format", call. = FALSE)
  if (!is.Date(date))
    stop("date should be a date in YYYY-MM-DD format", call. = FALSE)
  if (date < "2009-01-01")
    stop("date should be after 2009-01-01", call. = FALSE)
  if (!(identical("O3", pollutant) || identical("NO2", pollutant) |
      identical("SO2", pollutant) || identical("CO", pollutant) |
      identical("PM10", pollutant)))
     stop("Invalid pollutant value", call. = FALSE)

  if (date >= "2017-01-01" && show_messages)
    message(paste0("Sometime in 2015-2017 the stations with codes",
                   " ACO, AJU, INN, MON, and MPA were excluded from the",
                   " index"))

  url <- "http://www.aire.cdmx.gob.mx/default.php?opc=%27aqBjnmc=%27"
  fd <- list(
    fecha       = date,
    RadioGroup1 = switch(pollutant,
                         "O3" = 0,
                         "NO2" = 1,
                         "SO2" = 2,
                         "CO" = 3,
                         "PM10" = 4),
    aceptar     = "Submit",
    consulta    = 1
  )

  result <- POST(url,
                 body = fd,
                 encode = "form")
  if (http_error(result))
    stop("The request to <%s> failed [%s]",
         url,
         status_code(result), call. = FALSE)
  if (http_type(result) != "text/html")
    stop(paste0(url, " did not return text/html", call. = FALSE))
  poll_table <- xml2::read_html(content(result, "text"))

  df <- rvest::html_table(rvest::html_nodes(poll_table, "table")[[1]],
                          header = TRUE,
                          fill = TRUE)
  if (nrow(df) <= 1)
    stop("The website returned invalid data. Please check the date format.",
         call. = FALSE)
  pollutant2 <- names(df)[3]
  df <- df[, !is.na( df[1, ])]
  names(df) <- df[1, ]
  names(df)[1] <- "date"
  names(df)[2] <- "hour"
  df <- df[2:nrow(df), ]
  df <- gather(df, station_code, value, -date, -hour)
  df[which(df$value == ""), "value"] <- NA
  df$value <- as.numeric(as.character(df$value))
  df$pollutant <- pollutant2
  df$unit <- "IMECA"
  df[, c("date", "hour", "station_code", "pollutant", "unit", "value" )]
}
