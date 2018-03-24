#' Title
#'
#' @param time_div time to convert
#'
#' @importFrom stringr str_match str_replace str_replace_all str_detect
.convert_time <- function(time_div){
  time_div <- str_replace_all(time_div, "\n|\t", "")
  time_div <- str_replace(time_div, "h,(.|\n)+?(?=[0-9])", "h")
  month_names <- c("enero" = "january", "febrero" = "february",
                   "marzo" = "march",
                   "abril" = "april", "mayo" = "may", "junio" = "june",
                   "julio" = "july",
                   "agosto" = "august", "septiembre" = "september",
                   "octubre" = "october", "noviembre" = "november",
                   "diciembre" = "december")
  time_div <- str_replace_all(time_div, month_names)
  if (str_detect(time_div, "24:00"))
    warning(paste0("At midnight the website sometimes gets the time wrong and",
                   " reports a date 24 hours into the future"),
            call. = FALSE)
  time_div <- strptime(time_div, "%H:%M h%d de %B de %Y",
                       tz = "America/Mexico_City")
  as.character(strftime(time_div, "%Y-%m-%d %H:%M:%S"))
}

#' Get the latest pollution values for each station
#'
#' Download the latest hourly values for the pollutants with the highest values
#' for each station as measured in
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
#' @return A data.frame with pollution values in IMECAs, the hour corresponds to
#'   the \emph{America/Mexico_City} timezone (which changes with daylight
#'   saving time)
#' @family IMECA functions
#' @seealso \href{http://www.aire.cdmx.gob.mx/ultima-hora-reporte.php}{Reporte
#'   de calidad del aire}
#' @export
#' @importFrom utils URLdecode
#' @importFrom rvest html_nodes html_text
#' @importFrom xml2 read_html
#' @importFrom stringr str_match str_replace str_replace_all
#' @importFrom httr GET
#'
#' @examples
#' df <- get_latest_imeca()
#' head(df)
get_latest_imeca <- function() {
  url <- "http://www.aire.cdmx.gob.mx/ultima-hora-reporte.php"

  result <- GET(url,  httr::timeout(120))
  if (http_error(result))
    stop("The request to <%s> failed [%s]",
         url,
         status_code(result), call. = FALSE)
  if (http_type(result) != "text/html")
    stop(paste0(url, " did not return text/html", call. = FALSE))

  poll_table <- read_html(result)
  time <- .convert_time(html_text(html_nodes(poll_table, "div#textohora")))

  df <- html_table(html_nodes(poll_table, "table")[[1]], header = TRUE,
                   fill = TRUE)
  names(df) <- c("station_code", "municipio", "quality", "pollutant", "value")
  df <- df[2:nrow(df), ]
  df$value <- lapply(df$value,
                     function(x) URLdecode(str_match(URLdecode(x),
                                                     "'(\\d+)'")[[2]]))

  edomex <- html_table(html_nodes(poll_table, "table")[[2]], header = TRUE,
                       fill = TRUE)
  names(edomex) <- c("station_code", "municipio",
                     "quality", "pollutant", "value")
  edomex <- edomex[2:nrow(edomex), ]
  edomex$value <- lapply(edomex$value,
                         function(x) URLdecode(str_match(URLdecode(x),
                                                         "'(\\d+)'")[[2]]))

  mxc <- rbind(df, edomex)
  mxc$value[mxc$value == "NA"] <- NA
  mxc$value <- as.integer(mxc$value)
  mxc$datetime <- time
  mxc$unit <- "IMECA"
  mxc <- mxc[, c("station_code", "municipio", "quality", "pollutant",
                 "unit", "value", "datetime")]

  mxc[!is.na(mxc$station_code), ]
}
