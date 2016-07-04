
#' Title
#'
#' @param pollutant bla
#' @param zone bla
#' @param start_date bla
#' @param end_date bla
#' @param criterion bla
#'
#' @return data.frame
#'
#' @importFrom stringr str_c  str_replace_all
#' @importFrom rvest html_nodes html_table
#' @importFrom lubridate day month year
#' @importFrom httr content
download_zone <- function(criterion, pollutant, zone, start_date, end_date) {
  url = "http://www.aire.cdmx.gob.mx/estadisticas-consultas/consultas/resultado_consulta.php"
  fd <- list(
    diai	= day(start_date),
    mesi	= month(start_date),
    anoi	= year(start_date),
    diaf	= day(end_date),
    mesf	= month(end_date),
    anof	= year(end_date),
    #pollutant = "on",
    #zone = "on",
    Q	= criterion,
    inter	= "",
    consulta = "Consulta"
  )
  pollutant_tmp <- rep("on", length(pollutant))
  names(pollutant_tmp) <- pollutant
  fd <- append(fd, pollutant_tmp)
  zones_tmp <- rep("on", length(zone))
  names(zones_tmp) <- zone
  fd <- append(fd, zones_tmp)

  result <- httr::POST(url,
                       body = fd,
                       encode = "form")
  poll_table <- xml2::read_html(content(result, "text"))

  df <- rvest::html_table(rvest::html_nodes(poll_table, "table")[[1]], header = TRUE)
  df
}

#' Download pollution data by zone
#'
#' retrieve pollution data by geographic zone from the air quality server at \url{http://www.aire.cdmx.gob.mx/default.php?opc='aqBjnmU='}
#'
#' @param pollutant The type of pollutant to download
#' \itemize{
#'  \item{"SO2"}{ - Dioxido de azufre}
#'  \item{"CO"}{ - Monoxido de carbono}
#'  \item{"NO2"}{ - Dioxido de nitrogeno}
#'  \item{"O3"}{ - Ozono}
#'  \item{"PM10"}{ - Particulas menores a 10 micrometros}
#'  \item{"TC"}{- All the pollutants}
#' }
#' @param zone The geographic zone for which to download data
#' \itemize{
#'  \item{"NO"}{ - Noroeste}
#'  \item{"NE"}{ - Noreste}
#'  \item{"CE"}{ - Centro}
#'  \item{"SO"}{ - Suroeste}
#'  \item{"SE"}{ - Sureste}
#'  \item{"TZ"}{ - All the zones}
#' }
#' @param criterion The type of data to download
#' \itemize{
#'  \item{"HORARIOS"}{ - Hourly data}
#'  \item{"MAXIMOS""}{ - Daily maximums}
#' }
#' @param start_date The start date in YYYY-MM-DD format (earliest possible value is 2008-01-01).
#' @param end_date The end date in YYYY-MM-DD format.
#'
#' @return A data.frame with pollution data measured in IMECAS, by geographic zone
#'
#' @export
#' @importFrom stringr str_c  str_replace_all
#' @importFrom rvest html_nodes html_table
#' @importFrom xml2 read_html
#' @importFrom tidyr gather_ separate_
#'
#' @examples
#' \dontrun{
#' # Download daily maximum PM10 data (particulate matter 10 micrometers or less in diameter)
#' # from 2015-01-01 to 2016-03-20 for all geo zones
#' df <- get_zone_data("MAXIMOS", "PM10", "TZ", "2015-01-01", "2016-03-20")
#' head(df)
#' }
#'
get_zone_data <- function(criterion, pollutant, zone, start_date, end_date) {
  if (missing(pollutant))
    stop("You need to specify a contaminante")
  if (missing(zone))
    stop("You need to specify a zona")
  if (missing(criterion))
    stop("You need to specify a start date")
  if (missing(end_date))
    stop("You need to specify an end date (YYYY-MM-DD)")
  if (missing(start_date))
    stop("You need to specify a start date (YYYY-MM-DD)")
  if (start_date < "2008-01-01")
    stop("start_date should be after 2008-01-01")

  # standarize on uppercase since the station api expects upper but zone api expects lower
  criterion <- toupper(criterion)
  stopifnot(length(base::setdiff(pollutant,
                                 c("SO2", "CO", "NO2", "O3", "PM10", "TC"))) == 0)
  stopifnot(length(base::setdiff(zone,
                                 c("NO", "NE", "CE", "SO", "SE", "TZ"))) == 0)
  stopifnot(criterion %in% c("HORARIOS", "MAXIMOS"))
  # the API expects lowercase letters
  criterion <- tolower(criterion)

  # If pollutants are O3 or PM10 issua a warning that the way of calculating the index changed
  if(length(base::intersect(pollutant, c("O3", "PM10", "TZ"))) > 0)
    warning("\n*******************\nStarting October 28, 2014 the IMECA values for O3 and PM10 are computed using NOM-020-SSA1-2014 and NOM-025-SSA1-2014\n*******************")

  df <- download_zone(criterion, pollutant, zone, start_date, end_date)

  names(df) <- df[1,]
  names(df)[1] <- "date"
  names(df) <- str_replace_all(names(df), "\\s", "")
  df <- df[2:nrow(df),]

  # when the data is HORARIOS the second column corresponds to the hour
  if(criterion != tolower("HORARIOS")) {
    value_col = base::setdiff(names(df), "date")
  } else {
    names(df)[2] <- "hour"
    value_col = base::setdiff(names(df), c("date", "hour"))
  }
  df <- df %>%
    gather_("zone_pollutant", "value", value_col) %>%
    separate_("zone_pollutant", c("zone", "pollutant"), sep = 2)
  # Some  values are invalid and to avoid warnings I correct them manually
  df[which(df$value == ""), "value"] <- NA
  df[which(df$value == "M"), "value"] <- NA
  df$value <- as.numeric(df$value)
  df$date <- as.Date(df$date)
  df$unit <- "IMECA"
  if(criterion != tolower("HORARIOS")) {
    as.data.frame(df[ ,c("date", "zone", "pollutant", "unit", "value")])
  } else {
    as.data.frame(df[ ,c("date", "hour", "zone", "pollutant", "unit", "value")])
  }

}


