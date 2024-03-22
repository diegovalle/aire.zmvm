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
#' @importFrom httr content POST http_error status_code http_type add_headers
#' @importFrom xml2 read_html
#' @keywords internal
#' @noRd
.download_data_zone <- function(criterion, pollutant, zone, start_date,
                                end_date) {
  url <- paste0("http://www.aire.cdmx.gob.mx/",
                "estadisticas-consultas/consultas/resultado_consulta.php")
  fd <- list(
    tipo_attach = "b",
    diai = day(start_date),
    mesi = month(start_date),
    anoi = year(start_date),
    diaf = day(end_date),
    mesf = month(end_date),
    anof = year(end_date),
    #pollutant = "on",
    #zone = "on",
    #`trip-start` = start_date,
    #`trip-end` = end_date,
    Q = criterion,
    inter       = "",
    consulta = "Consulta"
  )
  pollutant_tmp <- rep("on", length(pollutant))
  names(pollutant_tmp) <- pollutant
  fd <- append(fd, pollutant_tmp)
  zones_tmp <- rep("on", length(zone))
  names(zones_tmp) <- zone
  fd <- append(fd, zones_tmp)

  result <- POST(url,
                 add_headers("user-agent" =
                               "https://github.com/diegovalle/aire.zmvm"),
                 body = fd,
                 encode = "form")

  if (http_error(result))
    stop(sprintf("The request to <%s> failed [%s]",
                 url,
                 status_code(result)
    ), call. = FALSE)
  if (http_type(result) != "text/html")
    stop(paste0(url, " did not return text/html", call. = FALSE))

  poll_table <- read_html(content(result, "text"))

  df <- html_table(html_nodes(poll_table, "table")[[1]],
                          header = TRUE)
  df
}


#' Download pollution data by zone in IMECAs
#'
#' Retrieve pollution data in IMECAs by geographic zone from the air quality
#' server at \href{http://www.aire.cdmx.gob.mx/default.php?opc=\%27aqBjnmU=\%27}{Consultas}
#'
#' Note that in 2015 it was determined that the stations with codes ACO, AJU,
#' INN, MON and MPA would no longer be taken into consideration when computing
#' the pollution index because they didn't meet the
#' \href{http://www.aire.cdmx.gob.mx/objetivos-monitoreo-calidad-aire.html}{objectives
#' of monitoring air quality}. They are no longer included in the index, even if
#' they are still part of the SIMAT (Sistema de Monitoreo Atmosférico de la
#' Ciudad de México). Thus, even if they are located inside a zone, they are not
#' included in the pollution values for that zone.
#'
#' The different geographic zones were defined in the
#' \href{http://www.aire.cdmx.gob.mx/descargas/ultima-hora/calidad-aire/pcaa/Gaceta_Oficial_CDMX.pdf}{ Gaceta Oficial de la Ciudad de México}
#' No. 230, 27 de Diciembre de 2016.
#'
#' \strong{Zona Centro}: Benito Juárez,
#' Cuauhtémoc, Iztacalco and Venustiano Carranza.
#'
#' \strong{Zona Noreste}: Gustavo A. Madero, Coacalco de Berriozábal,
#' Chicoloapan, Chimalhuacán,
#' Ecatepec de Morelos, Ixtapaluca, La Paz,
#' Nezahualcóyotl and Tecámac.
#'
#' \strong{Zona Noroeste}: Azcapotzalco,
#' Miguel Hidalgo, Atizapán de Zaragoza, Cuautitlán, Cuautitlán Izcalli,
#' Naucalpan de Juárez, Nicolás
#' Romero, Tlalnepantla de Baz and Tultitlán.
#'
#' \strong{Zona Sureste}: Iztapalapa, Milpa
#' Alta, Tláhuac, Xochimilco, Chalco and Valle de Chalco.
#'
#' \strong{Zona Suroeste}: Álvaro Obregón,
#' Coyoacán, Cuajimalpa, Magdalena Contreras, Tlalpan and Huixquilucan.
#'
#' @param pollutant The type of pollutant to download. One or more of the
#' following options:
#' \itemize{
#'  \item SO2 - Sulfur Dioxide
#'  \item CO - Carbon Monoxide
#'  \item NO2 - Nitrogen Dioxide
#'  \item O3 - Ozone
#'  \item PM10 - Particulate matter 10 micrometers or less
#'  \item TC - All the pollutants
#' }
#' @param zone The geographic zone for which to download data. One or more of
#' the following:
#' \itemize{
#'  \item NO - Noroeste
#'  \item NE - Noreste
#'  \item CE - Centro
#'  \item SO - Suroeste
#'  \item SE - Sureste
#'  \item TZ - All zones
#' }
#' @param criterion The type of data to download. One of the following options:
#' \itemize{
#'  \item HORARIOS - Hourly data
#'  \item MAXIMOS" - Daily maximums
#' }
#' @param start_date The start date in YYYY-MM-DD format (earliest possible
#' value is 2008-01-01).
#' @param end_date The end date in YYYY-MM-DD format.
#' @param showWarnings deprecated; you can use the function
#' \code{\link[base]{suppressWarnings}} instead.
#' @param show_messages show a message about issues with performing the
#' conversion
#'
#' @return A data.frame with pollution data measured in IMECAs, by geographic
#'   zone. The hours correspond to the \emph{Etc/GMT+6} timezone, with no
#'   daylight saving time
#' @family IMECA functions
#' @seealso \code{\link{zones}} a data.frame containing the municipios
#'   belonging to each zone, and
#'   \href{http://www.aire.cdmx.gob.mx/default.php?opc=\%27aqBjnmI=\%27}{Índice de
#'   calidad del aire por zonas}
#' @export
#' @importFrom stringr str_c  str_replace_all
#' @importFrom rvest html_nodes html_table
#' @importFrom xml2 read_html
#' @importFrom tidyr gather separate
#'
#' @examples
#' \donttest{
#' ## There was a regional (NE) PM10 pollution emergency on Jan 6, 2017
#' get_zone_imeca("MAXIMOS", "PM10", "NE", "2017-01-05", "2017-01-08",
#'                show_messages = FALSE)
#'
#' ## There was an ozone pollution emergency on May 15, 2017
#' get_zone_imeca("MAXIMOS", "O3", "TZ", "2017-05-15", "2017-05-15",
#'                show_messages = FALSE)
#'
#'}
#' \dontrun{
#' ## Download daily maximum PM10 data (particulate matter 10 micrometers or
#' ## less in diameter) from 2015-01-01 to 2016-03-20 for all geographic zones
#' df <- get_zone_imeca("MAXIMOS", "PM10", "TZ", "2015-01-01", "2016-03-20")
#' head(df)
#'
#' ## Download hourly O3 pollution data for May 15, 2017. Only the suroeste zone
#' df2 <- get_zone_imeca("HORARIOS", "O3", "SO", "2017-05-15", "2017-05-15")
#'
#' ## Convert to local Mexico City time
#' df2$mxc_time <- format(as.POSIXct(paste0(df2$date, " ", df2$hour, ":00"),
#'                                   tz = "Etc/GMT+6"),
#'                        tz = "America/Mexico_City")
#'
#' head(df2)
#' }
#'
get_zone_imeca <- function(criterion, pollutant, zone, start_date, end_date,
                           showWarnings = TRUE, show_messages = TRUE) {
  if (!missing("showWarnings"))
    warning(paste0("`showWarnings` argument deprecated. Use the function ",
                   "`suppressWarnings` instead."),
            call. = FALSE)
  if (missing(pollutant))
    stop("You need to specify a pollutant", call. = FALSE)
  if (missing(zone))
    stop("You need to specify a zona", call. = FALSE)
  if (missing(criterion))
    stop("You need to specify a start date", call. = FALSE)
  if (missing(end_date))
    stop("You need to specify an end_date (YYYY-MM-DD)", call. = FALSE)
  if (!is.Date(end_date))
    stop("end_ate should be a date in YYYY-MM-DD format", call. = FALSE)
  if (missing(start_date))
    stop("You need to specify a start_date (YYYY-MM-DD)", call. = FALSE)
  if (!is.Date(start_date))
    stop("start_date should be a string in YYYY-MM-DD format", call. = FALSE)
  if (start_date < "2008-01-01")
    stop(paste0("start_date should be after 2008-01-01, but you can visit",
                " http://www.aire.cdmx.gob.mx/",
                "aire/",
                "default.php?opc=%27aKBhnmI=%27&opcion=aw==",
                " to download data going back to 1992"), call. = FALSE)

  # standarize on uppercase since the station api expects upper, but
  # zone api expects lower
  criterion <- toupper(criterion)
  for (i in seq_len(length(pollutant)))
    if (!(identical("O3", pollutant[i]) || identical("NO2", pollutant[i]) ||
          identical("SO2", pollutant[i]) || identical("CO", pollutant[i]) ||
          identical("PM25", pollutant[i]) ||
          identical("PM10", pollutant[i]) || identical("TC", pollutant[i])))
      stop("Invalid pollutant value", call. = FALSE)
  pollutant <- unique(pollutant)
  for (i in seq_len(length(zone)))
    if (!(identical("NO", zone[i]) || identical("NE", zone[i]) ||
          identical("CE", zone[i]) || identical("SO", zone[i]) ||
          identical("SE", zone[i]) || identical("TZ", zone[i]) ))
      stop("zone should be one of 'NO', 'NE', 'CE', 'SO', 'SE', or 'TZ'",
           call. = FALSE)
  zone <- unique(zone)
  if (!(identical("HORARIOS", criterion) || identical("MAXIMOS", criterion)))
    stop("criterion should be 'HORARIOS' or 'MAXIMOS'", call. = FALSE)
  # the API expects lowercase letters
  criterion <- tolower(criterion)

  # If pollutants are O3 or PM10 display a message that the way of calculating
  # the index changed
  if (length(base::intersect(pollutant, c("O3", "PM10"))) > 0 && show_messages)
    message(paste0("Starting October 28, 2014 the IMECA",
                   " values for O3 and PM10 are computed using",
                   " NOM-020-SSA1-2014 and",
                   " NOM-025-SSA1-2014"))
  if (start_date >= "2017-01-01" && show_messages)
    message(paste0("Sometime in 2015-2017 the stations",
                   " ACO, AJU, INN, MON, and MPA were excluded from the",
                   " index"))
  tryCatch({
    df <- .download_data_zone(criterion, pollutant, zone, start_date, end_date)
    names(df) <- as.character(df[1, ])
    names(df)[1] <- "date"
    names(df) <- str_replace_all(names(df), "\\s", "")
    df <- df[2:nrow(df), ]

    # when the data is HORARIOS the second column corresponds to the hour
    if (criterion != tolower("HORARIOS")) {
      df <- df %>%
        gather(zone_pollutant, value, -date) %>%
        separate(zone_pollutant, c("zone", "pollutant"), sep = 2)
    } else {
      names(df)[2] <- "hour"
      df <- df %>%
        gather(zone_pollutant, value, -date, -hour) %>%
        separate(zone_pollutant, c("zone", "pollutant"), sep = 2)
    }
    # Some  values are invalid and to avoid warnings I correct them manually
    if (nrow(df[which(df$value == ""), "value"]) > 0)
      df[which(df$value == ""), "value"] <- NA
    if (nrow(df[which(df$value == "M"), "value"]) > 0)
      df[which(df$value == "M"), "value"] <- NA
    df$value <- as.numeric(df$value)
    df$date <- as.Date(df$date)
    df$unit <- "IMECA"
    names(df)
    df$pollutant <- .recode_pollutant(df$pollutant)
    if (criterion != tolower("HORARIOS")) {
      as.data.frame(df[, c("date", "zone", "pollutant", "unit", "value")])
    } else {
      as.data.frame(df[, c("date", "hour", "zone", "pollutant", "unit", "value")])
    }
  },
  error = function(cond) {
    message("An error occurred downloading data from www.aire.cdmx.gob.mx:")
    message(cond)
    return(NULL)
  }
  )

  }
