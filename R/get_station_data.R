recode_unit <- function(pollutant) {
  str_replace_all(pollutant, c("pm2" = "\u00B5g/m\u00B3", "so2" = "ppb", "co" = "ppm",
                               "nox" = "ppb", "no2" = "ppb", "no" = "ppb", "o3" = "ppb",
                               "pm10" = "\u00B5g/m\u00B3", "pm25" = "\u00B5g/m\u00B3",
                               "wsp" = "m/s",
                               "wdr" = "\u00B0",
                               "tmp" = "\u00B0C", "rh" = "%"))
}

recode_pollutant <- function(pollutant) {
  str_replace_all(pollutant, c("pm2" = "PM25", "so2" = "SO2", "co" = "CO",
                               "nox" = "NOX", "no2" = "NO2", "no" = "NO", "o3" = "O3",
                               "pm10" = "PM10", "pm25" = "PM25", "wsp" = "WSP", "wdr" = "WDR",
                               "tmp" = "TMP", "rh" = "RH", "PM2.5" = "PM25"))
}
#' Title
#'
#' @param year year to download
#' @param pollutant pollutant to filter by
#'
#'
#' @importFrom stringr str_c  str_sub str_replace_all
#' @importFrom readr read_csv col_character col_double col_integer
#' @importFrom dplyr filter
#' @importFrom lubridate fast_strptime
#'
.download_old_station_data <- function(pollutant, year) {
  upollutant <- toupper(pollutant)
  if (upollutant == "PM25")
    upollutant <- "PM2.5"
  base_url <- "http://148.243.232.112:8080/opendata/anuales_horarios_gz/contaminantes_"
  df <- read_csv(str_c(base_url, year, ".csv.gz"),
                 skip = 10, progress = FALSE, col_types = list(
                   date = col_character(),
                   cve_station = col_character(),
                   cve_parameter = col_character(),
                   value = col_double(),
                   unit = col_integer()
                 ))

  names(df) <- c("date", "station_code", "pollutant", "value", "unit")
  if (!upollutant %in% unique(df$pollutant)) {
    warning(str_c("No data for '", upollutant, "' in the year of ", year))
    return(data.frame(date = as.Date(character()),
                      hour = character(),
                      station_code = character(),
                      pollutant = character(),
                      value = numeric(),
                      unit = character(),
                      stringsAsFactors = FALSE)
           )
  }
  df <- dplyr::filter(df, pollutant == upollutant)

  df$hour <- as.numeric(str_sub(df$date, 12, 13))
  df$date <- str_c(str_sub(df$date, 7, 10), "-",
                   str_sub(df$date, 4, 5), "-",
                   str_sub(df$date, 1, 2))
  df$value <- as.numeric(df$value)
  df$date <- as.Date(df$date)

  df$unit <- str_replace_all(as.character(df$unit),
                             c("15" = "ppm", "1" = "ppb", "2" = "\u00B5g/m\u00B3"))
  df$pollutant <- recode_pollutant(upollutant)

  df <- df[, c("date", "hour", "station_code", "pollutant", "unit", "value")]
  as.data.frame(df)
}

#' Title
#'
#' @param criterion type of data to download
#' @param pollutant type of pollutant to download
#' @param year year to download
#' @param month month to download
#'
#' @importFrom stringr str_c  str_replace_all
#' @importFrom rvest html_nodes html_table
#' @importFrom xml2 read_html
#' @importFrom dplyr %>%
#' @importFrom lubridate fast_strptime
#' @importFrom httr GET
#'
.download_current_station_data <- function(criterion, pollutant, year, month = "") {
  if (pollutant == "pm25")
    pollutant <- "pm2"
  base_url <- "http://www.aire.cdmx.gob.mx/estadisticas-consultas/concentraciones/respuesta.php?"
  url <- str_c(base_url, "qtipo=", criterion, "&",
               "parametro=", pollutant, "&",
               "anio=", year, "&",
               "qmes=", month)
  poll_table <- read_html(httr::GET(url,  httr::timeout(120)))
  df <- html_table(html_nodes(poll_table, "table")[[1]], header = TRUE)
  names(df) <- df[1, ]
  names(df)[1] <- "date"
  names(df) <- iconv(names(df), from = "UTF-8", to = "ASCII", sub = "")
  names(df) <- str_replace_all(names(df), "\\s", "")
  if (!nrow(df) > 2)
    stop("something went wrong when downloading the data")
  df <- df[2:nrow(df), ]

  df[df == "nr"] <- NA
  df[, 2:ncol(df)] <- apply(df[, 2:ncol(df)], 2, as.numeric)
  # when the data is HORARIOS the second column corresponds to the hour
  if (criterion == "HORARIOS") {
    names(df)[2] <- "hour"
  }
  # The website messed up and changed the station_name of the Montecillo (Texcoco) station
  # to CHA instead of MON
  if ("CHA" %in% names(df)) {
    if (!"MON" %in% names(df)) {
      names(df)[which(names(df) == "CHA")] <- "MON"
    }
  }

  df$date <- str_c(str_sub(df$date, 7, 10), "-",
                   str_sub(df$date, 4, 5), "-",
                   str_sub(df$date, 1, 2))

  df$date <- as.Date(df$date)
  if (criterion != "HORARIOS") {
    val_cols <- base::setdiff(names(df), c("date"))
  } else {
    val_cols <- base::setdiff(names(df), c("date", "hour"))
  }
  df <- gather_(df, "station_code", "value", val_cols)
  df$station_code <- as.character(df$station_code)

  df$unit <- recode_unit(pollutant)
  df$pollutant <- recode_pollutant(pollutant)

  if (criterion != "HORARIOS") {
    df <- df[, c("date", "station_code", "pollutant", "unit", "value")]
  } else {
    df <- df[, c("date", "hour", "station_code", "pollutant", "unit", "value")]
  }

  as.data.frame(df)
}

# Temporary hack (hopefully) to download yearly hourly data by month
download_horario_by_month <- function(pollutant, year){
  df <- data.frame()
  cur_date <- Sys.Date()
  cur_year <- lubridate::year(cur_date)
  cur_month <- lubridate::month(cur_date)


  if (year == cur_year) {
    for (j in 1:cur_month)
      df <- rbind(df,  get_station_single_month(pollutant = pollutant, year, month = j))
  } else {
    for (j in 1:12)
      df <- rbind(df,  get_station_single_month(pollutant = pollutant, year, month = j))
  }
  return(df)
}

#' Title
#'
#' @param criterion type of data to download
#' @param pollutant type of pollutant
#' @param year year to download
#'
#' @importFrom dplyr %>% group_by_ summarise_ ungroup
#'
.download_data <- function(criterion, pollutant, year) {
  year_no_data <- 2005
  if (criterion == "HORARIOS") {
    # Fuck, the website stopped allowing download of HORARIOS yearly data
    # use the old archives before 2015 and use the monthly data after
    if (year > 2015) {
      download_horario_by_month(pollutant, year)
    } else
      .download_old_station_data(pollutant, year)
  } else if (criterion == "MAXIMOS") {
    if (year >= year_no_data) {
      .download_current_station_data(criterion, pollutant, year)
    } else
      .download_old_station_data(pollutant, year) %>%
      group_by_("date", "station_code", "pollutant", "unit") %>%
      summarise_(value = "ifelse(all(is.na(value)),
                               NA,
                               base::max(value, na.rm = TRUE))") %>%
      ungroup()
  } else if (criterion == "MINIMOS") {
    if (year >= year_no_data) {
      .download_current_station_data(criterion, pollutant, year)
    } else
      .download_old_station_data(pollutant, year) %>%
      group_by_("date", "station_code", "pollutant", "unit") %>%
      summarise_(value = "ifelse(all(is.na(value)),
                               NA,
                               base::min(value, na.rm = TRUE))") %>%
      ungroup()
  }
}


#' Download pollution data
#'
#' retrieve pollution data by station from the air quality server at \url{
#' http://www.aire.cdmx.gob.mx/estadisticas-consultas/concentraciones/index.php} for 2016 data.
#' For earlier years the archive files from \url{http://www.aire.cdmx.gob.mx/default.php?opc='aKBhnmI'&opcion=Zg==}
#' are used
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
#'  \item{"PM25"}{ - Particulas menores a 2.5 micrometros (microgramos por metro cubico)}
#'  \item{"WSP"}{ - Velocidad del viento (metros por segundo)}
#'  \item{"WDR"}{ - Direccion del viento (grados)}
#'  \item{"TMP"}{ - Temperatura ambiente (grados Celsius)}
#'  \item{"RH"}{ - Humedad relativa (porcentaje)}
#' }
#' @param year a numeric vector containing the years for which to download data (the earliest possible value is 1986)
#' @param progress Whether to display a progress bar (TRUE or FALSE). By default it will only display in an interactive session.
#'
#' @return a data.frame with pollution data, when downloading "HORARIOS" the hours correspond the
#' GMT+6 timezone
#'
#' @export
#' @importFrom tidyr gather_
#' @importFrom dplyr progress_estimated
#'
#' @examples
#' \dontrun{
#' ## Download daily maximum PM10 data (particulate matter 10 micrometers or less in diameter)
#' ## from 2015 to 2016
#' df <- get_station_data("MAXIMOS", "PM10", 2015:2016)
#' head(df)
#' ## Download ozone concentration hourly data for 2016
#' df2 <- get_station_data("HORARIOS", "O3", 2016)
#' head(df2)
#' }
get_station_data <- function(criterion, pollutant, year, progress = interactive()) {
  year_no_data <- 2005
  if (length(pollutant) > 1)
    stop("You can only download one pollutant at a time")
  stopifnot(criterion %in% c("HORARIOS", "MAXIMOS", "MINIMOS"))
  pollutant <- tolower(pollutant)
  stopifnot(pollutant %in% c("so2", "co", "nox", "no2",
                             "no", "o3", "pm10", "pm25",
                             "wsp", "wdr", "tmp", "rh"))
  if (all(pollutant %in% c("wsp", "wdr", "tmp", "rh") & year < year_no_data))
    stop("WSP, WDR, TMP or RH are only available after 2005. However you can visit <http://www.aire.cdmx.gob.mx/default.php?opc=%27aKBhnmI=%27&opcion=Zw==> to download older data")
  if (min(year) < 1986)
    stop("Data is only available from 1986 onwards")
  if (!is.null(progress))
    p <- progress_estimated(length(year))
  # ll <- mapply(function(criterion, pollutant, year) {pr <<- p;p$tick(); .download_data},
  #              criterion = rep(criterion, length(year)),
  #              pollutant = rep(pollutant, length(year)),
  #              year = year,
  #              SIMPLIFY = FALSE)
  df <- data.frame()
  for (i in year){
    df <- rbind(df,  .download_data(criterion, pollutant, i))
    if (!is.null(progress))
      p$tick()$print()
  }
  as.data.frame(df)
}



#' Download monthly pollution data
#'
#' retrieve hourly averages of pollution data by station from the air quality server at \url{
#' http://www.aire.cdmx.gob.mx/estadisticas-consultas/concentraciones/index.php}
#'
#' @param pollutant The type of pollutant to download.
#' \itemize{
#'  \item{"SO2"}{ - Dioxido de azufre (partes por billon)}
#'  \item{"CO"}{ - Monoxido de carbono (partes por millon)}
#'  \item{"NOX"}{ - Oxidos de nitrogeno (partes por billon)}
#'  \item{"NO2"}{ - Dioxido de nitrogeno (partes por billon)}
#'  \item{"NO"}{ - Oxido nitrico (partes por billon)}
#'  \item{"O3"}{ - Ozono (partes por billon)}
#'  \item{"PM10"}{ - Particulas menores a 10 micrometros (microgramos por metro cubico)}
#'  \item{"PM25"}{ - Particulas menores a 2.5 micrometros (microgramos por metro cubico)}
#'  \item{"WSP"}{ - Velocidad del viento (metros por segundo)}
#'  \item{"WDR"}{ - Direccion del viento (grados)}
#'  \item{"TMP"}{ - Temperatura ambiente (grados Celsius)}
#'  \item{"RH"}{ - Humedad relativa (porcentaje)}
#' }
#' @param year a numeric vector containing the years for which to download data (the earliest possible value is 1986)
#' @param month month number to download
#'
#' @return a data.frame with pollution data, when downloading "HORARIOS" the hours correspond the
#' GMT+6 timezone
#'
#' @export
#' @importFrom stringr str_pad
#' @examples
#' \dontrun{
#' ## Download daily hourly PM10 data (particulate matter 10 micrometers or less in diameter)
#' ## from March 2016
#' df_pm10 <- get_station_single_month("PM10", 2016, 3)
#' head(df_pm10)
#' df_o3 <- get_station_single_month("O3", 2017, 10)
#' head(df_o3)
#' }
get_station_single_month <- function(pollutant, year, month) {
  if ( missing(pollutant) | missing(year) | missing(month))
    stop("arguments missing")
  if (length(pollutant) > 1)
    stop("You can only download one pollutant at a time")
  if (length(year) > 1)
    stop("You can only download one year at a time")
  if (length(month) > 1)
    stop("You can only download one year at a time")
  pollutant <- tolower(pollutant)
  stopifnot(pollutant %in% c("so2", "co", "nox", "no2",
                             "no", "o3", "pm10", "pm25",
                             "wsp", "wdr", "tmp", "rh"))
  month <- str_pad(as.character(month), 2, "left", "0")
  stopifnot(month %in% c("01", "02", "03", "04",
                         "05", "06", "07", "08",
                         "09", "10", "11", "12"))
  year_no_data <- 2005
  if (year < year_no_data)
    stop("Monthly data is only available from 2005 onwards, try instead downloading the data for the entire year")
  .download_current_station_data("HORARIOS", pollutant, year, month)
}
