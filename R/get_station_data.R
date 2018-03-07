

recode_unit <- function(pollutant) {
  str_replace_all(pollutant, c("pm2" = "\u00B5g/m\u00B3",
                               "so2" = "ppb",
                               "co" = "ppm",
                               "nox" = "ppb",
                               "no2" = "ppb",
                               "no" = "ppb",
                               "o3" = "ppb",
                               "pm10" = "\u00B5g/m\u00B3",
                               "pm25" = "\u00B5g/m\u00B3",
                               "wsp" = "m/s",
                               "wdr" = "\u00B0",
                               "tmp" = "\u00B0C",
                               "rh" = "%"))
}

recode_pollutant <- function(pollutant) {
  str_replace_all(pollutant, c("pm2" = "PM25",
                               "so2" = "SO2",
                               "co" = "CO",
                               "nox" = "NOX",
                               "no2" = "NO2",
                               "no" = "NO",
                               "o3" = "O3",
                               "pm10" = "PM10",
                               "pm25" = "PM25",
                               "wsp" = "WSP",
                               "wdr" = "WDR",
                               "tmp" = "TMP",
                               "rh" = "RH",
                               "PM2.5" = "PM25"))
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
#' @keywords internal
#'
.download_old_station_data <- function(pollutant, year) {
  upollutant <- toupper(pollutant)
  if (upollutant == "PM25")
    upollutant <- "PM2.5"
  base_url <- paste0("http://148.243.232.112:8080/",
                     "opendata/anuales_horarios_gz/contaminantes_")
  if (upollutant %in% c("WSP", "WDR", "TMP", "RH"))
    base_url <- paste0("http://148.243.232.112:8080/",
                       "opendata/anuales_horarios_gz/meteorolog%C3%ADa_")
  ## The files from 2012 onwards changed the name of the columns
  ## cve_station and cve_parameter to id_station and id_parameter
  if (year >= 2012)
    df <- read_csv(str_c(base_url, year, ".csv.gz"),
                   skip = 10, progress = FALSE, col_types = list(
                     date = col_character(),
                     id_station = col_character(),
                     id_parameter = col_character(),
                     value = col_double(),
                     unit = col_integer()
                   ))
  else
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
                             c("15" = "ppm",
                               "1" = "ppb",
                               "2" = "\u00B5g/m\u00B3",
                               "3" = "m/s",
                               "4" = "\u00B0",
                               "5" = "\u00B0C",
                               "6" = "%"
                                 ))
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
#' @importFrom lubridate fast_strptime month
#' @importFrom httr GET
#' @importFrom tidyr gather
#' @keywords internal
#'
.download_current_station_data <- function(criterion, pollutant, year,
                                           month = "") {
  if (pollutant == "pm25")
    pollutant <- "pm2"
  base_url <- paste0("http://www.aire.cdmx.gob.mx/",
                     "estadisticas-consultas/concentraciones/respuesta.php?")
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
  # The website sometimes messes up and changes the station_code of the
  # Montecillo (Texcoco) station to CHA instead of MON
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
    df <- gather(df, station_code, value, -date)
  } else {
    df <- gather(df, station_code, value, -date, -hour)
  }
  df$station_code <- as.character(df$station_code)

  df$unit <- recode_unit(pollutant)
  df$pollutant <- recode_pollutant(pollutant)

  if (criterion != "HORARIOS") {
    df <- df[, c("date", "station_code", "pollutant", "unit", "value")]
    # For some reason when the criterion is MAXIMOS or MINIMOS the website
    # returns the month we asked for, plus the rest of the year. subset
    if ( (month %in% c("01", "02", "03", "04",
                      "05", "06", "07", "08",
                      "09", "10", "11", "12")) )
      df <- dplyr::filter(df, month(date) == as.numeric(month))
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
      df <- rbind(df,  get_station_month_data("HORARIOS",
                                              pollutant = toupper(pollutant),
                                              year, month = j))
  } else {
    for (j in 1:12)
      df <- rbind(df,  get_station_month_data("HORARIOS",
                                              pollutant = toupper(pollutant),
                                              year, month = j))
  }
  return(df)
}

#' Title
#'
#' @param criterion type of data to download
#' @param pollutant type of pollutant
#' @param year year to download
#'
#' @importFrom dplyr %>% group_by summarise ungroup
#' @keywords internal
#'
.download_data <- function(criterion, pollutant, year) {
  year_not_to_use_archives <- 2018
  ## The old archive data files include decimal points for WSP and TMP
  ## but not the web form. Be sure to use the web form only for recent data
  if (toupper(pollutant) == "WSP" || toupper(pollutant) == "TMP")
    year_no_minmax_data <- 2018
  else
    year_no_minmax_data <- 2005
  ## Fuck, the website stopped allowing download of HORARIOS yearly data
  ## use the old archives before 2017 and use the monthly data after
  if (criterion == "HORARIOS") {
    if (year >= year_not_to_use_archives) {
      download_horario_by_month(pollutant, year)
    } else
      .download_old_station_data(pollutant, year)
  } else if (criterion == "MAXIMOS") {
    if (year >= year_no_minmax_data) {
      .download_current_station_data(criterion, pollutant, year)
    } else
      .download_old_station_data(pollutant, year) %>%
      group_by(date, station_code, pollutant, unit) %>%
      summarise(value = ifelse(all(is.na(value)),
                               NA,
                               base::max(value, na.rm = TRUE))) %>%
      ungroup()
  } else if (criterion == "MINIMOS") {
    if (year >= year_no_minmax_data) {
      .download_current_station_data(criterion, pollutant, year)
    } else
      .download_old_station_data(pollutant, year) %>%
      group_by(date, station_code, pollutant, unit) %>%
      summarise(value = ifelse(all(is.na(value)),
                               NA,
                               base::min(value, na.rm = TRUE))) %>%
      ungroup()
  }
}


#' Download pollution data by station
#'
#' retrieve pollution data by station in the original units from the air quality
#' server at
#' \url{http://www.aire.cdmx.gob.mx/estadisticas-consultas/concentraciones/index.php}
#' or for earlier years use the archive files from
#' \url{http://www.aire.cdmx.gob.mx/default.php?opc='aKBhnmI'&opcion=Zg==} or
#' \url{http://www.aire.cdmx.gob.mx/default.php?opc='aKBhnmI='&opcion=Zw==} for
#' meteorological data.
#'
#' For wind speed (WSP) and temperature (TMP) archive
#' values are correct to one decimal place, but the most recent data is rounded
#' to the nearest integer.
#'
#' @param criterion Type of data to download.
#' \itemize{
#'  \item{"HORARIOS"}{ - Hourly data}
#'  \item{"MAXIMOS""}{ - Daily maximums}
#'  \item{"MINIMOS"}{ - Daily minimums}
#' }
#' @param pollutant The type of pollutant to download.
#' \itemize{
#'  \item{"SO2"}{ - Sulfur Dioxide (parts per billion)}
#'  \item{"CO"}{ - Carbon Monoxide (parts per million)}
#'  \item{"NOX"}{ - Nitrogen Oxides (parts per billion)}
#'  \item{"NO2"}{ - Nitrogen Dioxide (parts per billion)}
#'  \item{"NO"}{ - Nitric Oxide (parts per billion)}
#'  \item{"O3"}{ - Ozone (parts per billion)}
#'  \item{"PM10"}{ - Particulate matter 10 micrometers or less
#'  (micrograms per cubic meter)}
#'  \item{"PM25"}{ - Particulate matter 2.5 micrometers or less
#'  (micrograms per cubic meter)}
#'  \item{"WSP"}{ - Wind velocity (meters per second)}
#'  \item{"WDR"}{ - Wind direction (degrees)}
#'  \item{"TMP"}{ - Temperature (degrees Celsius)}
#'  \item{"RH"}{ - Relative humidity (percentage)}
#' }
#' @param year a numeric vector containing the years for which to download data
#' (the earliest possible value is 1986)
#' @param progress Whether to display a progress bar (TRUE or FALSE).
#' By default it will only display in an interactive session.
#'
#' @return a data.frame with pollution data. When downloading "HORARIOS" the
#' hours correspond to the
#' \emph{Etc/GMT+6} timezone, with no daylight saving time
#'
#' @export
#' @family raw data functions
#' @seealso \code{\link{stations}} for a data.frame with the location and names
#' of all polution measuring stations,
#' @importFrom progress progress_bar
#'
#' @examples
#' \dontrun{
#' ## Download daily maximum PM10 data (particulate matter 10 micrometers or
#' ## less in diameter) from 2015 to 2016
#' df <- get_station_data("MAXIMOS", "PM10", 2015:2016)
#' head(df)
#'
#' ## Download ozone concentration hourly data for 2016
#' df2 <- get_station_data("HORARIOS", "O3", 2016)
#'
#' ## Convert to local Mexico City time
#' df2$mxc_time <- format(as.POSIXct(paste0(df2$date, " ", df2$hour, ":00"),
#'                                   tz = "Etc/GMT+6"),
#'                        tz = "America/Mexico_City")
#' head(df2)
#' }
get_station_data <- function(criterion, pollutant, year,
                             progress = interactive()) {
  if (!(identical("HORARIOS", criterion) || identical("MAXIMOS", criterion) |
        identical("MINIMOS", criterion)))
    stop("criterion should be 'HORARIOS', 'MINIMOS', or 'MAXIMOS'")
  if (!(identical("O3", pollutant) || identical("NO2", pollutant) |
        identical("SO2", pollutant) || identical("CO", pollutant) |
        identical("PM10", pollutant) || identical("WSP", pollutant) |
        identical("WDR", pollutant) || identical("TMP", pollutant) |
        identical("NOX", pollutant) || identical("NO", pollutant) |
        identical("PM25", pollutant) || identical("RH", pollutant)))
    stop("Invalid pollutant value")
  ## Check the year argument is an integer or vector of integers
  if (length(year) < 1)
    stop("year should be an integer in YYYY format")
  for (i in seq_len(length(year)))
    if (is.integer2(year[i]) == FALSE)
      stop("year should be an integer in YYYY format")
  if (min(year) < 1986)
    stop("Data is only available from 1986 onwards")

  pollutant <- tolower(pollutant)

  if (identical(progress, TRUE) &&  length(year) > 1) {
    p <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                          total = length(year))
    p$tick(0)
  }
  df <- data.frame()
  for (i in year){
    df <- rbind(df, .download_data(criterion, pollutant, i))
    if (identical(progress, TRUE) &&  length(year) > 1)
      p$tick()
  }
  as.data.frame(df)
}

#' Download monthly pollution data
#'
#' retrieve hourly averages, daily maximums or daily minimums of pollution data
#' in the original units, by station, from the air quality server at
#' \url{http://www.aire.cdmx.gob.mx/estadisticas-consultas/concentraciones/index.php}
#'
#' The values for wind speed (WSP) and temperature (TMP) are rounded to the
#' nearest integer, but the \code{\link{get_station_data}} function allows you
#' to download data accurate to one decimal point.
#'
#' @param criterion Type of data to download.
#' \itemize{
#'  \item{"HORARIOS"}{ - Hourly data}
#'  \item{"MAXIMOS""}{ - Daily maximums}
#'  \item{"MINIMOS"}{ - Daily minimums}
#' }
#' @param pollutant The type of pollutant to download.
#' \itemize{
#'  \item{"SO2"}{ - Sulfur Dioxide (parts per billion)}
#'  \item{"CO"}{ - Carbon Monoxide (parts per million)}
#'  \item{"NOX"}{ - Nitrogen Oxides (parts per billion)}
#'  \item{"NO2"}{ - Nitrogen Dioxide (parts per billion)}
#'  \item{"NO"}{ - Nitric Oxide (parts per billion)}
#'  \item{"O3"}{ - Ozone (parts per billion)}
#'  \item{"PM10"}{ - Particulate matter 10 micrometers or less
#'  (micrograms per cubic meter)}
#'  \item{"PM25"}{ - Particulate matter 2.5 micrometers or less
#'  (micrograms per cubic meter)}
#'  \item{"WSP"}{ - Wind velocity (meters per second)}
#'  \item{"WDR"}{ - Wind direction (degrees)}
#'  \item{"TMP"}{ - Temperature (degrees Celsius)}
#'  \item{"RH"}{ - Relative humidity (percentage)}
#' }
#' @param year an integer indicating the year for which to download data
#' (the earliest possible value is 1986)
#' @param month month number to download
#'
#' @return a data.frame with pollution data, the hours correspond to the
#' \emph{Etc/GMT+6} timezone, with no daylight saving time
#'
#' @export
#' @family raw data functions
#' @seealso \code{\link{stations}} for a data.frame with the location and names
#' of all polution measuring stations
#' @importFrom stringr str_pad
#' @examples
#' \dontrun{
#' ## Download daily hourly PM10 data (particulate matter 10 micrometers or
#' ## less in diameter) from March 2016
#' df_pm10 <- get_station_month_data("HORARIOS", "PM10", 2016, 3)
#' head(df_pm10)
#'
#' ## Download daily hourly O3 data from October 2017
#' df_o3 <- get_station_month_data("O3", 2017, 10)
#' ## Convert to local Mexico City time
#' df_o3$mxc_time <- format(as.POSIXct(paste0(df_o3$date,
#'                                            " ",
#'                                            df_o3$hour, ":00"),
#'                                     tz = "Etc/GMT+6"),
#'                          tz = "America/Mexico_City")
#' head(df_o3)
#' }
get_station_month_data <- function(criterion, pollutant, year, month) {
  if ( missing(pollutant) || missing(year) || missing(month))
    stop("arguments missing")
  if (!(identical("HORARIOS", criterion) || identical("MAXIMOS", criterion) |
        identical("MINIMOS", criterion)))
    stop("criterion should be 'HORARIOS', 'MINIMOS', or 'MAXIMOS'")
  if (!(identical("O3", pollutant) || identical("NO2", pollutant) ||
        identical("SO2", pollutant) || identical("CO", pollutant) ||
        identical("PM10", pollutant) || identical("WSP", pollutant) ||
        identical("WDR", pollutant) || identical("TMP", pollutant) ||
        identical("NOX", pollutant) || identical("NO", pollutant) ||
        identical("PM25", pollutant) || identical("RH", pollutant)))
    stop("Invalid pollutant value")
  ## Check the year argument is an integer or vector of integers
  if (length(year) < 1)
    stop("year should be an integer in YYYY format")
  for (i in seq_len(length(year)))
    if (is.integer2(year[i]) == FALSE)
      stop("year should be an integer in YYYY format")
  if (min(year) < 2005)
    stop(paste("Data is only available from 2005 onwards. Try the using the",
               " function `get_station_data` to download data by year",
               " from 1986 onwards"))
  if (length(month) != 1)
    stop("you can only download a single month at a time")
  if (pollutant == "WSP" || pollutant == "TMP")
    warning(paste0("Wind speed (WSP) and temperature (TMP) were rounded to the",
            " nearest integer, in some circumstances you can download data",
            " accurate to",
            " one decimal point using the `get_station_data` function. ",
            "See the documentation for more information."), call. = FALSE)

  month <- str_pad(as.character(month), 2, "left", "0")
  if (!(identical("01", month) || identical("02", month) |
        identical("03", month) || identical("04", month) |
        identical("05", month) || identical("06", month) |
        identical("07", month) || identical("08", month) |
        identical("09", month) || identical("10", month) |
        identical("11", month) || identical("12", month)))
    stop("Invalid month value, should be between 1 and 12")

  pollutant <- tolower(pollutant)

  year_no_data <- 2005
  if (year < year_no_data)
    stop(paste0("Monthly data is only available from 2005 onwards, try instead",
                " downloading the data for the entire year (up to 1986) with",
                " `get_station_data`"))
  .download_current_station_data(criterion, pollutant, year, month)
}
