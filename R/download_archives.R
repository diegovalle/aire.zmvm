#' Download Pollution Archives
#'
#' Download the pollution files available at
#' \href{http://www.aire.cdmx.gob.mx/aire/default.php?opc=\%27aKBhnmI=\%27&opcion=Zg==}{Contaminante}
#'
#' @param year a numeric vector containing the years for which to download data
#' (the earliest possible value is 2009)
#' @param progress whether to display a progress bar (TRUE or FALSE).
#' By default it will only display in an interactive session.
#'
#' @return a data.frame with pollution information for the following pollutants
#' "CO", "NO", "NO2", "NOX", "O3", "PM10", "SO2", "PM25", and "PMCO"
#' @export
#' @importFrom readr read_csv
#' @importFrom progress progress_bar
#'
#' @examples
#' \dontrun{
#' head(download_pollution(2017))
#' }
download_pollution <- function(year, progress = interactive()) {
  get_data <- function(year) {
    RAMA <- paste0("http://datosabiertos.aire.cdmx.gob.mx:8080/",
                   "opendata/anuales_horarios_gz/contaminantes_")
    ## The files from 2012 onwards changed the name of the columns
    ## cve_station and cve_parameter to id_station and id_parameter
    if (year >= 2012)
      df <- read_csv(str_c(RAMA, year, ".csv.gz"),
                     skip = 10, progress = FALSE, col_types = list(
                       date = col_character(),
                       id_station = col_character(),
                       id_parameter = col_character(),
                       value = col_double(),
                       unit = col_integer()
                     ))
    else
      df <- read_csv(str_c(RAMA, year, ".csv.gz"),
                     skip = 10, progress = FALSE, col_types = list(
                       date = col_character(),
                       cve_station = col_character(),
                       cve_parameter = col_character(),
                       value = col_double(),
                       unit = col_integer()
                     ))
    df <- .clean_archive(df, TRUE)
    df$pollutant <- .recode_pollutant(df$pollutant)
    df
  }
  ## Check the year argument is an integer or vector of integers
  if (length(year) < 1)
    stop("year should be an integer in YYYY format")
  for (i in seq_len(length(year)))
    if (is.integer2(year[i]) == FALSE)
      stop("year should be an integer in YYYY format")
  if (min(year) < 1986)
    stop("year must be greater or equal to 1986")
  if (identical(progress, TRUE) && length(year) > 1) {
    p <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                          total = length(year))
    p$tick(0)
  }
  df <- data.frame()
  for (i in year){
    df <- rbind(df, get_data(i))
    if (identical(progress, TRUE) && length(year) > 1)
      p$tick()
  }
  as.data.frame(df)
}

#' Download Meteorological Data Archives
#'
#' Download the files available at
#' \href{http://www.aire.cdmx.gob.mx/aire/default.php?opc=\%27aKBhnmI=\%27&opcion=Zw==}{Meteorología}
#'
#' @param year a numeric vector containing the years for which to download data
#' (the earliest possible value is 1986)
#' @param progress whether to display a progress bar (TRUE or FALSE).
#' By default it will only display in an interactive session.
#'
#' @return a data.frame with meterological information:
#' "RH","TMP","WDR","WSP","PBa"
#' @export
#' @importFrom readr read_csv
#' @importFrom progress progress_bar
#'
#' @examples
#' \dontrun{
#' head(download_meteorological(2017))
#' }
download_meteorological <- function(year, progress = interactive()) {
  get_data <- function(year) {
    REDMET <- paste0("http://datosabiertos.aire.cdmx.gob.mx:8080/",
                     "opendata/anuales_horarios_gz/meteorolog%C3%ADa_")
    ## The files from 2012 onwards changed the name of the columns
    ## cve_station and cve_parameter to id_station and id_parameter
    if (year >= 2012)
      df <- read_csv(str_c(REDMET, year, ".csv.gz"),
                     skip = 10, progress = FALSE, col_types = list(
                       date = col_character(),
                       id_station = col_character(),
                       id_parameter = col_character(),
                       value = col_double(),
                       unit = col_integer()
                     ))
    else
      df <- read_csv(str_c(REDMET, year, ".csv.gz"),
                     skip = 10, progress = FALSE, col_types = list(
                       date = col_character(),
                       cve_station = col_character(),
                       cve_parameter = col_character(),
                       value = col_double(),
                       unit = col_integer()
                     ))
    .clean_archive(df, TRUE)
  }
  ## Check the year argument is an integer or vector of integers
  if (length(year) < 1)
    stop("year should be an integer in YYYY format")
  for (i in seq_len(length(year)))
    if (is.integer2(year[i]) == FALSE)
      stop("year should be an integer in YYYY format")
  if (min(year) < 1986)
    stop("year must be greater or equal to 1986")
  ## Errors in 2016 and 2017 data
  # if (any(year %in% 2016))
  #   warning(paste0("There may be errors in the 2016 wind speed data.",
  #                  " It was incorrectly converted to mph. Use the function",
  #                  " `get_station_data` to download the correct values"),
  #           call. = FALSE)
  if (identical(progress, TRUE) && length(year) > 1) {
    p <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                          total = length(year))
    p$tick(0)
  }
  df <- data.frame()
  for (i in year){
    df <- rbind(df, get_data(i))
    if (identical(progress, TRUE) && length(year) > 1)
      p$tick()
  }
  as.data.frame(df)
}

#' Download Lead Pollution Archives
#'
#' Download data on lead pollution from the archives available at
#' \href{http://www.aire.cdmx.gob.mx/aire/default.php?opc=\%27aKBhnmE=\%27&r=aHR0cDovLzE0OC4yNDMuMjMyLjExMjo4MDgwL29wZW5kYXRhL3JlZF9tYW51YWwvcmVkX21hbnVhbF9wbG9tby5jc3Y=}{Plomo}
#' and
#' \href{http://www.aire.cdmx.gob.mx/aire/default.php?opc=\%27aKBhnmE=\%27&r=aHR0cDovLzE0OC4yNDMuMjMyLjExMjo4MDgwL29wZW5kYXRhL3JlZF9tYW51YWwvcmVkX21hbnVhbF9wYXJ0aWN1bGFzX3N1c3AuY3N2}{Partículas suspendidas}
#'
#' @param type type of data to download.
#' \itemize{
#'  \item PbPST
#'  \item PST, PM10, PM25
#' }
#'
#' @return A data.frame with pollution data.
#' @export
#' @importFrom readr read_csv
#' @examples
#' \dontrun{
#' head(download_lead("PbPST"))
#' }
download_lead <- function(type) {
  if (!(identical("PbPST", type) || identical("PST, PM10, PM25", type)))
    stop("type should be 'PbPST', or 'PST, PM10, PM25'")
  if (type == "PbPST")
    REDMA <- paste0("http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/red_manual/",
                    "red_manual_plomo.csv")
  else if (type == "PST, PM10, PM25")
    REDMA <- paste0("http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/red_manual/",
                    "red_manual_particulas_susp.csv")
  df <- read_csv(str_c(REDMA),
                 skip = 8, progress = FALSE, col_types = list(
                   Date = col_character(),
                   cve_station = col_character(),
                   cve_parameter = col_character(),
                   value = col_double(),
                   unit = col_integer()
                 ))
  .clean_archive(df, FALSE)
}

#' Download Acid Rain Measurements Archives
#'
#' Download data on rainfall samples collected weekly during the rainy season, available at
#' \href{http://www.aire.cdmx.gob.mx/aire/default.php?opc=\%27aKBhnmE=\%27&r=aHR0cDovLzE0OC4yNDMuMjMyLjExMjo4MDgwL29wZW5kYXRhL3JlZGRhL2RlcG9zaXRvLmNzdg==}{Depósito}
#' and
#' \href{http://www.aire.cdmx.gob.mx/aire/default.php?opc=\%27aKBhnmE=\%27&r=aHR0cDovLzE0OC4yNDMuMjMyLjExMjo4MDgwL29wZW5kYXRhL3JlZGRhL2RlcG9zaXRvVC5jc3Y=}{Depósito}
#'
#' @param type type of ion measurement
#' \itemize{
#'  \item DEPOSITO -  ion quantity deposition
#'  \item CONCENTRACION - ion concentration
#' }
#' @param deposition type of deposition to download
#' \itemize{
#'  \item TOTAL - Total deposition (1988-2000)
#'  \item HUMEDO - Wet and dry deposition (1997-)
#' }
#' @return A data.frame with deposition data.
#' @export
#' @importFrom readr read_csv
#' @importFrom progress progress_bar
#' @examples
#' \dontrun{
#' ## Download rainfall in mm
#' df <- download_deposition(deposition = "HUMEDO", type = "CONCENTRACION") %>%
#'         filter(pollutant == "PP")
#' head(df)
#' }
download_deposition <- function(deposition, type) {
  if (!(identical("HUMEDO", deposition) || identical("TOTAL", deposition)))
    stop("deposition should be 'HUMEDO', or 'TOTAL'")
  if (!(identical("DEPOSITO", type) || identical("CONCENTRACION", type)))
    stop("type should be 'DEPOSITO', or 'CONCENTRACION'")
  # Deposito humedo - deposito
  if (deposition == "HUMEDO" & type == "DEPOSITO")
    REDDA <- "http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/redda/deposito.csv"
  # Deposito humedo - concentracion
  if (deposition == "HUMEDO" & type == "CONCENTRACION")
    REDDA <- "http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/redda/concentracion.csv"
  # Deposito total - deposito
  if (deposition == "TOTAL" & type == "DEPOSITO")
    REDDA <- "http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/redda/depositoT.csv"
  # Deposito total - concentracion
  if (deposition == "TOTAL" & type == "CONCENTRACION")
    REDDA <- "http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/redda/concentracionT.csv"
  df <- read_csv(REDDA,
                 skip = 8, progress = FALSE, col_types = list(
                   Date = col_character(),
                   cve_station = col_character(),
                   cve_parameter = col_character(),
                   value = col_double(),
                   unit = col_integer()
                 ))
  .clean_archive(df, FALSE)
}

#' Download Ultraviolet Radiation Archives
#'
#' Download data on UVA and UVB from the pollution archives available at
#' \href{http://www.aire.cdmx.gob.mx/aire/default.php?opc=\%27aKBhnmI=\%27&opcion=bA==}{Radiación Solar (UVA)}
#' and
#' \href{http://www.aire.cdmx.gob.mx/aire/default.php?opc=\%27aKBhnmI=\%27&opcion=bQ==}{Radiación Solar (UVB)}
#'
#' @param type type of data to download.
#' \itemize{
#'  \item UVA - long wave ultraviolet A
#'  \item UVB - short wave ultraviolet B
#' }
#' @param year a numeric vector containing the years for which to download data
#' (the earliest possible value is 2000)
#' @param progress whether to display a progress bar (TRUE or FALSE).
#' By default it will only display in an interactive session.
#'
#' @return A data.frame with pollution data. The
#' hours correspond to the
#' \emph{Etc/GMT+6} timezone, with no daylight saving time
#' @export
#' @importFrom readr read_csv
#' @importFrom progress progress_bar
#' @examples
#' \dontrun{
#' head(download_radiation("UVA", 2017))
#' }
download_radiation <- function(type, year, progress = interactive()) {
  get_data <- function(year, type) {
    if (type == "UVA")
      RADIACION <- "http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/radiacion/UVA_"
    else if (type == "UVB")
      RADIACION <- "http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/radiacion/UVB_"
    df <- read_csv(str_c(RADIACION, year, ".csv"),
                   skip = 8, progress = FALSE, col_types = list(
                     Date = col_character(),
                     cve_station = col_character(),
                     parameter = col_character(),
                     value = col_double(),
                     unit = col_integer()
                   ))
    .clean_archive(df, TRUE)
  }
  ## Check the year argument is an integer or vector of integers
  if (length(year) < 1)
    stop("year should be an integer in YYYY format")
  for (i in seq_len(length(year)))
    if (is.integer2(year[i]) == FALSE)
      stop("year should be an integer in YYYY format")
  if (min(year) < 2000)
    stop("year must be greater or equal to 2000")
  if (!(identical("UVB", type) || identical("UVA", type)))
    stop("deposition should be 'UVA', or 'UVB'")
  if (identical(progress, TRUE) && length(year) > 1) {
    p <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                          total = length(year))
    p$tick(0)
  }
  df <- data.frame()
  for (i in year){
    df <- rbind(df, get_data(i, type))
    if (identical(progress, TRUE) && length(year) > 1)
      p$tick()
  }
  as.data.frame(df)
}

#' Download archives of the 24 hour averages of pollutants
#'
#' Data comes from
#' \href{http://www.aire.cdmx.gob.mx/aire/default.php?opc=\%27aKBhnmI=\%27&opcion=ag==}{Promedios de 24 horas de partículas suspendidas(PM10 Y PM2.5)} and
#' \href{http://www.aire.cdmx.gob.mx/aire/default.php?opc=\%27aKBhnmI=\%27&opcion=aQ==}{Promedios de 24 horas de Dióxido azufre}
#'
#' @param type  type of data to download.
#' \itemize{
#'  \item SO2 - Sulfur Dioxide (parts per billion)
#'  \item PS - Suspended solids
#' }
#' @param year a numeric vector containing the years for which to download data
#' (the earliest possible value is 1986 for SO2 and 1995 for PS)
#' @param progress whether to display a progress bar (TRUE or FALSE).
#' By default it will only display in an interactive session.
#'
#' @return A data.frame with pollution data.
#' @export
#' @importFrom readr read_csv
#' @importFrom progress progress_bar
#'
#' @examples
#' \dontrun{
#' head(download_24hr_average("PS", 2017))
#' }
download_24hr_average <- function(type, year, progress = interactive()) {
  get_data <- function(year, type) {
    if (type == "PS")
      base_url <- paste0("http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/",
                         "promedios_diarios/promedios_")
    else if (type == "SO2")
      base_url <- paste0("http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/",
                         "promedios_diarios/promedios_")

    df <- read_csv(str_c(base_url, year, "_", tolower(type), ".csv"),
                   skip = 8, progress = FALSE,
                   col_types = list(
                     date         = col_character(),
                     id_station   = col_character(),
                     id_parameter = col_character(),
                     value        = col_double(),
                     unit         = col_integer()
                   ))
    .clean_archive(df, FALSE)
  }
  ## Check the year argument is an integer or vector of integers
  if (length(year) < 1)
    stop("year should be an integer in YYYY format")
  for (i in seq_len(length(year)))
    if (is.integer2(year[i]) == FALSE)
      stop("year should be an integer in YYYY format")
  if (!(identical("SO2", type) || identical("PS", type)))
    stop("type should be 'SO2', or 'PS'")
  if (min(year) < 1986 & type == "SO2")
    stop("year must be greater or equal to 1986")
  if (min(year) < 1995 & type == "PS")
    stop("year must be greater or equal to 1995")

  if (identical(progress, TRUE) && length(year) > 1) {
    p <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                          total = length(year))
    p$tick(0)
  }
  df <- data.frame()
  for (i in year){
    df <- rbind(df, get_data(i, type))
    if (identical(progress, TRUE) && length(year) > 1)
      p$tick()
  }
  as.data.frame(df)
}

#' Download Atmospheric Pressure Archives
#'
#' The data comes from \href{http://www.aire.cdmx.gob.mx/aire/default.php?opc=\%27aKBhnmI=\%27&opcion=bg==}{Presión Atmosférica}
#'
#' @param year a numeric vector containing the years for which to download data
#' (the earliest possible value is 2009)
#' @param progress whether to display a progress bar (TRUE or FALSE).
#' By default it will only display in an interactive session.
#'
#' @return A data.frame with atmospheric pressure data.
#' @export
#' @importFrom readr read_csv
#' @importFrom progress progress_bar
#'
#' @examples
#' \dontrun{
#' head(download_pressure(2017))
#' }
download_pressure <- function(year, progress = interactive()) {
  get_data <- function(year) {
    PRESION <- "http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/presion/PA_"
    df <- read_csv(str_c(PRESION, year, ".csv"),
                   skip = 8, progress = FALSE,
                   col_types = list(
                     Date        = col_character(),
                     cve_station = col_character(),
                     parameter   = col_character(),
                     value       = col_double(),
                     unit        = col_integer()
                   ))
    .clean_archive(df, TRUE)
  }
  ## Check the year argument is an integer or vector of integers
  if (length(year) < 1)
    stop("year should be an integer in YYYY format")
  for (i in seq_len(length(year)))
    if (is.integer2(year[i]) == FALSE)
      stop("year should be an integer in YYYY format")
  if (min(year) < 2009)
    stop("year must be equal or greater than 2009")

  if (identical(progress, TRUE) && length(year) > 1) {
    p <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                          total = length(year))
    p$tick(0)
  }
  df <- data.frame()
  for (i in year){
    df <- rbind(df, get_data(i))
    if (identical(progress, TRUE) && length(year) > 1)
      p$tick()
  }
  as.data.frame(df)
}

#' Clean archive files
#'
#' @param df data.frame to clean
#' @param include_hour is the data hourly
#'
#' @return data.frame
#' @export
#' @importFrom stringr str_replace_all str_sub
#' @keywords internal
.clean_archive <-  function(df, include_hour) {
  names(df) <- c("date", "station_code", "pollutant", "value", "unit")

  df$date <- str_replace_all(df$date, "\t", "")
  if (include_hour)
    df$hour <- as.numeric(str_sub(df$date, 12, 13))
  df$date <- str_c(str_sub(df$date, 7, 10), "-",
                   str_sub(df$date, 4, 5), "-",
                   str_sub(df$date, 1, 2))
  df$value <- as.numeric(df$value)
  df$date <- as.Date(df$date)

  df$unit <- .recode_unit_code(df$unit)

  if (include_hour)
    df <- df[, c("date", "hour", "station_code", "pollutant", "unit", "value")]
  else
    df <- df[, c("date", "station_code", "pollutant", "unit", "value")]

  # sometimes with download_deposition("TOTAL", "DEPOSITO") and
  # download_deposition("TOTAL", "CONCENTRACION") the last line is NAs
  if (is.na(df$date[nrow(df)]))
    df <- df[1:(nrow(df) - 1), ]
  as.data.frame(df)
}
