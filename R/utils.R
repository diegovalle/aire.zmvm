# test if int is an integer
is.integer2 <- function(int) {
  if (length(int) < 1)
    return(FALSE)
  if (any(is.na(int)))
    return(FALSE)
  tryCatch(identical(int, as.integer(floor(int))) |
             identical(int, as.double(floor(int))) |
             identical(int, as.single(floor(int))),
           error = function(e) {
             FALSE
           })
}

# test if date is in YYYY-MM-DD format
is.Date <- function(date, date.format = "%Y-%m-%d") {
  if (length(date) < 1)
    return(FALSE)
  tryCatch(!is.na(as.Date(date, date.format)),
           error = function(e) {
             FALSE
           })
}

# http://www.aire.cdmx.gob.mx/descargas/monitoreo/normatividad/NADF-009-AIRE-2006.pdf
# 4.20 Redondeo: Formato que modifica la información después de una cifra de
# interés, de tal manera que si la siguiente cifra es 4 ó menor no se considera
# y no cambia la cifra de interés. Cuando la cifra siguiente es 5 ó mayor,
# entonces la cifra de interés se incrementa en una unidad (17). Por ejemplo: el
# redondeo de la cifra 0.1105 es 0.111, y en el caso de la cifra 0.1104 el
# resultado es 0.110.
round_away_from_zero <- function(r) {
  posneg <-  sign(r)
  z <- abs(r)
  z <- trunc(z + 0.5)
  z * posneg
}

#' Recode pollutant abbreviations
#'
#' @param pollutant type of pollutant (O3, SO2, etc)
#'
#' @return data.frame
#' @export
#' @importFrom dplyr recode
#' @keywords internal
.recode_pollutant <- function(pollutant) {
  recode(pollutant,
         "pm2" = "PM25",
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
         "PM2.5" = "PM25",
         "PM2" = "PM25")
}

#' Recode numeric codes to concentration units (extended)
#'
#' @param code numeric code for the the units
#'
#' @return data.frame
#' @export
#' @importFrom dplyr recode
#' @keywords internal
.recode_unit_code <- function(code) {
  recode(code,
         `1`  =	"ppb",
         `2`  =	"\u00b5g/m\u00b3", # µg/m³",
         `3`  =	"m/s",
         `4`  =	"\u00b0", # "°",
         `5`  =	"\u00b0C", # "°C",
         `6`  =	"%",
         `7`  =	"W/m\u00b2", # "W/m²",
         `8`  =	"\u00b5mol/m\u00b2/s", # "µmol/m²/s",
         `9`  =	"mmHg",
         `10` =	"pH",
         `11` =	"mm",
         `12` =	"\u00b5S/cm", # "µS/cm",
         `13` =	"mg/L",
         `14` =	"mg/m\u00b2", # "mg/m²",
         `15` =	"ppm",
         `16` =	"MED/h",
         `17` =	"mW/cm\u00b2" # mW/cm²
  )
}

#'  Recode numeric codes to concentration units
#'
#' @param pollutant type of pollutant (O3, SO2, etc)
#'
#' @return data.frame
#' @export
#' @importFrom dplyr recode
#' @keywords internal
.recode_unit <- function(pollutant) {
  recode(pollutant,
         "pm2" = "\u00B5g/m\u00B3",
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
         "rh" = "%")
}

#' Hack to download 2016 WSP HORARIO data since the csv files were converted
#' from mph to m/s (when the original data were already in m/s)
#'
#'
#' @param syear shortyear in YY format
#' @return data.frame
#' @export
#' @importFrom stringr str_extract
#' @importFrom readxl read_excel
#' @importFrom tidyr gather
#' @importFrom utils download.file unzip
#' @keywords internal
.get_archive_wsp <- function(syear) {
  download_loc <- paste0("http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/excel/",
                         "REDMET/", syear, "REDMET.zip")

  # unzip
  tmpdir <- tempdir()
  file <- basename(download_loc)
  download.file(download_loc, file.path(tmpdir, file), quiet = TRUE)
  # get the contents of the archive before unziping
  xls_files <- unzip(file.path(tmpdir, file), exdir = tmpdir,
                     list = TRUE)[, "Name"]
  if (!length(xls_files)) {
    stop("Zip file is empty")
  }

  unzip(file.path(tmpdir, file), exdir = tmpdir)
  df <- read_excel(file.path(tmpdir,
                             xls_files[which(xls_files == paste0("20", syear,
                                                                 "WSP.xls"))]),
                   na = c("-99", ""))
  # Clean the data
  names(df)[1] <- "date"
  names(df)[2] <- "hour"
  df <- gather(df, station_code, value, -date, -hour)
  df$pollutant <- "WSP"
  df$unit <- "m/s"
  df <- df[, c("date", "hour", "station_code", "pollutant", "unit", "value")]
  as.data.frame(df)
}
