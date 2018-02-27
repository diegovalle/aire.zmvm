is.Date <- function(date, date.format = "%Y-%m-%d") {
  tryCatch(!is.na(as.Date(date, date.format)),
           error = function(e) {FALSE})
}

#' Download pollution data by station in IMECAS
#'
#' Note that in
#' 2015 it was determined that the stations with codes ACO, AJU, INN, MON
#' and MPA would no longer be taken into consideration when computing the
#' pollution index and at some point in the future would no longer be inclued
#' in the data returned by this function
#'
#' @param pollutant The type of pollutant to download
#' \itemize{
#'  \item{"SO2"}{ - Dioxido de azufre}
#'  \item{"CO"}{ - Monoxido de carbono}
#'  \item{"NO2"}{ - Dioxido de nitrogeno}
#'  \item{"O3"}{ - Ozono}
#'  \item{"PM10"}{ - Particulas menores a 10 micrometros}
#' }
#' @param date The date for which to download data in YYYY-MM-DD format
#' (the earliest possible date is 2009-01-01).
#' @param showWarnings Show warnings about problems with the data
#'
#' @return A data.frame with pollution data measured in IMECAS, by station.
#' The hours correspond to the UTC-6 timezone, with no daylight saving time
#' @export
#' @importFrom rvest html_nodes html_table
#' @importFrom xml2 read_html
#' @importFrom tidyr gather
#'
#' @examples
#' ## There was an ozone pollution emergency on May 15, 2017
#' df_o3 <- get_station_imeca("O3", "2017-05-15", showWarnings = FALSE)
#' head(df_o3[order(-df_o3$value), ])
get_station_imeca <- function(pollutant, date,
                              showWarnings = TRUE) {
  if (missing(date))
    stop("You need to specify a start date (YYYY-MM-DD)")
  if(!is.Date(date))
    stop("Date should be a date in YYYY-MM-DD format")
  if (date < "2009-01-01")
    stop("start_date should be after 2009-01-01")
  stopifnot(length(base::setdiff(pollutant,
                                 c("O3", "NO2", "SO2", "CO", "PM10"))) == 0)
  if (date >= "2017-01-01" && showWarnings)
    warning("\n*******************\nSometime in 2015-2017 the stations ACO, AJU, INN, MON, and MPA were excluded from the index\n*******************")


  url <- "http://www.aire.cdmx.gob.mx/default.php?opc=%27aqBjnmc=%27"
  fd <- list(
    fecha	= date,
    RadioGroup1	= switch(pollutant,
                         "O3" = 0,
                         "NO2" = 1,
                         "SO2" = 2,
                         "CO" = 3,
                         "PM10" = 4),
    aceptar	= "Submit",
    consulta	= 1
  )

  result <- httr::POST(url,
                       body = fd,
                       encode = "form")
  poll_table <- xml2::read_html(content(result, "text"))

  df <- rvest::html_table(rvest::html_nodes(poll_table, "table")[[1]],
                          header = TRUE,
                          fill = TRUE)
  if (nrow(df) <= 1)
    stop("The website returned invalid data. Please check the date format.")
  pollutant2 <- names(df)[3]
  df <- df[,!is.na( df[1, ])]
  names(df) <- df[1, ]
  names(df)[1] <- "date"
  names(df)[2] <- "hour"
  df <- df[2:nrow(df), ]
  df <- gather(df, station_code, value, -date, -hour)
  df[which(df$value == ""), "value"] <- NA
  df$value <- as.numeric(as.character(df$value))
  df$pollutant <- pollutant2
  df$unit <- "IMECA"
  df[,c("date", "hour", "station_code", "pollutant", "unit", "value" )]
}

