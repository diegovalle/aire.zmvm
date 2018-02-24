is.Date <- function(date, date.format = "%Y-%m-%d") {
  tryCatch(!is.na(as.Date(date, date.format)),
           error = function(e) {FALSE})
}

#' Download pollution data by station in IMECAS
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
#'
#' @return A data.frame with pollution data measured in IMECAS, by station.
#' @export
#' @importFrom rvest html_nodes html_table
#' @importFrom xml2 read_html
#' @importFrom tidyr gather separate_
#'
#' @examples
#' ## There was an ozone pollution emergency on May 15, 2017
#' get_station_imeca("O3", "2017-05-15")
get_station_imeca <- function(pollutant, date) {
  if (missing(date))
    stop("You need to specify a start date (YYYY-MM-DD)")
  if(!is.Date(date))
    stop("Date should be a date in YYYY-MM-DD format")
  if (date < "2009-01-01")
    stop("start_date should be after 2008-01-01")
  stopifnot(length(base::setdiff(pollutant,
                                 c("O3", "NO2", "SO2", "CO", "PM10"))) == 0)

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

