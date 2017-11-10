#' Title
#'
#' @param time_div time to convert
#'
#' @importFrom stringr str_match str_replace str_replace_all str_detect
.convert_time <- function(time_div){
  time_div <- str_replace_all(time_div, "\n|\t", "")
  time_div <- str_replace(time_div, "h,(.|\n)+?(?=[0-9])", "h")
  month_names <- c("enero" = "january", "febrero" = "february", "marzo" = "march",
                   "abril" = "april", "mayo" = "may", "junio" = "june", "julio" = "july",
                   "agosto" = "august", "septiembre" = "september",
                   "octubre" = "october", "noviembre" = "november", "diciembre" = "december")
  time_div <- str_replace_all(time_div, month_names)
  if (str_detect(time_div, "24:00"))
    warning("At midnight the website sometimes get the time wrong and reports a date 24 hours into the future")
  time_div <- strptime(time_div, "%H:%M h%d de %B de %Y", tz = "America/Mexico_City")
  as.character(strftime(time_div, "%Y-%m-%d %H:%M:%S"))
}

#' Get the latest pollution values for each station
#'
#' @return A data.frame with pollution values in IMECAs, the time corresponds to the
#' America/Mexico_City timezone
#' @export
#' @importFrom utils URLdecode
#' @importFrom rvest html_nodes html_text
#' @importFrom xml2 read_html
#' @importFrom stringr str_match str_replace str_replace_all
#' @importFrom httr GET
#'
#' @examples
#' df <- get_latest_data()
#' head(df)
get_latest_data <- function() {
  url <- "http://www.aire.cdmx.gob.mx/ultima-hora-reporte.php"

  poll_table <- read_html(httr::GET(url,  httr::timeout(120)))
  time <- .convert_time(html_text(html_nodes(poll_table, "div#textohora")))

  df <- html_table(html_nodes(poll_table, "table")[[1]], header = TRUE, fill = TRUE)
  names(df) <- c("station_code", "municipio", "quality", "pollutant", "value")
  df <- df[2:nrow(df), ]
  df$value <- lapply(df$value, function(x) URLdecode(str_match(URLdecode(x), "'(\\d+)'")[[2]]))

  edomex <- html_table(html_nodes(poll_table, "table")[[2]], header = TRUE, fill = TRUE)
  names(edomex) <- c("station_code", "municipio", "quality", "pollutant", "value")
  edomex <- edomex[2:nrow(edomex), ]
  edomex$value <- lapply(edomex$value,
                         function(x) URLdecode(str_match(URLdecode(x), "'(\\d+)'")[[2]]))

  mxc <- rbind(df, edomex)
  mxc$value[mxc$value == "NA"] <- NA
  mxc$value <- as.integer(mxc$value)
  mxc$datetime <- time
  mxc$unit <- "IMECA"
  mxc <- mxc[, c("station_code", "municipio", "quality", "pollutant", "unit", "value", "datetime")]

  mxc[!is.na(mxc$station_code), ]
}
