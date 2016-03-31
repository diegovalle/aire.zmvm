#' Get the latest pollution values for each station
#'
#' @return A data.frame with pollution values in IMECAs
#' @export
#' @importFrom utils URLdecode
#' @importFrom rvest html_nodes html_text
#' @importFrom xml2 read_html
#' @importFrom stringr str_match str_replace str_replace_all
#'
#' @examples
#' \dontrun{
#' df <- get_latest_data()
#' head(df)
#' }
get_latest_data <- function() {
  url = "http://www.aire.df.gob.mx/ultima-hora-reporte.php"

  poll_table <- read_html(url)

  hour <- html_text(html_nodes(poll_table, "div#textohora"))
  hour <- str_replace_all(hour, "\n|\t", "")
  hour <- str_replace(hour, ",[:alpha:]+ ", "")
  month_names <- c('enero'='january','febrero'='february','marzo'='march',
                   'abril'='april','mayo'='may','junio'='june','julio'='july',
                   'agosto'='august','septiembre'='september',
                   'octubre'='october','noviembre'='november','diciembre'='december')
  hour <- str_replace_all(hour, month_names)
  time <- as.character(strptime(hour, "%H:%M h%d de %B de %Y ", tz = "MX"))

  df <- html_table(html_nodes(poll_table, "table")[[1]], header = TRUE, fill = TRUE)
  names(df) <- c("station_code", "municipio", "quality", "contaminant", "value")
  df <- df[2:nrow(df),]
  df$value <- sapply(df$value, function(x) URLdecode(str_match(URLdecode(x),"'(\\d+)'")[[2]]))

  edomex <- html_table(html_nodes(poll_table, "table")[[2]], header = TRUE, fill = TRUE)
  names(edomex) <- c("station_code", "municipio", "quality", "contaminant", "value")
  edomex <- edomex[2:nrow(edomex),]
  edomex$value <- sapply(edomex$value,
                         function(x) URLdecode(str_match(URLdecode(x),"'(\\d+)'")[[2]]))

  mxc <- rbind(df, edomex)
  mxc$value[mxc$value=="NA"] <- NA
  mxc$value <- as.numeric(mxc$value)
  mxc$time <- time

  mxc[!is.na(mxc$station_code),]
}
