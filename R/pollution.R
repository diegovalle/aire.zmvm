

#' Title
#'
#' @param tipo bla
#' @param parametro bla
#' @param anio bla
#'
#' @importFrom stringr str_c  str_replace_all
#' @importFrom rvest html_nodes html_table
#' @importFrom xml2 read_html
#' @importFrom dplyr %>%
#'
clean_aire <- function(tipo, parametro, anio) {
  base_url = "http://www.aire.df.gob.mx/estadisticas-consultas/concentraciones/respuesta.php?"
  url <- str_c(base_url, "qtipo=", tipo, "&",
               "parametro=", parametro, "&",
               "anio=", anio, "&",
               "qmes=")
  poll_table <- read_html(url)

  df <- html_table(html_nodes(poll_table, "table")[[1]], header = TRUE)
  # df <- poll_table %>%
  #   html_nodes("table")%>%
  #   .[[1]] %>%
  #   html_table(header = TRUE)
  names(df) <- df[1,]
  names(df)[1] <- "date"
  names(df) <- str_replace_all(names(df), "\\s", "")
  df <- df[2:nrow(df),]

  df[df == "nr"] <- NA
  df[,2:ncol(df)] <- apply(df[,2:ncol(df)], 2, as.numeric)
  # when the data is HORARIOS the second column corresponds to the hour
  if(tipo != "HORARIOS") {
    idx = 2
  } else {
    idx = 3
    names(df)[2] <- "hour"
  }
  df$max <- apply(df[ ,idx:ncol(df)], 1, max, na.rm = TRUE)
  df$min <- apply(df[ ,idx:ncol(df)], 1, min, na.rm = TRUE)
  df$mean <- apply(df[ ,idx:ncol(df)], 1, mean, na.rm = TRUE)
  df$median <- apply(df[ ,idx:ncol(df)], 1, median, na.rm = TRUE)
  return(df)
}

#' Download pollution data
#'
#' retrieve pollution data from the air quality server at \url{
#' http://www.aire.df.gob.mx/estadisticas-consultas/concentraciones/index.php}
#'
#' @param tipo Type of data to download. Can be "HORARIOS" for hourly averages, "MAXIMOS" for the
#' daily maximum and "MINIMOS" for the daily minimum
#' @param parametro The type of pollutant to download. Can be of type "so2", "co", "nox", "no2",
#' "no", "o3", "pm10", "pm2", "wsp", "wdr", "tmp", "rh"
#' @param anio a vector containing the years for which to download data
#'
#' @return a data.frame with pollution data
#' @export
#' @importFrom data.table rbindlist
#' @importFrom lubridate dmy
#'
#' @examples
#' \dontrun{
#' # Download daily maximum PM10 data (particulate matter 10 micrometers or less in diameter)
#' # from 2005 to 2016
#' df <- get_pollution_data("MAXIMOS", "pm10", 2005:2016)
#' head(df)
#' }
get_pollution_data <- function(tipo, parametro, anio) {
  stopifnot(tipo %in% c("HORARIOS", "MAXIMOS", "MINIMOS"))
  stopifnot(parametro %in% c("so2", "co", "nox", "no2",
                             "no", "o3", "pm10", "pm2",
                             "wsp", "wdr", "tmp", "rh"))
  stopifnot(min(anio)>=2005)
  years <- anio
  ll <- mapply(clean_aire,
               tipo = rep(tipo, length(years)),
               parametro = rep(parametro, length(years)),
               anio = years,
               SIMPLIFY = FALSE)
  df <- rbindlist(ll)
  df$date <- dmy(df$date)
  return(df)
}
