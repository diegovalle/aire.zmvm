library(readr)
library(dplyr)

zones <- read_csv("data-raw/zonas.csv", col_types = cols(
  region = col_character(),
  state_code = col_character(),
  state_abbr = col_character(),
  municipio_code = col_character(),
  municipio_name = col_character(),
  zone = col_character()
)) %>%
  arrange(zone)
head(zones)
devtools::use_data(zones, overwrite = TRUE)
