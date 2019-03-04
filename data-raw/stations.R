stations <- read.csv("data-raw/stations.csv", stringsAsFactors = FALSE)

Encoding(stations$station_name) <- "UTF-8"
Encoding(stations$comment) <- "UTF-8"

#write.csv(stations, "data-raw/stations.csv", row.names = FALSE)
save(stations, file = "data/stations.RData", compress = "xz")
