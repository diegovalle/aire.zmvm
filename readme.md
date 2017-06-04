Mexico City Pollution Data
================
Diego Valle-Jones
May 19, 2017

-   [What does it do?](#what-does-it-do)
-   [Installation](#installation)
-   [Quick Example](#quick-example)

Master: [![Travis-CI Build Status](https://travis-ci.org/diegovalle/aire.zmvm.svg?branch=master)](https://travis-ci.org/diegovalle/aire.zmvm)

|              |                                                        |
|--------------|--------------------------------------------------------|
| **Author:**  | Diego Valle-Jones                                      |
| **License:** | [BSD\_3](https://opensource.org/licenses/BSD-3-Clause) |
| **Status:**  | alpha                                                  |
| **Website:** | <https://github.com/diegovalle/aire.zmvm>              |

What does it do?
----------------

This package downloads pollution data for the Mexico City metro area. It can download real-time, daily maximum, minimum, or hourly average data for each of the pollution measuring stations or geographical zones in the Zona Metropolitana del Valle de México (greater Mexico City).

Installation
------------

For the moment this package is only available from github. For the development version:

``` r
if (!require(devtools)) {
    install.packages("devtools")
}
devtools::install_github('diegovalle/aire.zmvm')
```

Quick Example
-------------

The package consists mainly of three functions:

-   `get_station_data` to download data for each of the pollution (and wind and temperature) measuring stations.
-   `get_zone_data` to download data for each of the 5 geographic zones of Mexico City
-   `get_latest_data` to download the latest values for each of the pollution measuring stations.

``` r
library("aire.zmvm")
library("dplyr")
library("ggplot2")
library("ggseas")

o3 <- get_station_data(criterion = "MAXIMOS", # Can be one of MAXIMOS (daily maximum), 
                                                # MINIMOS (daily minimum), 
                                                # or HORARIOS (hourly average)
                       pollutant = "O3", # Can be one of "SO2", "CO", "NOX", "NO2", "NO", "O3", 
                                         # "PM10", "PM25", "WSP", "WDR", "TMP", "RH"
                       year = 2009:2017) # A numeric vector, the earliest year allowed is 1986
knitr::kable(head(o3))
```

| date       | station\_code | pollutant | unit |  value|
|:-----------|:--------------|:----------|:-----|------:|
| 2009-01-01 | ACO           | O3        | ppb  |     67|
| 2009-01-02 | ACO           | O3        | ppb  |     71|
| 2009-01-03 | ACO           | O3        | ppb  |    112|
| 2009-01-04 | ACO           | O3        | ppb  |     91|
| 2009-01-05 | ACO           | O3        | ppb  |     70|
| 2009-01-06 | ACO           | O3        | ppb  |     71|

``` r
# Daily max among all base stations
o3_max <- o3 %>% 
  group_by(date) %>% 
  summarise(max = ifelse(all(is.na(value)),
                         NA,
                         base::max(value, na.rm = TRUE))) %>%
  na.omit()

# ozone values at which a contingencia ambiental was declared
# and the dates during which they were valid
contingencia <- data.frame(ppb = c(155, 185, 199, 205, 210),
  start = c(2016.33, 2014, 2011, 2010, 2009),
  end = c(2017.5, 2016 + 93/365, 2014, 2011, 2010))
max_daily_df <- tsdf(ts(o3_max$max, start = c(2009,1), frequency = 365.25))
ggplot(max_daily_df,
       aes(x = x, y = y)) + 
  geom_line(colour = "grey75", alpha = .5) +
  stat_rollapplyr(width = 30, align = "right", color = "#01C5D2") +
  #geom_vline(xintercept = 2015 + 183/365) +
  geom_segment(data = contingencia, 
               aes(x=start, y=ppb, xend=end, yend=ppb), color="darkred", linetype = 2)  +
  xlab("date") +
  ylab("parts per billion") +
  ggtitle("Maximum daily ozone concentration and 30 day rolling average", 
          subtitle = "Red lines indicate the values necessary to activate a phase I smog alert. \nData source: SEDEMA")
```

    ## Warning: Removed 29 rows containing missing values (geom_path).

![](readme_files/figure-markdown_github/unnamed-chunk-1-1.png)
