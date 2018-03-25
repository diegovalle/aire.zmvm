Mexico City Air Quality Data
================
Diego Valle-Jones
March 25, 2018

-   [What does it do?](#what-does-it-do)
-   [Installation](#installation)
-   [Core Functions](#core-functions)
-   [Quick Example](#quick-example)

[![Travis-CI Build Status](https://travis-ci.org/diegovalle/aire.zmvm.svg?branch=master)](https://travis-ci.org/diegovalle/aire.zmvm) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/c7kg6o68exx0lirg?svg=true)](https://ci.appveyor.com/project/diegovalle/aire-zmvm/branch/master) [![Coverage Status](https://img.shields.io/codecov/c/github/diegovalle/aire.zmvm/master.svg)](https://codecov.io/github/diegovalle/aire.zmvm?branch=master) ![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version-ago/aire.zmvm?color=green)

|              |                                                        |
|--------------|--------------------------------------------------------|
| **Author:**  | Diego Valle-Jones                                      |
| **License:** | [BSD\_3](https://opensource.org/licenses/BSD-3-Clause) |
| **Website:** | <https://hoyodesmog.diegovalle.net/aire.zmvm/>         |

What does it do?
----------------

Tools for downloading airquality data for the Mexico City metro area. This package can download real-time, daily maximum, minimum, or hourly average data for each of the pollution measuring stations or geographical zones in the Zona Metropolitana del Valle de México (greater Mexico City). It also includes the locations of all the measuring stations and a function to perform inverse distance weighting modified to work with wind direction.

Installation
------------

You can always install the development version from GitHub:

``` r
if (!require(devtools)) {
    install.packages("devtools")
}
devtools::install_github('diegovalle/aire.zmvm')
```

To install the most recent package version from CRAN type:

``` r
install.packages("aire.zmvm")
library(aire.zmvm)
```

Note that the version on CRAN might not reflect the most recent changes made to this package.

Core Functions
--------------

The package core functions:

-   `get_station_data` and `get_station_month_data` download pollution, wind and temperature data for each of the measuring stations in the original units (ppb, µg/m³, etc).
-   `get_station_imeca` download pollution values for each station in IMECAs
-   `get_zone_imeca` download pollution data in IMECAs for each of the 5 geographic zones of Mexico City
-   `get_latest_imeca` download the latest pollution hourly maximums for each of the measuring stations.
-   `idw360` inverse distance weighting modified to work with degrees, useful for wind data

<table style="width:100%;">
<colgroup>
<col width="23%" />
<col width="14%" />
<col width="9%" />
<col width="16%" />
<col width="11%" />
<col width="11%" />
<col width="6%" />
<col width="6%" />
</colgroup>
<thead>
<tr class="header">
<th>Function</th>
<th>Date range</th>
<th>Units</th>
<th>Wind, Tmp, RH</th>
<th>Earliest Date</th>
<th>Pollutants</th>
<th>Includes All Stations</th>
<th>Criterion</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>get_station_data</td>
<td>years</td>
<td>Original</td>
<td>Yes</td>
<td>1986</td>
<td>SO2, CO, NO2, O3, PM10, PM25, WSP, WDR, TMP, RH</td>
<td>Yes</td>
<td>hourly, daily maximum, daily minimum</td>
</tr>
<tr class="even">
<td>get_station_month_data</td>
<td>1 month</td>
<td>Original</td>
<td>Yes</td>
<td>2005‑01</td>
<td>SO2, CO, NO2, O3, PM10, PM25, WSP, WDR, TMP, RH</td>
<td>Yes</td>
<td>hourly, daily maximum, daily minimum</td>
</tr>
<tr class="odd">
<td>get_station_imeca</td>
<td>1 day</td>
<td>IMECA</td>
<td>No</td>
<td>2009‑01‑01</td>
<td>SO2, CO, NO2, O3, PM10</td>
<td>No</td>
<td>hourly</td>
</tr>
<tr class="even">
<td>get_zone_imeca</td>
<td>1 or more days</td>
<td>IMECA</td>
<td>No</td>
<td>2008‑01‑01</td>
<td>SO2, CO, NO2, O3, PM10</td>
<td>Only zones</td>
<td>hourly, daily maximum</td>
</tr>
<tr class="odd">
<td>get_latest_imeca</td>
<td>1 hour</td>
<td>IMECA</td>
<td>No</td>
<td>Latest only</td>
<td>Maximum value of SO2, CO, NO2, O3, PM10</td>
<td>No</td>
<td>latest hourly</td>
</tr>
</tbody>
</table>

Quick Example
-------------

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
# source: http://www.aire.cdmx.gob.mx/descargas/ultima-hora/calidad-aire/pcaa/pcaa-modificaciones.pdf
contingencia <- data.frame(ppb = c(216, 210, 205, 199, 185, 155, 155),
  start = c(2009, 2009.4973, 2010.4973, 2011.5795, 
            2012.6052, 2016.291, 2016.4986),
  end = c(2009.4973, 2010.4945, 2011.4945, 
          2012.6025, 2016.2883, 2016.4959, Inf))
max_daily_df <- tsdf(ts(o3_max$max, start = c(2009,1), frequency = 365.25))
ggplot(max_daily_df,
       aes(x = x, y = y)) + 
  geom_line(colour = "grey75", alpha = .5) +
  stat_rollapplyr(width = 30, align = "right", color = "#01C5D2") +
  #geom_vline(xintercept = 2015 + 183/365) +
  geom_segment(data = contingencia, 
               aes(x=start, y=ppb, xend=end, yend=ppb), color="darkred", 
               linetype = 2)  +
  xlab("date") +
  ylab("parts per billion") +
  ggtitle("Maximum daily ozone concentration and 30 day rolling average", 
          subtitle = paste0("Red lines indicate the values necessary to ",
                            "activate a phase I smog alert.",
                            "\nData source: SEDEMA"))
```

![](man/figures/README-unnamed-chunk-2-1.png)
