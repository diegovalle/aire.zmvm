Mexico City Pollution Data
================
Diego Valle-Jones
March 18, 2016

-   [What does it do?](#what-does-it-do)
-   [Installation](#installation)
-   [Quick Example](#quick-example)

Master: [![Travis-CI Build Status](https://travis-ci.org/diegovalle/aire.zmvm.svg?branch=master)](https://travis-ci.org/diegovalle/aire.zmvm)

<table style="width:43%;">
<colgroup>
<col width="20%" />
<col width="22%" />
</colgroup>
<tbody>
<tr class="odd">
<td align="left"><strong>Author:</strong></td>
<td align="left">Diego Valle-Jones</td>
</tr>
<tr class="even">
<td align="left"><strong>License:</strong></td>
<td align="left"><a href="https://opensource.org/licenses/BSD-3-Clause">BSD_3</a></td>
</tr>
<tr class="odd">
<td align="left"><strong>Status:</strong></td>
<td align="left">alpha</td>
</tr>
<tr class="even">
<td align="left"><strong>Website:</strong></td>
<td align="left"><a href="https://github.com/diegovalle/aire.zmvm" class="uri">https://github.com/diegovalle/aire.zmvm</a></td>
</tr>
</tbody>
</table>

What does it do?
----------------

This package downloads pollution data for the Mexico City metro area. It can download real-time, daily maximum, minimum, or average for each of the pollution measuring stations or geographical zones in the Zona Metropolitana del Valle de México (greater Mexico City)

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

pm10 <- get_station_data(criterion = "MAXIMOS", # Can be MAXIMOS (daily maximum), 
                                                # MINIMOS (daily minimum), 
                                                # or HORARIOS (hourly average)
                       pollutant = "O3", # "SO2", "CO", "NOX", "NO2", "NO", "O3", 
                                         # "PM10", "PM2", "WSP", "WDR", "TMP", "RH"
                       year = 1986:2016) # The earliest year allowed is 1986
knitr::kable(head(pm10))
```

| date       | station\_code |  value|
|:-----------|:--------------|------:|
| 1986-01-01 | AZC           |     NA|
| 1986-01-01 | CES           |     NA|
| 1986-01-01 | HAN           |     NA|
| 1986-01-01 | MER           |     NA|
| 1986-01-01 | PED           |     NA|
| 1986-01-01 | PLA           |     NA|

``` r
# Daily max among all base stations
pm10_max <- pm10 %>% 
  group_by(date) %>% 
  summarise(max = ifelse(all(is.na(value)),
                         NA,
                         base::max(value, na.rm = TRUE))) %>%
  na.omit()

# Plot the daily highest pm10 level with trendline
ggplot(pm10_max, 
       aes(as.Date(date), max, group = 1)) +
  geom_point(color = "black", size = .2, alpha = .4) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 40)) +
  ggtitle("Daily maximum O3 levels") +
  ylab("maximum daily O3 value in ppb") +
  xlab("date") +
  geom_vline(xintercept = as.numeric(as.Date("1989-11-20"))) +
  annotate("text", label = "hoy no circula", 
           x = as.Date("1987-03-20"),
           y = 480) +
  theme_bw()
```

![](readme_files/figure-markdown_github/unnamed-chunk-1-1.png)<!-- -->
