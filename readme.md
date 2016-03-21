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

This package downloads pollution data for the Mexico City metro area. It can download the daily maximum, minimum, or average for each of the pollution measuring stations in the Zona Metropolitana del Valle de Mexico

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

The package consists mainly of two functions:

-   `get_zone_data` to download data for each of the 5 geographic zones of Mexico City
-   `get_station_data` to download data for each of the pollution (and wind and temperature) measuring stations.

``` r
library("aire.zmvm")
library("dplyr")
library("ggplot2")

# Download pm10 data since 2008 for all available zones ("TZ")
pm_10 <- get_zone_data(criterion = "MAXIMOS", # Can be MAXIMOS (daily maximum) or HORARIOS (hourly average)
                       pollutant = "PM10", # "SO2", "CO", "NO2", "O3", "PM10", "TC" (All pollutants)
                       zone = "TZ", # "NO", "NE", "CE", "SO", "SE", "TZ" (All zones)
                       start_date = "2008-01-01", # Can't be earlier than 2008-01-01
                       end_date = "2016-03-19") # Can be up to the current date
knitr::kable(head(pm_10))
```

| date       | zone | pollutant |  value|
|:-----------|:-----|:----------|------:|
| 2008-01-01 | NO   | PM10      |     70|
| 2008-01-02 | NO   | PM10      |     57|
| 2008-01-03 | NO   | PM10      |     48|
| 2008-01-04 | NO   | PM10      |     68|
| 2008-01-05 | NO   | PM10      |     72|
| 2008-01-06 | NO   | PM10      |     63|

``` r
# Plot the overall highest maximum pm10 level with trendline
ggplot(pm_10 %>% group_by(date) %>% summarise(max = max(value, na.rm = TRUE)), 
       aes(date, max, group = 1)) +
  geom_line(color = "darkgray", size = .2) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 50), se = FALSE) +
  theme_bw()
```

![](readme_files/figure-markdown_github/unnamed-chunk-1-1.png)<!-- -->
