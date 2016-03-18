Mexico City Pollution Data
================
Diego Valle-Jones
February 11, 2016

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

``` r
library("aire.zmvm")
library("dplyr")
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library("lubridate")
library("seasonal")


years <- 2005:2016
# Download pm10 data for the years 2005:2016
# The type of data can be: MAXIMOS, MINIMOS or HORARIOS (hourly average)
# The type of pollutant can be: "so2", "co", "nox", "no2",
#                               "no", "o3", "pm10", "pm2", "wsp", "wdr", "tmp", "rh"
pm_10 <- get_pollution_data("MAXIMOS", "pm10", years)
# Notice the data.frame already has columns max, min, mean, median
knitr::kable(head(pm_10))
```

| date       |  ACO|  AJU|  ATI|  CAM|  CES|  CHA|  CHO|  COY|  CUA|  CUT|  FAC|  HGM|  IZT|  LLA|  LPR|  LVI|  MER|  NEZ|  PED|  PLA|  SAG|  SFE|  SJA|  SUR|  TAH|  TAX|  TEC|  TLA|  TLI|  TPN|  UAX|  UIZ|  VIF|  XAL|  CCA|  max|  min|      mean|  median|
|:-----------|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|---------:|-------:|
| 2005-01-01 |   NA|   NA|   NA|   NA|  308|   NA|   NA|   NA|   NA|   NA|  238|   NA|   NA|   NA|   NA|  372|  302|   NA|  206|  207|  396|   NA|   NA|  312|   NA|  298|   NA|  225|   NA|   NA|   NA|   NA|  683|  502|   NA|  683|  206|  352.7143|     308|
| 2005-01-02 |   NA|   NA|   NA|   NA|  112|   NA|   NA|   NA|   NA|   NA|   99|   NA|   NA|   NA|   NA|  137|  115|   NA|   66|   88|  118|   NA|   NA|   96|   NA|  132|   NA|   98|   NA|   NA|   NA|   NA|  137|  133|   NA|  137|   66|  109.5714|     112|
| 2005-01-03 |   NA|   NA|   NA|   NA|  109|   NA|   NA|   NA|   NA|   NA|  165|   NA|   NA|   NA|   NA|  118|  130|   NA|   76|  135|  173|   NA|   NA|  136|   NA|  106|   NA|  156|   NA|   NA|   NA|   NA|  229|  191|   NA|  229|   76|  144.9286|     136|
| 2005-01-04 |   NA|   NA|   NA|   NA|  120|   NA|   NA|   NA|   NA|   NA|  166|   NA|   NA|   NA|   NA|  150|  157|   NA|   80|  150|  235|   NA|   NA|  108|   NA|  118|   NA|  178|   NA|   NA|   NA|   NA|  304|  232|   NA|  304|   80|  170.1429|     157|
| 2005-01-05 |   NA|   NA|   NA|   NA|  143|   NA|   NA|   NA|   NA|   NA|  182|   NA|   NA|   NA|   NA|  176|  151|   NA|  139|  189|  186|   NA|   NA|  185|   NA|  281|   NA|  160|   NA|   NA|   NA|   NA|  323|  231|   NA|  323|  139|  200.5714|     185|
| 2005-01-06 |   NA|   NA|   NA|   NA|  157|   NA|   NA|   NA|   NA|   NA|  146|   NA|   NA|   NA|   NA|  201|  155|   NA|  122|  161|  274|   NA|   NA|  121|   NA|  121|   NA|  129|   NA|   NA|   NA|   NA|  272|  285|   NA|  285|  121|  182.1429|     157|

``` r
# Monthly median of maximums
pm10_m <- pm_10 %>%
  group_by(month(date), year(date)) %>%
  summarise(max = median(max, na.rm = TRUE))


pm10_ts <- ts(pm10_m$max, start = years[1], freq = 12)

#Seasonally adjusted (by trading day and easter holiday)
m <- seas(pm10_ts,
          regression.variables = c("td1coef", "easter[1]"))
plot(m)
```

![](readme_files/figure-markdown_github/unnamed-chunk-1-1.png)<!-- -->
