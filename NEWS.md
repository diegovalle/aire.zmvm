# aire.zmvm 0.8.2

## New features

* Added new stations FAR and SAC to the `stations` data.frame

## Bug fixes and improvements

* Fixes `get_station_imeca` because of a change in the website

# aire.zmvm 0.8.1

## Bug fixes and improvements

* `get_zone_imeca` and `get_latest_imeca` now return NULL when aire.cdmx.gob.mx is down
* `convert_to_index` added 'PELIGROSA' category
* `convert_to_imeca` uses NADF-009-AIRE-2017 to convert concentration units to IMECA

# aire.zmvm 0.8.0

## New features

* `download_24hr_average` Download archives of the 24 hour averages of PM10 and SO2
* `download_deposition` Download Rainfall Samples Archives
* `download_lead` Download Lead Pollution Archives
* `download_meteorological` Download Meteorological Data Archives
* `download_pollution` Download Pollution Archives
* `download_pressure` Download Atmospheric Pressure Archives
* `download_radiation` Download Ultraviolet Radiation Archives
* `convert_to_index` Convert a concentration value to one of 5 categories

## Bug fixes and improvements

* The get_*_data functions now use https://github.com/diegovalle/aire.zmvm as referrer
* Since there are errors in the wind speed data some functions show a warning or prohibit downloading if certain time ranges are requested


# aire.zmvm 0.6.1

## Bug fixes and improvements

* Make sure the `get_station_imeca` example is not run


# aire.zmvm 0.6.0

## New features

* function `get_station_imeca` for downloading pollution data in IMECAs from each station
* `get_station_month_data` replaces `get_station_data_monthly`. Allows for downloading daily
maximums and daily minimums
* `get_station_data` now can download TMP, WSP, WDR and RH data back to 1986
* `zones` data.frame with the municipios belonging to each geographic zone of Mexico city

## Bug fixes and improvements

* Define the geographic zones for measuring pollution in the documentation
* Correct the date ranges for the values needed to declare pollution emergencies in the README
* Messages about measuring stations no longer included in the index are now shown with `message()` instead of `warning()`
* Messages about changes in the way the IMECA is computed are now shown with `message()` instead of `warning()`
* `get_station_data` no longer gives a warning when downloading data from 2012 to 2015
* `get_station_data` progress bar now works correctly
* `get_station_month_data` warnings when data doesn't match the archives

## Deprecated and Defunct
* `get_latest_data` is deprecated. You should instead use `get_latest_imeca`.
* `get_zone_data` is deprecated. You should instead use `get_zone_imeca`.
* `get_station_data_monthly` is deprecated. You should instead use `get_station_month_data`.
* `showWarnings` argument to `get_zone_imeca` was deprecated for `show_messages`.


# aire.zmvm 0.5.0

* First release

New functionality:

* get_station_data()
* get_zone_data()
* get_latest_data()
* idw360()
* stations data.frame
