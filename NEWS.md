# aire.zmvm 0.6.1

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
* `get_station_data_monthly` is deprecadet. You should instead use `get_station_month_data`.
* `showWarnings` argument to `get_zone_imeca` was deprecated for `show_messages`.


# aire.zmvm 0.5.0

* First release

New functionality:

* get_station_data()
* get_zone_data()
* get_latest_data()
* idw360()
* stations data.frame
