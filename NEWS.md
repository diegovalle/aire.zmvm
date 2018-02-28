# aire.zmvm 0.5.0.9000

## New features

* function `get_station_imeca` for downloading pollution data in IMECAs from each station
* `get_station_data` now can download TMP, WSP, WDR and RH data back to 1986

## Bug fixes and improvements

* Define the geographic zones for measuring pollution in the documentation
* Correct the date ranges for the values needed to declare pollution emergencies in the README
* Messages about measuring stations no longer included in the index are now shown with `message()` instead of `warning()`
* Messages about changes in the way the IMECA is computed are now shown with `message()` instead of `warning()`

## Deprecated and Defunct
* `get_latest_data` is deprecated. You should instead use `get_latest_imeca`. 
* `get_zone_data` is deprecated. You should instead use `get_zone_imeca`.
* `showWarnings` argument to `get_zone_imeca` was deprecated for `show_messages`


# aire.zmvm 0.5.0

* First release

New functionality:

* get_station_data()
* get_zone_data()
* get_latest_data()
* idw360()
* stations data.frame
