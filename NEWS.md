# aire.zmvm 0.5.0.9000

## New features

* function `get_station_imeca` for downloading pollution data in IMECAs from each station
* `get_station_data` now can download TMP, WSP, WDR and RH data back to 1986

## Bug fixes and improvements

* Define the geographic zones in the documentation
* Correct the date ranges for the values needed to declare pollution emergencies in the README
* rename `get_latest_data` to `get_latest_imeca` and `get_zone_data` to `get_zone_imeca`


# aire.zmvm 0.5.0

* First release

New functionality:

* get_station_data()
* get_zone_data()
* get_latest_data()
* idw360()
* stations data.frame
