# rjdworkspace 1.1.0

## Changes :

* add new function to `update_path` of **workspaces**, **SAProcessing** and specifically **Sa-items** for csv, xls and xlsx files


# rjdworkspace 1.0.0 - Initial release.

## Changes from dev version :

* Globally, the functions which use 2 workspaces now have 2 arguments `ws_from` and `ws_to` which designate the workspace which contains the updated data (`ws_from`) and the workspace to update (`ws_to`). The arguments are in that order. 
* This concerns the functions :
    * `transfer_series`
    * `replace_series`
    * `update_metadata`
    * `update_metadata_roughly`
    * `set_metadata` for SA ITEM

* the function `replace_series` has an new argument `mp_from_name` and `mp_to_name` to specified the multipreprocessing in which to search for series
