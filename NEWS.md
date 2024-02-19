# rjdworkspace dev version

# rjdworkspace 1.1.6

* add a new hidden function to copy ws
* simplify the XML path writting with `URLencode`
* resolve bugs for `update_path` with WS created with `RJDemetra::add_sa_item`


# rjdworkspace 1.1.3

* Bug fixed in `get_comment`
* Changes in `transfer_series` to regroup `transfer_series` and `replace_series` with new arguments
    * ws_from and ws_to to identify the workspaces in which we take the data and to which we transfer the data
    * name_mp_from, name_mp_to, pos_mp_from and pos_mp_to to identify the SA-Processings (or Multi-Processings denoted mp)
    * create_mp to create a new Multi-Processing
    * replace_series to replace existing series
* Adding some documentation to functions
* Successfull check!


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

* the function `replace_series` has an new argument `mp_from_name` and `mp_to_name` to specify the multipreprocessing in which to search for series
