# rjdworkspace 0.1.1

* Initial CRAN submission.

## Changes from dev version :

* Globally, the functions which use 2 workspaces now have 2 arguments `ws_from` and `ws_to` which designate the workspace which contains the updated data (`ws_from`) and the workspace to update (`ws_to`). The arguments are in that order. This concerns the functions :
    * `transfer_series` 