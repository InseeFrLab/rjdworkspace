# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [Unreleased]

### Fixed

* Function `update_xml_file()` updated with a REGEX

### Changed

* Function `update_path()` is now based on the .xml files and not on the SAP position or name


## [1.1.9] - 2025-01-16

### Fixed

* changing the meta-character &amp; into & in `update_xml_file()`


## [1.1.8] - 2025-01-09

### Fixed

* `update_path()` keeps the imports options when updating a path

### Added

* New logo for {rjdworkspace}


## [1.1.7] - 2024-05-22

### Added

* CRAN release 1.1.7
* New documentation and new examples
* `verbose` argument to print more indication. It replaces the argument `print_indication()`.
* New logo for rjdworkspace

### Fixed

* Bug in update_path with the meta character ` ` (which can be encode as `%20` or as `+`)

### Changed

* `copy_ws()` is now exported
* format the file NEWS.md
* new path related to rjdverse


## [1.1.6] - 2024-02-14

### Added

* add a new hidden function to copy ws

### Changed

* simplify the XML path writting with `URLencode()`

### Fixed

* resolve bugs for `update_path()` with WS created with `RJDemetra::add_sa_item()`


## [1.1.3] - 2023-08-29

### Added

* Adding some documentation to functions
* Successfull check!

### Changed

* Changes in `transfer_series()` to regroup `transfer_series()` and `replace_series()` with new arguments
    * `ws_from`b and `ws_to` to identify the workspaces in which we take the data and to which we transfer the data
    * `name_mp_from`, `name_mp_to`, `pos_mp_from` and `pos_mp_to` to identify the SA-Processings (or Multi-Processings denoted mp)
    * `create_mp` to create a new Multi-Processing
    * `replace_series()` to replace existing series
    
### Fixed

* Bug fixed in `get_comment()`


## [1.1.0] - 2023-07-04

### Added

* add new function to `update_path()` of **workspaces**, **SAProcessing** and specifically **Sa-items** for csv, xls and xlsx files


## [1.0.0] - 2023-07-04

### Changed

* Globally, the functions which use 2 workspaces now have 2 arguments `ws_from` and `ws_to` which designate the workspace which contains the updated data (`ws_from`) and the workspace to update (`ws_to`). The arguments are in that order. 
* This concerns the functions :
    * `transfer_series`
    * `replace_series`
    * `update_metadata`
    * `update_metadata_roughly`
    * `set_metadata` for SA ITEM
* the function `replace_series` has an new argument `mp_from_name` and `mp_to_name` to specify the multipreprocessing in which to search for series


[Unreleased]: https://github.com/InseeFrLab/rjdworkspace/compare/v1.1.9...HEAD
[1.1.9]: https://github.com/InseeFrLab/rjdworkspace/compare/v1.1.8...v1.1.9
[1.1.8]: https://github.com/InseeFrLab/rjdworkspace/compare/v1.1.7...v1.1.8
[1.1.7]: https://github.com/InseeFrLab/rjdworkspace/compare/v1.1.6...v1.1.7
[1.1.6]: https://github.com/InseeFrLab/rjdworkspace/compare/v1.1.3...v1.1.6
[1.1.3]: https://github.com/InseeFrLab/rjdworkspace/compare/v1.1.0...v1.1.3
[1.1.0]: https://github.com/InseeFrLab/rjdworkspace/compare/v1.0.0...v1.1.0
[1.0.0]: https://github.com/InseeFrLab/rjdworkspace/releases/tag/v1.0.0
