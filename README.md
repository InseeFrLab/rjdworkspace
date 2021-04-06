
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rjdworskpace

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rjdworkspace)](https://cran.r-project.org/package=rjdworkspace)

## Overview

rjdworkspace gives a set of tools to manipulate `JDemetra+` workspaces.
It depends on the .jar files of
[RJDemetra](https://github.com/nbbrd/rjdemetra) and extends some
functions. In particular, rjdworkspace allows to:

  - update all metadata of a workspace with those contained in another
    one (`update_metadata()`, `update_metadata_roughly()`) or update the
    metadata of a SaItem with the metadata contained in another SaItem
    (`set_metadata()`) ;  
  - replace, remove or add a SaItem in a workspace (`remove_sa_item()`,
    `remove_all_sa_item()`, `replace_sa_item()`, `replace_series()`,
    `add_new_sa_item()`);  
  - get and set the comment of a SaItem (`get_comment()`,
    `set_comment()`);
  - set the specification of a model contained in a SaItem
    (`set_spec()`).

## Installation

rjdworkspace relies on RJDemetra that requires Java SE 8 or later
version.

``` r
# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("InseeFrLab/rjdworkspace")
```
