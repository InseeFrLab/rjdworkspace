
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rjdworskpace

[![Travis Build
Status](https://img.shields.io/travis/AQLT/rjdworkspace.svg?logo=travis)](https://travis-ci.org/AQLT/rjdworkspace)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rjdworkspace)](https://cran.r-project.org/package=rjdworkspace)

## Overview

rjdworkspace gives a set of tools to manipulate `JDemetra+` workspaces.
It depends on the .jar files of
[RJDemetra](https://github.com/nbbrd/rjdemetra). In particular it allows
to update the metadata of a workspace from those contained in another
one with the `update_metadata()` function.

## Installation

rjdworkspace relies on RJDemetra that requires Java SE 8 or later
version.

``` r
# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("AQLT/rjdworkspace")
```
