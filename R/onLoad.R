#' @import rJava
#' @import RJDemetra
.onLoad <- function(libname, pkgname) {
    .jpackage(pkgname, lib.loc = libname)
}
