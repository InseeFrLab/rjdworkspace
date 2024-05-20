#' Manipulate SaItems
#'
#' Functions to remove/replace/add a `sa_item` from/to a SA-Processing.
#'
#' @param sap the SA-Processing.
#' @param pos the index of the `sa_item` to remove or to replace.
#' @param sa_item `sa_item` object.
#'
#' @returns The functions \code{remove_sa_item()}, \code{remove_all_sa_item()} and \code{replace_sa_item()} return invisibly (with \code{invisible()}) \code{TRUE} or an error.
#' The function \code{add_new_sa_item()} returns invisibly (with \code{invisible()}) the updated `SA-Item`.
#'
#' @name manipulate_sa_item
#' @rdname manipulate_sa_item
#' @examples
#'
#' library("RJDemetra")
#'
#' sa_x13 <- jx13(series = ipi_c_eu[, "FR"])
#' sa_ts <- jtramoseats(series = ipi_c_eu[, "FR"])
#'
#' wk <- new_workspace()
#' sap1 <- new_multiprocessing(workspace = wk, name = "sap-1")
#' add_sa_item(workspace = wk, multiprocessing = "sap-1",
#'             sa_obj = sa_x13, name = "X13")
#' add_sa_item(workspace = wk, multiprocessing = "sap-1",
#'             sa_obj = sa_ts, name = "TramoSeats")
#'
#' sa_item1 <- get_object(x = sap1, pos = 1L)
#'
#' remove_sa_item(sap = sap1, pos = 1L) # Remove the first sa-item
#' add_new_sa_item(sap = sap1, sa_item = sa_item1) # Add the sa-item at the end
#'
#' # To replace the first sa_item by "sa_item1"
#' replace_sa_item(sap = sap1, pos = 1L, sa_item = sa_item1)
#' @export
remove_sa_item <- function(sap, pos = 1) {
    if (!inherits(sap, "multiprocessing")) {
        stop("x must be a multiprocessing")
    }

    item <- .jcall(sap, "Ljava/util/List;", "getItems")
    item$remove(as.integer(pos - 1))
    return(invisible(TRUE))
}
#' @name manipulate_sa_item
#' @rdname manipulate_sa_item
#' @export
remove_all_sa_item <- function(sap) {
    if (!inherits(sap, "multiprocessing")) {
        stop("x must be a multiprocessing")
    }
    item <- .jcall(sap, "Ljava/util/List;", "getItems")
    item$clear()
    return(invisible(TRUE))
}
#' @name manipulate_sa_item
#' @rdname manipulate_sa_item
#' @export
replace_sa_item <- function(sap, pos = 1, sa_item) {
    if (!inherits(sap, "multiprocessing")) {
        stop("x must be a multiprocessing")
    }
    item <- .jcall(sap, "Ljava/util/List;", "getItems")
    if (inherits(sa_item, "sa_item")) {
        class(sa_item) <- "jobjRef"
    }
    item$set(as.integer(pos - 1), sa_item)
    return(invisible(TRUE))
}

#' @name manipulate_sa_item
#' @rdname manipulate_sa_item
#' @export
add_new_sa_item <- function(sap, sa_item) {
    if (!inherits(sap, "multiprocessing")) {
        stop("x must be a multiprocessing")
    }
    item <- .jcall(sap, "Ljava/util/List;", "getItems")
    if (inherits(sa_item, "sa_item")) {
        class(sa_item) <- "jobjRef"
    }
    return(invisible(item$add(sa_item)))
}

#' Set the name of a SaItem
#'
#' Function to set the name of a `"sa_item"`.
#'
#' @param sa_item a `"sa_item"` object.
#' @param name the new name.
#'
#' @examples
#'
#' library("RJDemetra")
#'
#' sa_x13 <- jx13(series = ipi_c_eu[, "FR"])
#'
#' wk <- new_workspace()
#' sap1 <- new_multiprocessing(workspace = wk, name = "sap-1")
#'
#' add_sa_item(workspace = wk, multiprocessing = "sap-1",
#'             sa_obj = sa_x13, name = "Wrong name")
#'
#' sa_item1 <- get_object(x = sap1, pos = 1L)
#'
#' new_sa_item <- set_name(sa_item = sa_item1, name = "Good name")
#' replace_sa_item(sap = sap1, pos = 1L, sa_item = new_sa_item)
#'
#' # The first sa_item of sap1 is now "Good name"
#' get_name(x = get_object(x = sap1, pos = 1L))
#'
#' @return a new `"sa_item"` with the new name.
#' @export
set_name <- function(sa_item, name) {
    sa_def <- .jcall(
        obj = sa_item,
        returnSig = "Ljd2/datatypes/sa/SaItemType;",
        method = "getSaDefinition"
    )
    # jts <- .jcall(sa_def, "Ljd2/datatypes/Ts;", "getTs")

    # jts_temp <- builder_from_ts(jts, name = name)
    sa_item <- builder_from_sa(sa_def, name = name)
    new_sa_item <- .jnew("ec/tstoolkit/jdr/ws/SaItem", sa_item)
    new_sa_item
}
#' Set the specification of a SaItem
#'
#' Function to set the specification of a `"sa_item"`.
#'
#' @param sa_item a `"sa_item"` object.
#' @param spec the object into which the new specification is extracted/stored.
#'
#' @examples
#'
#' library("RJDemetra")
#'
#' sa_x13 <- jx13(series = ipi_c_eu[, "FR"])
#' sa_ts <- jtramoseats(series = ipi_c_eu[, "FR"])
#'
#' wk <- new_workspace()
#' sap1 <- new_multiprocessing(workspace = wk, name = "sap-1")
#'
#' add_sa_item(
#'     workspace = wk,
#'     multiprocessing = "sap-1",
#'     sa_obj = sa_x13,
#'     name = "tramo seats"
#' )
#'
#' sa_item1 <- get_object(x = sap1, pos = 1L)
#' new_sa_item <- set_spec(sa_item = sa_item1, spec = sa_ts)
#'
#' # The first sa_item is now seasonally adjusted with TRAMO-SEATS
#' replace_sa_item(sap = sap1, pos = 1, sa_item = new_sa_item)
#'
#' @return a new `"sa_item"` with the new specification
#' @export
set_spec <- function(sa_item, spec) {
    sa_def <- .jcall(
        obj = sa_item,
        returnSig = "Ljd2/datatypes/sa/SaItemType;",
        method = "getSaDefinition"
    )

    sa_item <- builder_from_sa(
        sa_def = sa_def,
        estimationSpec = RJDemetra::get_jspec(spec)
    )
    new_sa_item <- .jnew("ec/tstoolkit/jdr/ws/SaItem", sa_item)
    new_sa_item
}
