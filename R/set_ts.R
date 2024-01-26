#' Change the input time series of a SaItem
#'
#' Function to change the input time series of a SaItem
#'
#' @param ts the new [stats::ts()] object.
#' @param sa_item the `sa_item` to modify.
#'
#' @return a `sa_item`
#' @examples
#'
#' library("RJDemetra")
#'
#' # Definition of the original time series
#' sa_x13 <- jx13(series = ipi_c_eu[, "FR"])
#'
#' wk <- new_workspace()
#' sap1 <- new_multiprocessing(workspace = wk, name = "sap-1")
#'
#' # Adding a new SA-Item (`sa_x13`) to `sap1`
#' add_sa_item(workspace = wk, multiprocessing = "sap-1",
#'             sa_obj = sa_x13, name = "X13")
#'
#' # Retrieving the adjusted series
#' sa_item1 <- get_object(x = sap1, pos = 1L)
#'
#' # Creation of a new sa_item and change of the input time series
#' new_sa_item <- set_ts(sa_item = sa_item1, ts = ipi_c_eu[, "BE"])
#' # Replacement of the series in the workspace
#' replace_sa_item(sap = sap1, pos = 1L, sa_item = new_sa_item)
#'
#' @export
set_ts <- function(sa_item, ts) {
    sa_def <- .jcall(
        obj = sa_item,
        returnSig = "Ljd2/datatypes/sa/SaItemType;",
        method = "getSaDefinition"
    )
    jts <- .jcall(
        obj = sa_def,
        returnSig = "Ljd2/datatypes/Ts;",
        method = "getTs"
    )

    # builder_ts <- jts$toBuilder()
    # builder_ts$data(ts_r2jd(ts))
    # jts_temp <- builder_ts$build()
    # builder_sa <- sa_def$toBuilder()
    # builder_sa$ts(jts_temp)
    # sa_item <- builder_sa$build()

    jts_temp <- builder_from_ts(jts, data = ts_r2jd(ts))
    sa_item <- builder_from_sa(sa_def, ts = jts_temp)
    new_sa_item <- .jnew("ec/tstoolkit/jdr/ws/SaItem", sa_item)
    new_sa_item
}
