#' Change the input time series of a SaItem
#'
#' Function to change the input time series of a SaItem
#'
#' @param ts the new [stats::ts()] object.
#' @param sa_item the `sa_item` to modify.
#'
#' @return a `sa_item`
#' @examples 
#' # Definition of the original time series
#' sa_x13 <- jx13(ipi_c_eu[, "FR"])
#' 
#' wk <- new_workspace()
#' mp1 <- new_multiprocessing(wk, "sa1")
#' # Addition of the series to the workspace and seasonal adjustment with the X13 model
#' add_sa_item(wk, "sa1", sa_x13, "X13")
#' # Retrieving the adjusted series
#' sa1 <- get_object(mp1, 1)
#' 
#' # Creation of a new sa_item and change of the input time series
#' new_sa_item <- set_ts(sa1, ipi_c_eu[, "BE"])
#' # Replacement of the series in the workspace
#' replace_sa_item(mp1, 1, new_sa_item)
#' @export
set_ts <- function(sa_item, ts) {
  sa_def <- .jcall(sa_item, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
  jts <- .jcall(sa_def, "Ldemetra/datatypes/Ts;", "getTs")
  
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
