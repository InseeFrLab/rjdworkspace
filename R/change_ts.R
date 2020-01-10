#' Change the input time series from a SaItem
#'
#' Function change the input time series from a SaItem
#'
#' @param ts the new \code{\link[stats]{ts}} object.
#' @param sa_item the SaItem to modify.
#'
#' @return a SaItem
#' @export
change_ts <- function(ts, sa_item){
  sa_def <- .jcall(sa_item, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
  jts <- .jcall(sa_def, "Ldemetra/datatypes/Ts;", "getTs")
  
  builder_ts <- jts$toBuilder()
  builder_ts$data(ts_r2jd(ts))
  jts_temp <- builder_ts$build()
  builder_sa <- sa_def$toBuilder()
  builder_sa$ts(jts_temp)
  sa_item <- builder_sa$build()
  
  new_sa_item <- .jnew("ec/tstoolkit/jdr/ws/SaItem", sa_item)
  new_sa_item
}
