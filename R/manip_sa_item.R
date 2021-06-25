#' Manipulate SaItems
#'
#' Functions to remove/replace/add a `sa_item` from/to a multiprocessing.
#'
#' @param mp the multiprocessing.
#' @param pos the index of the `sa_item` to remove or to replace.
#' @param sa_item `sa_item` object.
#'
#' @name manipulate_sa_item
#' @rdname manipulate_sa_item
#' @examples 
#' sa_x13 <- jx13(ipi_c_eu[, "FR"])
#' sa_ts <- jtramoseats(ipi_c_eu[, "FR"])
#' 
#' wk <- new_workspace()
#' mp1 <- new_multiprocessing(wk, "sa1")
#' add_sa_item(wk, "sa1", sa_x13, "X13")
#' add_sa_item(wk, "sa1", sa_ts, "TramoSeats")
#' 
#' sa1 <- get_object(mp1, 1)
#' 
#' remove_sa_item(mp1, 1) # Remove the first model
#' add_new_sa_item(mp1, sa1) # Add the model at the end
#' # To replace the first sa_item by "sa1"
#' replace_sa_item(mp1, 1, sa1)
#' @export
remove_sa_item <- function(mp, pos = 1){
  if(!inherits(mp, "multiprocessing"))
    stop("x must be a multiprocessing")
  
  item <- .jcall(mp, "Ljava/util/List;", "getItems")
  item$remove(as.integer(pos - 1))
  return(invisible(TRUE))
}
#' @name manipulate_sa_item
#' @rdname manipulate_sa_item
#' @export
remove_all_sa_item <- function(mp){
  if(!inherits(mp, "multiprocessing"))
    stop("x must be a multiprocessing")
  item <- .jcall(mp, "Ljava/util/List;", "getItems")
  item$removeAll()
  return(invisible(TRUE))
}
#' @name manipulate_sa_item
#' @rdname manipulate_sa_item
#' @export
replace_sa_item <- function(mp, pos = 1, sa_item){
  if(!inherits(mp, "multiprocessing"))
    stop("x must be a multiprocessing")
  item <- .jcall(mp, "Ljava/util/List;", "getItems")
  if(inherits(sa_item, "sa_item"))
    class(sa_item) <- "jobjRef"
  item$set(as.integer(pos - 1), sa_item)
  return(invisible(TRUE))
}

#' @name manipulate_sa_item
#' @rdname manipulate_sa_item
#' @export
add_new_sa_item <- function(mp, sa_item){
  if(!inherits(mp, "multiprocessing"))
    stop("x must be a multiprocessing")
  item <- .jcall(mp, "Ljava/util/List;", "getItems")
  if(inherits(sa_item, "sa_item"))
    class(sa_item) <- "jobjRef"
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
#' sa_x13 <- jx13(ipi_c_eu[, "FR"])
#' 
#' wk <- new_workspace()
#' mp1 <- new_multiprocessing(wk, "sa1")
#' add_sa_item(wk, "sa1", sa_x13, "tramo seats")
#' sa1 <- get_object(mp1, 1)
#' 
#' new_sa <- set_name(sa1, "X13")
#' replace_sa_item(mp1, 1, new_sa)
#' # The first sa_item of mp1 is now "X13"
#' RJDemetra::get_name(get_object(mp1, 1))
#' 
#' @return a new `"sa_item"` with the new name.
#' @export
set_name <- function(sa_item, name){
  sa_def <- .jcall(sa_item, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
  jts <- .jcall(sa_def, "Ldemetra/datatypes/Ts;", "getTs")
  
  jts_temp <- builder_from_ts(jts, name = name)
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
#' sa_x13 <- jx13(ipi_c_eu[, "FR"])
#' sa_ts <- jtramoseats(ipi_c_eu[, "FR"])
#' 
#' wk <- new_workspace()
#' mp1 <- new_multiprocessing(wk, "sa1")
#' add_sa_item(wk, "sa1", sa_x13, "tramo seats")
#' sa1 <- get_object(mp1, 1)
#' 
#' new_sa <- set_spec(sa1, sa_ts)
#' # The first sa_item is now seasonally adjusted with TRAMO-SEATS
#' replace_sa_item(mp1, 1, new_sa)
#' 
#' @return a new `"sa_item"` with the new specification
#' @export
set_spec <- function(sa_item, spec){
  sa_def <- .jcall(sa_item, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
  
  sa_item <- builder_from_sa(sa_def, estimationSpec = RJDemetra::get_jspec(spec))
  new_sa_item <- .jnew("ec/tstoolkit/jdr/ws/SaItem", sa_item)
  new_sa_item
}
