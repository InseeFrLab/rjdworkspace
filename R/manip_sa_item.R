#' Manipulate SaItems
#'
#' Functions to remove/replace/add a SAItem from/to a multiprocessing
#'
#' @param x the multiprocessing.
#' @param pos the index of the SaItem to remove or to replace.
#' @param sa_item SaItem object.
#'
#' @name manipulate_sa_item
#' @rdname manipulate_sa_item
#' @export
remove_sa_item <- function(x, pos = 1){
  if(!inherits(x, "multiprocessing"))
    stop("x must be a multiprocessing")
  
  item <- .jcall(x, "Ljava/util/List;", "getItems")
  item$remove(as.integer(pos - 1))
  return(invisible(TRUE))
}
#' @name manipulate_sa_item
#' @rdname manipulate_sa_item
#' @export
remove_all_sa_item <- function(x){
  if(!inherits(x, "multiprocessing"))
    stop("x must be a multiprocessing")
  item <- .jcall(x, "Ljava/util/List;", "getItems")
  item$removeAll()
  return(invisible(TRUE))
}
#' @name manipulate_sa_item
#' @rdname manipulate_sa_item
#' @export
replace_sa_item <- function(x, pos = 1, sa_item){
  if(!inherits(x, "multiprocessing"))
    stop("x must be a multiprocessing")
  item <- .jcall(x, "Ljava/util/List;", "getItems")
  if(inherits(sa_item, "sa_item"))
    class(sa_item) <- "jobjRef"
  item$set(as.integer(pos - 1), sa_item)
  return(invisible(TRUE))
}

#' @name manipulate_sa_item
#' @rdname manipulate_sa_item
#' @export
add_new_sa_item <- function(x, sa_item){
  if(!inherits(x, "multiprocessing"))
    stop("x must be a multiprocessing")
  item <- .jcall(x, "Ljava/util/List;", "getItems")
  if(inherits(sa_item, "sa_item"))
    class(sa_item) <- "jobjRef"
  return(invisible(item$add(sa_item)))
}
