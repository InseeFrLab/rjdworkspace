#' Update the metadata from a workspace
#'
#' Functions to update the metadata of a workspace by those contained in another
#'
#' @param path_workspace1 Path to the workspace that contains the metadata.
#' @param path_workspace2 Path to the workspace to update metadata.
#' @param path_new_workspace Path to the new workspace.
#'
#'
#' @details \code{update_metadata()} checks the names of the multiprocessings and the SaItem of
#' the two workspaces to update metadata. \code{update_metadata_roughly()} does not do any
#' checking: the metadata of the first SaItem of the first multiprocessings
#' of the \code{workspace2} is updated with the metadata of the first SaItem of the
#' first multiprocessings of the \code{workspace2}.
#'
#' @examples \dontrun{
#' path_workspace1 <- "D:/test_metadata/reference.xml"
#' path_workspace2 <- "D:/test_metadata/r_wk.xml"
#' path_new_workspace <- "D:/test_metadata/r_wk_metadata.xml"
#' update_metadata_roughly(path_workspace1, path_workspace2, path_new_workspace)
#' update_metadata(path_workspace1, path_workspace2, path_new_workspace)
#' }
#'
#' @name update_metadata
#' @rdname update_metadata
#' @export
update_metadata <- function(path_workspace1, path_workspace2,
                            path_new_workspace){
  wk1 <- RJDemetra::load_workspace(path_workspace1)
  wk2 <- RJDemetra::load_workspace(path_workspace2)
  all_mp_wk1 <- RJDemetra::get_all_objects(wk1)
  all_mp_wk1_names <- names(all_mp_wk1)

  # mp_wk2 <- all_mp_wk2[[1]]
  # i_mp <- 2
  for(i_mp in seq_len(RJDemetra::count(wk2))){
    mp_wk2 <- RJDemetra::get_object(wk2, i_mp)
    mp_name <- RJDemetra::get_name(mp_wk2)
    mp_wk1_i <- which(all_mp_wk1_names %in% mp_name)
    if(length(mp_wk1_i) > 0){
      if(length(mp_wk1_i) > 1)
        warning(sprintf('At least 2 multiprocessing called "%s" were founded in the workspace1: the first object will be used', mp_name))

      mp_wk1 <- all_mp_wk1[[mp_wk1_i[1]]]
      all_sa_wk1_names <- names(RJDemetra::get_all_objects(mp_wk1))
      for(i_sa in seq_len(RJDemetra::count(mp_wk2))){
        # i_sa <- 2
        sa_wk2 <- RJDemetra::get_object(mp_wk2, i_sa)
        sa_name <- RJDemetra::get_name(sa_wk2)
        sa_wk1_i <- which(all_sa_wk1_names %in% sa_name)
        if(length(sa_wk1_i) > 0){
          if(length(sa_wk1_i) > 1)
            warning(sprintf('At least 2 SaItem called "%s" were founded in the workspace1: the first object will be used',sa_name))

          sa_wk1 <- RJDemetra::get_object(mp_wk1, sa_wk1_i[1])

          sa_def1 <- .jcall(sa_wk1, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
          jts1 <- .jcall(sa_def1, "Ldemetra/datatypes/Ts;", "getTs")

          sa_def2 <- .jcall(sa_wk2, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
          jts2 <- .jcall(sa_def2, "Ldemetra/datatypes/Ts;", "getTs")
          
          builder_ts <- jts2$toBuilder()
          builder_ts$metaData(jts1$getMetaData())
          jts_temp <- builder_ts$build()
          builder_sa <- sa_def2$toBuilder()
          builder_sa$ts(jts_temp)
          builder_sa$metaData(sa_def1$getMetaData())
          sa_def_temp <- builder_sa$build()

          new_sa_item <- .jnew("ec/tstoolkit/jdr/ws/SaItem", sa_def_temp)
          replace_sa_item(x = mp_wk2, sa_item = new_sa_item,
                          pos = i_sa)

        }else{
          warning(sprintf('The SaItem "%s" is not found in the workspace1',sa_name))
        }
      }

    }else{
      warning(sprintf('The multiprocessing "%s" is not found in the workspace1',mp_name))
    }
  }
  RJDemetra::save_workspace(wk2, path_new_workspace)
}
#' @name update_metadata
#' @rdname update_metadata
#' @export
update_metadata_roughly <- function(path_workspace1, path_workspace2,
                                    path_new_workspace){
  wk1 <- RJDemetra::load_workspace(path_workspace1)
  wk2 <- RJDemetra::load_workspace(path_workspace2)

  for(i_mp in seq_len(RJDemetra::count(wk2))){
    # i_mp <- i_sa <- 1
    mp_wk2 <- RJDemetra::get_object(wk2, i_mp)
    mp_wk1 <- RJDemetra::get_object(wk1, i_mp)

    for(i_sa in seq_len(RJDemetra::count(mp_wk2))){
      sa_wk2 <- RJDemetra::get_object(mp_wk2, i_sa)
      sa_wk1 <- RJDemetra::get_object(mp_wk1, i_sa)
      sa_def1 <- .jcall(sa_wk1, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
      jts1 <- .jcall(sa_def1, "Ldemetra/datatypes/Ts;", "getTs")

      sa_def2 <- .jcall(sa_wk2, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
      jts2 <- .jcall(sa_def2, "Ldemetra/datatypes/Ts;", "getTs")

      builder_ts <- jts2$toBuilder()
      builder_ts$metaData(jts1$getMetaData())
      jts_temp <- builder_ts$build()

      builder_sa <- sa_def2$toBuilder()
      builder_sa$ts(jts_temp)
      builder_sa$metaData(sa_def1$getMetaData())
      sa_def_temp <- builder_sa$build()

      new_sa_item <- .jnew("ec/tstoolkit/jdr/ws/SaItem", sa_def_temp)
      replace_sa_item(x = mp_wk2, sa_item = new_sa_item,
                      pos = i_sa)
    }
  }
  RJDemetra::save_workspace(wk2, path_new_workspace)
}


#' Extract comments
#' 
#' Function to extract the comments of a workspace
#' 
#' @param x the object where to get the comments.
#' 
#' @export
get_comment <- function(x){
  UseMethod("get_comment", x)
}
#' @export
get_comment.workspace <- function(x){
  multiprocessings <- RJDemetra::get_all_objects(x)
  lapply(multiprocessings, get_comment)
}
#' @export
get_comment.multiprocessing <- function(x){
  all_sa_objects <- RJDemetra::get_all_objects(x)
  lapply(all_sa_objects, get_comment)
}
#' @export
get_comment.sa_item <- function(x){
  sa_def <- .jcall(x, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
  metadata <- sa_def$getMetaData()
  metadata$get("comment")
}

#' Change comment
#' 
#' Function to change the comments of a sa_item object
#' 
#' @param x the sa_item where to change the comments.
#' @param comment the new comment.
#' 
#' @return a new sa_item.
#' 
#' @export
set_comment <- function(x, comment){
  UseMethod("set_comment", x)
}
#' @export
set_comment.sa_item <- function(x, comment){
  sa_def <- .jcall(x, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
  jts2 <- .jcall(sa_def, "Ldemetra/datatypes/Ts;", "getTs")
  
  metadata <- sa_def$getMetaData()
  
  jmap<-.jnew("java/util/LinkedHashMap")
  jmap <- .jcast(jmap, "java/util/Map")
  jmap$putAll(metadata)
  jmap$put("comment", comment)
  
  builder_ts <- jts2$toBuilder()
  builder_ts$metaData(jts2$getMetaData())
  jts_temp <- builder_ts$build()
  builder_sa <- sa_def$toBuilder()
  builder_sa$ts(jts_temp)
  builder_sa$metaData(jmap)
  sa_def_temp <- builder_sa$build()

  new_sa_item <- .jnew("ec/tstoolkit/jdr/ws/SaItem", sa_def_temp)
  new_sa_item
}

