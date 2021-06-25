#' Update the metadata from a workspace
#'
#' Functions to update the metadata of a workspace by those contained in another one
#'
#' @param workspace1 Workspace that contains the new metadata.
#' @param workspace2 Workspace to update.
#'
#'
#' @details `update_metadata()` checks the multiprocessings and SaItems' names within
#' the two workspaces before updating workspace2's metadata. `update_metadata_roughly()` does not do any
#' checks: `workspace2`'s first multiprocessing's first SaItem metadata 
#' is updated with `workspace1`'s first multiprocessing's first SaItem metadata.
#' Both functions create and return a new workspace containing the updated series.
#'
#' @examples \dontrun{
#' workspace1_ <- load_workspace("D:/test_metadata/reference.xml")
#' workspace1  <- compute(workspace1_)
#' workspace2_ <- load_workspace("D:/test_metadata/ws_wk.xml")
#' workspace2  <- compute(workspace2_)
#' 
#' updated_workspace <- update_metadata_roughly(workspace1, workspace2)
#' save_workspace(updated_workspace, "D:/test_metadata/ws_wk_metadata.xml")
#' 
#' updated_workspace <- update_metadata(workspace1, workspace2)
#' save_workspace(updated_workspace, "D:/test_metadata/ws_wk_metadata.xml")
#' }
#'
#' @name update_metadata
#' @rdname update_metadata
#' @export
update_metadata <- function(workspace1, workspace2){
  # wk1 <- RJDemetra::load_workspace(workspace1)
  # wk2 <- RJDemetra::load_workspace(workspace2)
  all_mp_wk1 <- RJDemetra::get_all_objects(workspace1)
  all_mp_wk1_names <- names(all_mp_wk1)

  for(i_mp in seq_len(RJDemetra::count(workspace2))){
    mp_wk2 <- RJDemetra::get_object(workspace2, i_mp)
    mp_name <- RJDemetra::get_name(mp_wk2)
    mp_wk1_i <- which(all_mp_wk1_names %in% mp_name)
    if(length(mp_wk1_i) > 0){
      if(length(mp_wk1_i) > 1)
        warning(sprintf('At least 2 multiprocessing called "%s" were found in the workspace1: the first object will be used', mp_name))

      mp_wk1 <- all_mp_wk1[[mp_wk1_i[1]]]
      all_sa_wk1_names <- names(RJDemetra::get_all_objects(mp_wk1))
      for(i_sa in seq_len(RJDemetra::count(mp_wk2))){
        # i_sa <- 2
        sa_wk2 <- RJDemetra::get_object(mp_wk2, i_sa)
        sa_name <- RJDemetra::get_name(sa_wk2)
        sa_wk1_i <- which(all_sa_wk1_names %in% sa_name)
        if(length(sa_wk1_i) > 0){
          if(length(sa_wk1_i) > 1)
            warning(sprintf('At least 2 SaItem called "%s" were found in the workspace1: the first object will be used',sa_name))

          sa_wk1 <- RJDemetra::get_object(mp_wk1, sa_wk1_i[1])
          new_sa_item <- set_metadata(sa_to = sa_wk2,
                                      sa_from = sa_wk1)
          replace_sa_item(mp = mp_wk2, sa_item = new_sa_item,
                          pos = i_sa)
# 
#           sa_def1 <- .jcall(sa_wk1, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
#           jts1 <- .jcall(sa_def1, "Ldemetra/datatypes/Ts;", "getTs")
# 
#           sa_def2 <- .jcall(sa_wk2, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
#           jts2 <- .jcall(sa_def2, "Ldemetra/datatypes/Ts;", "getTs")
#           
          # builder_ts <- jts2$toBuilder()
          # builder_ts$metaData(jts1$getMetaData())
          # jts_temp <- builder_ts$build()
          # builder_sa <- sa_def2$toBuilder()
          # builder_sa$ts(jts_temp)
          # builder_sa$metaData(sa_def1$getMetaData())
          # sa_def_temp <- builder_sa$build()
# 
#           jts_temp <- builder_from_ts(jts2, metaData =  jts1$getMetaData())
#           sa_def_temp <- builder_from_sa(sa_def2, ts = jts_temp,
#                                          metaData = sa_def1$getMetaData())
#           
#           new_sa_item <- .jnew("ec/tstoolkit/jdr/ws/SaItem", sa_def_temp)
#           replace_sa_item(mp = mp_wk2, sa_item = new_sa_item,
#                           pos = i_sa)
#           sa_wk2 <- RJDemetra::get_object(mp_wk2, i_sa)
#           sa_wk1 <- RJDemetra::get_object(mp_wk1, i_sa)
          # builder_ts <- jts2$toBuilder()
          # builder_ts$metaData(jts1$getMetaData())
          # jts_temp <- builder_ts$build()
          # 
          # builder_sa <- sa_def2$toBuilder()
          # builder_sa$ts(jts_temp)
          # builder_sa$metaData(sa_def1$getMetaData())
          # sa_def_temp <- builder_sa$build()


        }else{
          warning(sprintf('The SaItem "%s" is not found in the workspace1',sa_name))
        }
      }

    }else{
      warning(sprintf('The multiprocessing "%s" is not found in the workspace1',mp_name))
    }
  }
  return(workspace2)
}
#' @name update_metadata
#' @rdname update_metadata
#' @export
update_metadata_roughly <- function(workspace1, workspace2){
  # wk1 <- RJDemetra::load_workspace(workspace1)
  # wk2 <- RJDemetra::load_workspace(workspace2)

  for(i_mp in seq_len(RJDemetra::count(workspace2))){
    mp_wk2 <- RJDemetra::get_object(workspace2, i_mp)
    mp_wk1 <- RJDemetra::get_object(workspace1, i_mp)

    for(i_sa in seq_len(RJDemetra::count(mp_wk2))){
      sa_wk2 <- RJDemetra::get_object(mp_wk2, i_sa)
      sa_wk1 <- RJDemetra::get_object(mp_wk1, i_sa)
      # builder_ts <- jts2$toBuilder()
      # builder_ts$metaData(jts1$getMetaData())
      # jts_temp <- builder_ts$build()
      # 
      # builder_sa <- sa_def2$toBuilder()
      # builder_sa$ts(jts_temp)
      # builder_sa$metaData(sa_def1$getMetaData())
      # sa_def_temp <- builder_sa$build()
      new_sa_item <- set_metadata(sa_to = sa_wk2,
                                  sa_from = sa_wk1)
      replace_sa_item(mp = mp_wk2, sa_item = new_sa_item,
                      pos = i_sa)
    }
  }
  return(workspace2)
}

#' Set the metadata of a SaItem
#' 
#' Function to set the name of a `"sa_item"` from the one contained in another `"sa_item"`.
#' 
#' @param sa_to the `"sa_item"` object to modify.
#' @param sa_from the `"sa_item"` object from which the desired metadata is retrieved.
#' 
#' @return a new `"sa_item"` with the model of `sa_to` and the metadata of `sa_from`.
#' @export
set_metadata <- function(sa_to, sa_from){
  sa_def_from <- .jcall(sa_from, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
  jts_from <- .jcall(sa_def_from, "Ldemetra/datatypes/Ts;", "getTs")
  
  sa_def_to <- .jcall(sa_to, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
  jts_to <- .jcall(sa_def_to, "Ldemetra/datatypes/Ts;", "getTs")
  
  jts_update <- builder_from_ts(jts_to, metaData =  jts_from$getMetaData())
  sa_def_update <- builder_from_sa(sa_def_to, ts = jts_update,
                                   metaData = sa_def_from$getMetaData())
  .jnew("ec/tstoolkit/jdr/ws/SaItem", sa_def_update)
}

#' Extract comments
#' 
#' Function to extract the comments of a workspace
#' 
#' @param x the object from which the comments are retrieved.
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
#' @param x the sa_item of which the comments will be changed.
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
  jts <- .jcall(sa_def, "Ldemetra/datatypes/Ts;", "getTs")
  
  metadata <- sa_def$getMetaData()
  
  jmap<-.jnew("java/util/LinkedHashMap")
  jmap <- .jcast(jmap, "java/util/Map")
  jmap$putAll(metadata)
  jmap$put("comment", comment)
  
  # builder_ts <- jts$toBuilder()
  # builder_ts$metaData(jts$getMetaData())
  # jts_temp <- builder_ts$build()
  # builder_sa <- sa_def$toBuilder()
  # builder_sa$ts(jts)
  # builder_sa$metaData(jmap)
  #sa_def_temp <- builder_sa$build()
  sa_def_temp <- builder_from_sa(sa_def,
                                 metaData = jmap)
  new_sa_item <- .jnew("ec/tstoolkit/jdr/ws/SaItem", sa_def_temp)
  new_sa_item
}

