#' Update the metadata from a workspace
#'
#' Functions to update the metadata of a workspace by those contained in another one
#'
#' @param ws_from Workspace that contains the new metadata.
#' @param ws_to Workspace to update.
#'
#' @details `update_metadata()` checks the multiprocessings and SaItems' names within
#' the two workspaces before updating ws_to's metadata. `update_metadata_roughly()` does not do any
#' checks: `ws_to`'s first multiprocessing's first SaItem metadata 
#' is updated with `ws_from`'s first multiprocessing's first SaItem metadata.
#' Both functions create and return a new workspace containing the updated series.
#'
#' @examples
#' 
#' library("RJDemetra")
#' 
#' path_to_ws1 <- file.path(system.file("extdata", package = "rjdworkspace"),
#'                          "WS/ws_example_1.xml")
#' path_to_ws2 <- file.path(system.file("extdata", package = "rjdworkspace"),
#'                          "WS/ws_example_2.xml")
#'  
#' ws_1 <- load_workspace(path_to_ws1)
#' compute(ws_1)
#' ws_2 <- load_workspace(path_to_ws2)
#' compute(ws_2)
#' 
#' updated_workspace <- update_metadata_roughly(ws_from = ws_1, ws_to = ws_2)
#' path_to_output <- file.path(tempfile(), "ws_update_meta_roughly.xml")
#' save_workspace(workspace = updated_workspace, file = path_to_output)
#' 
#' updated_workspace <- update_metadata(ws_from = ws_1, ws_to = ws_2)
#' path_to_output <- file.path(tempfile(), "ws_update_meta.xml")
#' save_workspace(workspace = updated_workspace, file = path_to_output)
#'
#' @name update_metadata
#' @rdname update_metadata
#' @export
update_metadata <- function(ws_from, ws_to) {
    # ws1 <- RJDemetra::load_workspace(ws_from)
    # ws2 <- RJDemetra::load_workspace(ws_to)
    all_mp_ws_from <- RJDemetra::get_all_objects(ws_from)
    all_mp_ws_from_names <- names(all_mp_ws_from)
    
    for (i_mp in seq_len(RJDemetra::count(ws_to))) {
        mp_ws_to <- RJDemetra::get_object(ws_to, i_mp)
        mp_name <- RJDemetra::get_name(mp_ws_to)
        mp_ws_from_i <- which(all_mp_ws_from_names %in% mp_name)
        if (length(mp_ws_from_i) > 0) {
            if (length(mp_ws_from_i) > 1)
                warning(sprintf('At least 2 multiprocessing called "%s" were found in the ws_from: the first object will be used', mp_name))
            
            mp_ws_from <- all_mp_ws_from[[mp_ws_from_i[1]]]
            all_sa_ws_from_names <- names(RJDemetra::get_all_objects(mp_ws_from))
            for (i_sa in seq_len(RJDemetra::count(mp_ws_to))) {
                # i_sa <- 2
                sa_ws_to <- RJDemetra::get_object(mp_ws_to, i_sa)
                sa_name <- RJDemetra::get_name(sa_ws_to)
                sa_ws_from_i <- which(all_sa_ws_from_names %in% sa_name)
                if (length(sa_ws_from_i) > 0) {
                    if (length(sa_ws_from_i) > 1)
                        warning(sprintf('At least 2 SaItem called "%s" were found in the ws_from: the first object will be used',sa_name))
                    
                    sa_ws_from <- RJDemetra::get_object(mp_ws_from, sa_ws_from_i[1])
                    new_sa_item <- set_metadata(sa_to = sa_ws_to,
                                                sa_from = sa_ws_from)
                    replace_sa_item(mp = mp_ws_to, sa_item = new_sa_item,
                                    pos = i_sa)
                    
                    # sa_def1 <- .jcall(sa_ws_from, "Ljd2/datatypes/sa/SaItemType;", "getSaDefinition")
                    # jts1 <- .jcall(sa_def1, "Ljd2/datatypes/Ts;", "getTs")
                    # 
                    # sa_def2 <- .jcall(sa_ws_to, "Ljd2/datatypes/sa/SaItemType;", "getSaDefinition")
                    # jts2 <- .jcall(sa_def2, "Ljd2/datatypes/Ts;", "getTs")
                    # 
                    # builder_ts <- jts2$toBuilder()
                    # builder_ts$metaData(jts1$getMetaData())
                    # jts_temp <- builder_ts$build()
                    # builder_sa <- sa_def2$toBuilder()
                    # builder_sa$ts(jts_temp)
                    # builder_sa$metaData(sa_def1$getMetaData())
                    # sa_def_temp <- builder_sa$build()
                    # 
                    # jts_temp <- builder_from_ts(jts2, metaData =  jts1$getMetaData())
                    # sa_def_temp <- builder_from_sa(sa_def2, ts = jts_temp,
                    #                                metaData = sa_def1$getMetaData())
                    # 
                    # new_sa_item <- .jnew("ec/tstoolkit/jdr/ws/SaItem", sa_def_temp)
                    # replace_sa_item(mp = mp_ws_to, sa_item = new_sa_item,
                    #                 pos = i_sa)
                    # sa_ws_to <- RJDemetra::get_object(mp_ws_to, i_sa)
                    # sa_ws_from <- RJDemetra::get_object(mp_ws_from, i_sa)
                    # builder_ts <- jts2$toBuilder()
                    # builder_ts$metaData(jts1$getMetaData())
                    # jts_temp <- builder_ts$build()
                    # 
                    # builder_sa <- sa_def2$toBuilder()
                    # builder_sa$ts(jts_temp)
                    # builder_sa$metaData(sa_def1$getMetaData())
                    # sa_def_temp <- builder_sa$build()
                    
                } else {
                    warning(sprintf('The SaItem "%s" is not found in the ws_from',sa_name))
                }
            }
            
        } else {
            warning(sprintf('The multiprocessing "%s" is not found in the ws_from',mp_name))
        }
    }
    return(ws_to)
}
#' @name update_metadata
#' @rdname update_metadata
#' @export
update_metadata_roughly <- function(ws_from, ws_to) {
    # ws1 <- RJDemetra::load_workspace(ws_from)
    # ws2 <- RJDemetra::load_workspace(ws_to)
    
    for (i_mp in seq_len(RJDemetra::count(ws_to))) {
        mp_ws_to <- RJDemetra::get_object(ws_to, i_mp)
        mp_ws_from <- RJDemetra::get_object(ws_from, i_mp)
        
        for (i_sa in seq_len(RJDemetra::count(mp_ws_to))) {
            sa_ws_to <- RJDemetra::get_object(mp_ws_to, i_sa)
            sa_ws_from <- RJDemetra::get_object(mp_ws_from, i_sa)
            # builder_ts <- jts2$toBuilder()
            # builder_ts$metaData(jts1$getMetaData())
            # jts_temp <- builder_ts$build()
            # 
            # builder_sa <- sa_def2$toBuilder()
            # builder_sa$ts(jts_temp)
            # builder_sa$metaData(sa_def1$getMetaData())
            # sa_def_temp <- builder_sa$build()
            new_sa_item <- set_metadata(sa_to = sa_ws_to, sa_from = sa_ws_from)
            replace_sa_item(mp = mp_ws_to, sa_item = new_sa_item, pos = i_sa)
        }
    }
    return(ws_to)
}

#' Set the metadata of a SaItem
#' 
#' Function to set the name of a `"sa_item"` from the one contained in another `"sa_item"`.
#' 
#' @param sa_from the `"sa_item"` object from which the desired metadata is retrieved.
#' @param sa_to the `"sa_item"` object to modify.
#' 
#' @return a new `"sa_item"` with the model of `sa_to` and the metadata of `sa_from`.
#' @export
set_metadata <- function(sa_from, sa_to) {
    sa_def_from <- .jcall(sa_from, "Ljd2/datatypes/sa/SaItemType;", "getSaDefinition")
    jts_from <- .jcall(sa_def_from, "Ljd2/datatypes/Ts;", "getTs")
    
    sa_def_to <- .jcall(sa_to, "Ljd2/datatypes/sa/SaItemType;", "getSaDefinition")
    jts_to <- .jcall(sa_def_to, "Ljd2/datatypes/Ts;", "getTs")
    
    jts_update <- builder_from_ts(jts_to, metaData = jts_from$getMetaData())
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
get_comment <- function(x) {
    UseMethod("get_comment", x)
}
#' @export
get_comment.workspace <- function(x) {
    multiprocessings <- RJDemetra::get_all_objects(x)
    lapply(multiprocessings, get_comment)
}
#' @export
get_comment.multiprocessing <- function(x) {
    all_sa_objects <- RJDemetra::get_all_objects(x)
    lapply(all_sa_objects, get_comment)
}
#' @export
get_comment.sa_item <- function(x) {
    sa_def <- .jcall(x, "Ljd2/datatypes/sa/SaItemType;", "getSaDefinition")
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
set_comment <- function(x, comment) {
    UseMethod("set_comment", x)
}
#' @export
set_comment.sa_item <- function(x, comment) {
    sa_def <- .jcall(x, "Ljd2/datatypes/sa/SaItemType;", "getSaDefinition")
    jts <- .jcall(sa_def, "Ljd2/datatypes/Ts;", "getTs")
    
    metadata <- sa_def$getMetaData()
    
    jmap <- .jnew("java/util/LinkedHashMap")
    jmap <- .jcast(jmap, "java/util/Map")
    jmap$putAll(metadata)
    jmap$put("comment", comment)
    
    sa_def_temp <- builder_from_sa(sa_def,
                                   metaData = jmap)
    new_sa_item <- .jnew("ec/tstoolkit/jdr/ws/SaItem", sa_def_temp)
    new_sa_item
}
