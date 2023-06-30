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


#' Update the path to the raw series file
#' 
#' Function to change the path to the raw series file in a workspace
#' 
#' @param ws_path the path to the workspace
#' @param raw_data_path the path to the raw data file
#' @param param the number of lines between a series name and the path to its raw file in the xml file. The default value is 8.
#' @param print_log a boolean to indicate whether to print progress indications.
#' 
#' @return the eventual list of series from the workspace that weren't found in the raw series file
#' 
#' @details - The paths' translation into ASCII is done by the function.
#' - The function operates on a workspace's first SAProcessing (ie. on a whole workspace when it contains one SAP).
#' - The raw series file must be a csv file.
#' - The paths must be "full" (as opposed to relative to the setwd directory, for example).
#' 
#' @examples
#' 
#' path_to_ws <- file.path(system.file("extdata", package = "rjdworkspace"),
#'                         "WS/ws_example_path.xml")
#'                         
#' path_to_raw_data <- file.path(system.file("extdata", package = "rjdworkspace"),
#'                               "data_file.csv")
#'                               
#' # Minimal syntax
#' update_path(ws_path = path_to_ws, raw_data_path = path_to_raw_data)
#' 

#' @export

update_path <- function(ws_path, raw_data_path, param = 8, print_log = FALSE) {
    
    # Verification that the ws_path leads to a valid workspace 
    ws <- load_workspace(ws_path)
    compute(ws)
    if (!inherits(ws, "workspace")) {
        stop("There is an error in the workspace path")
    }
    # print(ws)
    
    
    
    # If the default value of param is used and only print_log is indicated,
    # The logical parameter is re-attributed to print_logical and the default value (of 8), to param
    if (is.logical(param)) {
        print_log <- param
        param = 8
    }
    
    # Conversion of the raw data path into ASCII/JD+ format (folder separators: / or \\)
    raw_data_path_JD5 <- gsub(":", "%3A", raw_data_path)
    raw_data_path_JD4 <- gsub("/", "%5C", raw_data_path_JD5)
    raw_data_path_JD3 <- gsub("\\\\", "%5C", raw_data_path_JD4)
    raw_data_path_JD2 <- gsub(" ", "+", raw_data_path_JD3)
    raw_data_path_JD1 <- gsub("_a1", "", raw_data_path_JD2)
    raw_data_path_JD <- gsub("_a2", "", raw_data_path_JD1)
    raw_data_path_JD
    
    # Extraction of the path to the workspace main folder
    ws_folder <- gsub("\\.xml$", "", ws_path)
    
    # Retrieval of the xml file(s) within the SAProcessing sub-directory
    fic_xml <- list.files(sprintf("%s/SAProcessing", ws_folder), 
                         pattern = "\\.xml$")
    if (print_log) {
        print(fic_xml)
    }
    # "SAProcessing-1.xml"
    
    # Complete path to the xml file(s) within the SAProcessing sub-directory
    ch_fic_xml <- paste0(sprintf("%s/SAProcessing/", ws_folder), fic_xml)
    ch_fic_xml
    # "my_worksp ace/SAProcessing/SAProcessing-1.xml"
    
    # Import of said xml file(s)
    fic_xml <- readLines(ch_fic_xml)
    
    # Retrieval of the series' names from the csv file
    raw_data <- utils::read.csv2(raw_data_path, check.names = FALSE)
    series_names_csv <- colnames(raw_data)[-1]# date removal
    series_names_csv
    
    # And from the workspace
    series_names_ws <- names(get_all_objects(get_all_objects(ws)[[1]]))
    
    # Series in the workspace that aren't in the csv file
    untreated_series <- series_names_ws[!(series_names_ws %in% series_names_csv)]
    
    # For each series in the workspace:
    for (i in seq(1, length(series_names_ws))) {
        # i=1
        series_i <- series_names_ws[i]
        # if it is in the csv file
        if (series_i %in% series_names_csv) {
            if (print_log) {
                print(paste0(i, "  ", series_i))
            }
            # We retrieve the position of the line containing the serie's name (<ts name="C4511"> for ex) in the xml file
            # +8 or +param  = position of the line containing the path to the raw data file 
            pos_path <- grep(paste0('"', series_i, '"'), fic_xml, fixed = TRUE) + param
            
            # We extract the path to the raw data file
            chain1 <- unlist(stringr::str_split(fic_xml[pos_path], "file="))
            chain2 <- unlist(stringr::str_split(chain1[2], ".csv"))
            # We insert the new raw data file path and put "the line back in place"
            fic_xml[pos_path] <- paste0(chain1[1], "file=", raw_data_path_JD,chain2[2])
        }
    }
    # We save the updated xml file 
    writeLines(fic_xml, ch_fic_xml)
    print("Update completed")
    
    if (length(untreated_series) != 0) {
        print("The following series of the workspace aren't in the csv file:")
        return(untreated_series)
    }
}
