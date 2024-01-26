#'
#' @importFrom utils URLencode
#'
format_path_to_xml <- function(path) {
    path_norm <- normalizePath(path)
    formatted_path <- URLencode(path_norm, reserved = TRUE)

    return(formatted_path)
}

update_one_xml <- function(xml_path, pos_sa_item, formated_data_path) {
    a <- XML::xmlParse(xml_path)

    node_informationSet <- XML::xmlChildren(a)[["informationSet"]]
    # First SA-ITEM (index 1 is reserved for metadata of the SA-processing)
    node_SAITEM <- XML::xmlChildren(node_informationSet)[[1 + pos_sa_item]]
    # Metadata node
    subset_node <- XML::xmlChildren(node_SAITEM)[["subset"]][["item"]][["ts"]]
    perent_metadata_node <- subset_node[["metaData"]]
    nodes_metadata <- XML::xmlChildren(perent_metadata_node)

    id_pos <- which(x = sapply(X = nodes_metadata,
                               FUN = XML::xmlAttrs)["name", ] == "@id")

    node_to_change <- nodes_metadata[[id_pos]]

    attrib <- XML::xmlAttrs(node_to_change)

    chain_temp <- unlist(strsplit(attrib["value"], split = "file="))
    chain1 <- chain_temp[1]
    chain_temp <- unlist(strsplit(chain_temp[2], split = "#series"))
    chain2 <- chain_temp[2]

    attrib["value"] <- paste0(chain1, "file=", formated_data_path,
                              "#series", chain2)
    XML::xmlAttrs(node_to_change) <- attrib
    XML::saveXML(a, file = xml_path)

    return(invisible(NULL))
}

check_information <- function(ws_xml_path, pos_sap, pos_sa_item) {

    # Verification that the ws_xml_path leads to a valid workspace
    ws <- RJDemetra::load_workspace(ws_xml_path)
    compute(ws)
    if (!inherits(ws, "workspace")) {
        stop("There is an error in the workspace path")
    }

    ws_folder_path <- gsub(
        pattern = "\\.xml$", replacement = "",
        x = ws_xml_path
    )
    all_xml_sap <- list.files(sprintf("%s/SAProcessing", ws_folder_path),
        pattern = "\\.xml$"
    )

    if ((!missing(pos_sap))
        && (!paste0("SAProcessing-", pos_sap, ".xml") %in% all_xml_sap)) {
        stop("The SA-Processing doesn't exist.")
    }

    if ((!missing(pos_sa_item)) && missing(pos_sap)) {
        stop("You must specify a SA-Processing.")
    }

    nb_sap <- length(get_all_objects(ws))
    if (!missing(pos_sap)) {
        if (nb_sap < pos_sap) {
            stop("The SA-Processing doesn't exist.")
        }

        nb_sa_item <- length(get_all_objects(get_object(ws, pos = pos_sap)))
        if (!missing(pos_sa_item)) {
            if (nb_sa_item < max(pos_sa_item)) {
                stop("The SA Item doesn't exist.")
            }
        }
    }

    return(invisible(NULL))
}

#' Update the path to the raw series file
#'
#' @param ws_xml_path the path to the xml file of the workspace
#' @param raw_data_path the new path to the raw data
#' @param pos_sap the index of the SA-Processing containing the series (Optional)
#' @param pos_sa_item the index of the SA-Item containing the series (Optional)
#'
#' @description
#' Function to update the path of the raw data file in a workspace.
#' This function works with .csv, .xls and .xlsx format.
#'
#' @details
#' The argument `pos_sap` and `pos_sa_item` are optional.
#' If `pos_sa_item` is not supplied, all SA-Item will be updated.
#' If `pos_sap` is not supplied, all SA-Processing will be updated.
#'
#' If `pos_sa_item` is supplied, `pos_sap` must be specified.
#'
#' It's also important that the new data file has the same structure as the
#' previous file :
#'      - same column names
#'      - same column position
#'      - same extension and format (.csv, .xls or .xlsx)
#'
#' @return the `workspace` ws_to augmented with series present in ws_from and
#' not already in ws_to
#'
#' @examples
#'
#' library("RJDemetra")
#' new_dir <- tempdir()
#'
#' # Moving the WS in a temporary environment
#' rjdworkspace:::copy_ws(ws_name = "ws_example_path",
#'                        path_ws = file.path(system.file("extdata", package = "rjdworkspace"), "WS"),
#'                        new_path = new_dir)
#'
#' # Moving the raw data in a temporary environment
#' file.copy(
#'     from = file.path(system.file("extdata", package = "rjdworkspace"), "data_file.csv"),
#'     to = new_dir
#' )
#'
#' path_ws <- file.path(new_dir, "ws_example_path.xml")
#' new_raw_data_path <- file.path(new_dir, "data_file.csv")
#'
#' update_path(
#'     ws_xml_path = path_ws,
#'     raw_data_path = new_raw_data_path,
#'     pos_sap = 1L,
#'     pos_sa_item = 1L:2L
#' )
#' update_path(
#'     ws_xml_path = path_ws,
#'     raw_data_path = new_raw_data_path,
#'     pos_sap = 1L
#' )
#' update_path(
#'     ws_xml_path = path_ws,
#'     raw_data_path = new_raw_data_path
#' )
#'
#' @export
update_path <- function(ws_xml_path, raw_data_path, pos_sap, pos_sa_item) {

    if (!tools::file_ext(raw_data_path) %in% c("csv", "xls", "xlsx")) {
        stop(paste("Only the following data formats are accepted:",
                   "csv, xls, xlsx, and no others."))
    }

    # Check that the ws_xml_path leads to a valid workspace
    ws <- RJDemetra::load_workspace(ws_xml_path)
    compute(ws)
    if (!inherits(ws, "workspace")) {
        stop("There is an error in the workspace path")
    }

    ws_folder_path <- gsub(
        pattern = "\\.xml$", replacement = "",
        x = ws_xml_path
    )
    all_xml_sap <- list.files(sprintf("%s/SAProcessing", ws_folder_path),
        pattern = "\\.xml$"
    )

    if ((!missing(pos_sap))
        && (!paste0("SAProcessing-", pos_sap, ".xml") %in% all_xml_sap)) {
        stop("The SA-Processing doesn't exist.")
    }

    if ((!missing(pos_sa_item)) && missing(pos_sap)) {
        stop("You must specify a SA-Processing.")
    }

    nb_sap <- length(get_all_objects(ws))
    if (!missing(pos_sap)) {
        if (nb_sap < pos_sap) {
            stop("The SA-Processing doesn't exist.")
        }

        nb_sa_item <- length(get_all_objects(get_object(ws, pos = pos_sap)))
        if (!missing(pos_sa_item)) {
            if (nb_sa_item < max(pos_sa_item)) {
                stop("The SA Item doesn't exist.")
            }
        }
    }

    new_raw_data_path <- format_path_to_xml(raw_data_path)

    if (missing(pos_sap)) {
        all_xml_sap <- list.files(
            path = sprintf("%s/SAProcessing", ws_folder_path),
            pattern = "\\.xml$", full.names = TRUE
        )
        for (pos_sap in seq_along(all_xml_sap)) {
            xml_path <- all_xml_sap[pos_sap]
            nb_sa_item <- length(get_all_objects(get_object(ws, pos = pos_sap)))

            for (pos2 in seq_len(nb_sa_item)) {
                update_one_xml(
                    xml_path = xml_path,
                    pos_sa_item = pos2,
                    formated_data_path = new_raw_data_path
                )
            }
        }
    } else {
        xml_path <- paste0(
            sprintf("%s/SAProcessing/", ws_folder_path),
            "SAProcessing-", pos_sap, ".xml"
        )

        if (missing(pos_sa_item)) {
            for (pos in seq_len(nb_sa_item)) {
                update_one_xml(
                    xml_path = xml_path,
                    pos_sa_item = pos,
                    formated_data_path = new_raw_data_path
                )
            }
        } else {
            for (pos in pos_sa_item) {
                update_one_xml(
                    xml_path = xml_path,
                    pos_sa_item = pos,
                    formated_data_path = new_raw_data_path
                )
            }
        }
    }

    print("Done!")
    return(invisible(ws))
}
