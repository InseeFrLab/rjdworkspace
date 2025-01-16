#'
#' @importFrom utils URLencode
#'
format_path_to_xml <- function(path) {
    path_norm <- normalizePath(path)
    formatted_path <- URLencode(path_norm, reserved = TRUE)
    formatted_path <- gsub(
        x = formatted_path,
        pattern = "%20",
        replacement = "+",
        fixed = TRUE
    )
    return(formatted_path)
}

update_one_xml <- function(xml_path, pos_sa_item, formatted_path, verbose = TRUE) {

    if (verbose) {
        cat("Opening the xml file ", xml_path, "...\n")
    }
    xml_file <- XML::xmlParse(xml_path)
    informationSet_node <- XML::xmlChildren(xml_file)$informationSet

    # First SA-ITEM (index 1 is reserved for metadata of the SA-processing)
    SAITEM_nodes <- XML::xmlChildren(
        x = XML::xmlChildren(
            x = XML::xmlChildren(x = informationSet_node)[[1L + pos_sa_item]]
        )[["subset"]]
    )

    # Si R 4.1...
    # SAITEM_node <- node_informationSet |>
    #     XML::xmlChildren() |>
    #     base::`[[`(1 + pos_sa_item) |>
    #     XML::xmlChildren() |>
    #     base::`[[`("subset") |>
    #     XML::xmlChildren()

    pos_ts_node <- which("ts" == vapply(
        X = SAITEM_nodes,
        FUN = XML::xmlAttrs,
        "name",
        FUN.VALUE = character(1L)
    ))

    # Metadata node
    metadata_nodes <- XML::xmlChildren(SAITEM_nodes[[pos_ts_node]][["ts"]][["metaData"]])

    pos_id_node <- which("@id" == vapply(
        X = metadata_nodes,
        FUN = XML::xmlAttrs,
        "name",
        FUN.VALUE = character(2L)
    )["name", ])

    node_to_change <- metadata_nodes[[pos_id_node]]

    attrib <- XML::xmlAttrs(node_to_change)

    providers_options <- unlist(strsplit(attrib["value"], split = "&", fixed = TRUE))
    id <- which(startsWith(x = providers_options, prefix = "file"))
    providers_options[id] <- paste0("file=", formatted_path)
    attrib["value"] <- paste(providers_options, collapse = "&")

    XML::xmlAttrs(node_to_change) <- attrib

    if (verbose) {
        cat("Rewriting the xml file...\n")
    }
    XML::saveXML(doc = xml_file, file = xml_path)
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

    all_xml_sap_path <- list.files(
        path = sprintf("%s/SAProcessing", ws_folder_path),
        pattern = "\\.xml$", full.names = TRUE
    )

    if ((!missing(pos_sap))
        && (!paste0("SAProcessing-", pos_sap, ".xml") %in% all_xml_sap_path)) {
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
        if (!missing(pos_sa_item) && nb_sa_item < max(pos_sa_item)) {
            stop("The SA Item doesn't exist.")
        }
    }

    return(invisible(NULL))
}

#' Update the path to the raw series file
#'
#' @param ws_xml_path the path to the xml file of the workspace
#' @param raw_data_path the new path to the raw data
#' @param pos_sap the index of the SA-Processing containing the series
#' (Optional)
#' @param pos_sa_item the index of the SA-Item containing the series (Optional)
#' @param verbose A boolean to print indications on the processing
#' status (optional and TRUE by default)
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
#' ws_template_path <- file.path(system.file("extdata", package = "rjdworkspace"),
#'                          "WS")
#'
#' # Moving the WS in a temporary environment
#' copy_ws(
#'     ws_name = "ws_example_path",
#'     from = ws_template_path,
#'     to = new_dir
#' )
#'
#' # Moving the raw data in a temporary environment
#' data_path <- file.path(system.file("extdata", package = "rjdworkspace"),
#'                        "data_file.csv")
#' file.copy(
#'     from = data_path,
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
update_path <- function(ws_xml_path,
                        raw_data_path,
                        pos_sap,
                        pos_sa_item,
                        verbose = TRUE) {

    if (!tools::file_ext(raw_data_path) %in% c("csv", "xls", "xlsx")) {
        stop("Only the following data formats are accepted: csv, xls, xlsx, and no others.")
    }

    # Check that the ws_xml_path leads to a valid workspace
    ws <- RJDemetra::load_workspace(ws_xml_path)
    RJDemetra::compute(ws)
    if (!inherits(ws, "workspace")) {
        stop("There is an error in the workspace path")
    }

    ws_folder_path <- gsub(
        pattern = "\\.xml$", replacement = "",
        x = ws_xml_path
    )
    all_xml_files <- list.files(
        path = sprintf("%s/SAProcessing", ws_folder_path),
        pattern = "\\.xml$", full.names = FALSE, all.files = TRUE
    )

    if ((!missing(pos_sap))
        && (!paste0("SAProcessing-", pos_sap, ".xml") %in% all_xml_files)) {
        stop("The SA-Processing doesn't exist.")
    }

    if ((!missing(pos_sa_item)) && missing(pos_sap)) {
        stop("You must specify a SA-Processing.")
    }

    nb_sap <- RJDemetra::count(ws)
    if (!missing(pos_sap)) {
        if (nb_sap < pos_sap) {
            stop("The SA-Processing doesn't exist.")
        }

        nb_sa_item <- RJDemetra::count(
            RJDemetra::get_object(ws, pos = pos_sap)
        )
        if (!missing(pos_sa_item) && nb_sa_item < max(pos_sa_item)) {
            stop("The SA Item doesn't exist.")
        }
    }

    new_raw_data_path <- format_path_to_xml(raw_data_path)
    all_xml_sap_path <- list.files(
        path = sprintf("%s/SAProcessing", ws_folder_path),
        pattern = "\\.xml$", full.names = TRUE
    )

    if (missing(pos_sap)) {
        for (pos_sap in seq_along(all_xml_sap_path)) {
            xml_path <- all_xml_sap_path[pos_sap]
            nb_sa_item <- RJDemetra::count(
                RJDemetra::get_object(ws, pos = pos_sap)
            )

            for (pos2 in seq_len(nb_sa_item)) {
                update_one_xml(
                    xml_path = xml_path,
                    pos_sa_item = pos2,
                    formatted_path = new_raw_data_path,
                    verbose = verbose
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
                    formatted_path = new_raw_data_path,
                    verbose = verbose
                )
            }
        } else {
            for (pos in pos_sa_item) {
                update_one_xml(
                    xml_path = xml_path,
                    pos_sa_item = pos,
                    formatted_path = new_raw_data_path,
                    verbose = verbose
                )
            }
        }
    }

    if (verbose) {
        cat("Done!\n")
    }
    return(invisible(ws))
}
