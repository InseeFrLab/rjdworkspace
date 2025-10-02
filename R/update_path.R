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

update_one_xml <- function(xml_path, formatted_path, verbose = TRUE) {

    if (verbose) {
        cat("Opening the xml file ", xml_path, "...\n")
    }
    xml_file <- XML::xmlParse(xml_path)
    informationSet_node <- XML::xmlChildren(xml_file)$informationSet

    idx_sa <- which(!vapply(
        X = XML::xmlChildren(x = informationSet_node),
        FUN = XML::xmlAttrs,
        "name",
        FUN.VALUE = character(1L)
    ) %in% c("domainspecs", "metadata"))

    for (pos_sa_item in idx_sa) {
        SAITEM_nodes <- XML::xmlChildren(
            x = XML::xmlChildren(
                x = XML::xmlChildren(x = informationSet_node)[[pos_sa_item]]
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
        regex_pattern <- "(file=)[^&#]+"

        attrib["value"] <- gsub(
            pattern = regex_pattern,
            replacement = paste0("\\1", formatted_path),
            x = attrib["value"],
            fixed = FALSE
        )
        XML::xmlAttrs(node_to_change) <- attrib
    }
    if (verbose) {
        cat("Rewriting the xml file...\n\n")
    }
    XML::saveXML(doc = xml_file, file = xml_path)
    return(invisible(NULL))
}

#' @title Update the path to the raw series file
#'
#' @param ws_xml_path the path to the xml file of the workspace
#' @param raw_data_path the new path to the raw data
#' @param sap_xml_path the path (or just the filename) of the xml file of
#' SA-Processing containing the series (Optional).
#' @param verbose A boolean to print indications on the processing
#' status (optional and TRUE by default)
#' @description
#' Function to update the path of the raw data file in a workspace.
#' This function works with .csv, .xls and .xlsx format.
#'
#' @details
#' Warning! Since version 1.2.0, this function updates the raw data path of ALL
#' SA-Items in a SA-Processing. Therefore, the only way to identify a specific
#' SA-Processing is to provide the path to the XML file (which is not obvious
#' when the SA-Processings have different names or indices to their position).
#'
#' The argument `sap_xml_path` is optional.
#' If `sap_xml_path` is not supplied, all SA-Processing will be updated.
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
#'     sap_xml_path = file.path(new_dir, "ws_example_path", "SAProcessing", "SAProcessing-1.xml"),
#' )
#' update_path(
#'     ws_xml_path = path_ws,
#'     raw_data_path = new_raw_data_path,
#'     sap_xml_path = "SAProcessing-1.xml",
#' )
#' update_path(
#'     ws_xml_path = path_ws,
#'     raw_data_path = new_raw_data_path
#' )
#' @export
update_path <- function(ws_xml_path,
                        raw_data_path,
                        sap_xml_path,
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
    all_xml_sap_files <- list.files(
        path = sprintf("%s/SAProcessing", ws_folder_path),
        pattern = "\\.xml$", full.names = FALSE, all.files = TRUE
    )

    if (!missing(sap_xml_path)) {
        sap_filenames <- basename(sap_xml_path)
        if (any(tools::file_ext(sap_filenames) != "xml")) {
            stop("Only .xml files are are accepted.")
        } else if (!all(basename(sap_filenames) %in% all_xml_sap_files)) {
            stop("The SA-Processing doesn't exist. `sap_xml_path` should be in ", paste0(all_xml_sap_files, collapse = ", "))
        }
        xml_files <- normalizePath(file.path(ws_folder_path, "SAProcessing", sap_filenames), mustWork = TRUE)
    } else {
        xml_files <- normalizePath(file.path(ws_folder_path, "SAProcessing", all_xml_sap_files), mustWork = TRUE)
    }

    new_raw_data_path <- format_path_to_xml(raw_data_path)
    for (xml_file in xml_files) {
        update_one_xml(
            xml_path = xml_file,
            formatted_path = new_raw_data_path,
            verbose = verbose
        )
    }

    if (verbose) {
        cat("Done!\n")
    }
    return(invisible(ws))
}
