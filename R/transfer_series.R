# Cette fonction retourne :
#   - la position du SAP
#   - une erreur ou un warning si le nom du SAP et de la position ne correspondent à rien
#   - une erreur si le WS est vide (création d'un SAP) avec error_on_unknown
#   - sinon 0

identify_object <- function(ws,
                            name_sap, pos_sap,
                            error_on_unknown = FALSE) {
    # Empty WS
    if (RJDemetra::count(ws) == 0) {
        if (error_on_unknown) {
            print("The program stops without transferring the series.")
            stop("The ws is empty.")
        } else {
            return(0)
        }
    }

    # Non empty WS
    saps <- RJDemetra::get_all_objects(ws)
    names_saps <- names(saps)

    if (missing(name_sap) && missing(pos_sap)) {
        pos_sap <- 1L
        name_sap <- names_saps[1L]
        print(paste0("No sap has been selected, the first one (", name_sap, ") will be used."))
    } else if (missing(name_sap)) {
        if (pos_sap <= RJDemetra::count(ws) && pos_sap > 0L) {
            name_sap <- names_saps[pos_sap]
        } else {
            print("The program stops without transferring the series.")
            stop(paste0("There is no SAP n\u00B0", pos_sap))
        }
    } else if (missing(pos_sap)) {
        if (name_sap %in% names_saps) {
            pos_sap <- which(names_saps == name_sap)
        } else if (error_on_unknown) {
            print("The program stops without transferring the series.")
            stop(paste0("There is no SAP named ", name_sap, " in ws."))
        } else {
            return(0)
        }
    } else {
        if (!name_sap %in% names_saps) {
            if (error_on_unknown) {
                print("The program stops without transferring the series.")
                stop(paste0("There is no SAP named ", name_sap, " in ws."))
            } else {
                return(0)
            }
        } else if (pos_sap > RJDemetra::count(ws) || pos_sap <= 0L) {
            print("The program stops without transferring the series.")
            stop(paste0("There is no SAP n\u00B0", pos_sap))
        } else if (names_saps[pos_sap] != name_sap) {
            print("The program stops without transferring the series.")
            stop("The arguments pos_sap and name_sap are refering to differents objects.")
        }
    }

    return(pos_sap)
}


#' Transfer_series
#'
#' To copy & paste series from one workspace to another
#'
#' @param ws_from The workspace containing the additionnal series
#' @param ws_to The workspace to add series to
#' @param selected_series The vector containing the series-to-update's names.
#' @param pos_sap_from The position of the SA-Processing to transfer the series from
#' @param pos_sap_to The position of the SA-Processing to transfer the series to
#' @param name_sap_from The name of the SA-Processing to transfer the series from (optional)
#' @param name_sap_to The name of the SA-Processing to transfer the series to (optional)
#' @param print_indications A boolean to print indications on the processing status (optional)
#' @param create_sap A boolean to create a new SA-Processing if not existing (optional)
#' @param replace_series A boolean to replace existing series (optional)
#'
#' @details
#' To use this function you need to first launch `load_workspace` and after `save_workspace` to save the changes.
#'
#' `name_sap_to` and `name_sap_from` refer to the SAP's name and not SAP's file's name.
#'
#' The transfer will fail if:
#'      - `name_sap_from` doesn't exist
#'      - `pos_sap_from` < 0 or exceed the maximum number of SAP
#'      - `pos_sap_to` < 0 or exceed the maximum number of SAP
#'      - The arguments `pos_sap_from` and `name_sap_from` are refering to differents objects.
#'      - The arguments `pos_sap_to` and `name_sap_to` are refering to differents objects.
#'
#' If `name_sap_to` and `pos_sap_to` are unspecified, the update will be performed using
#' the workspaces' first SAProcessing (same for the SAP from).
#' However if the informations of one on the two SAP (from or to) are specified (name or position), they will be attributed by default to the other worskpace.
#'
#' If `name_sap_to` doesn't refer to an existing SAP, a new SAP will be created (if `create_sap` is `TRUE`).
#'
#' If a sa_item has a specification which uses external regressor, you have to be sure that the regressors are also in the destination workspace.
#'
#' @return the `workspace` ws_to augmented with series present in ws_from and not already in ws_to
#'
#' @examples
#'
#' library("RJDemetra")
#'
#' dir_ws <- tempdir()
#'
#' # Moving the WS in a temporary environment
#' rjdworkspace:::copy_ws(ws_name = "ws_output",
#'         path_ws = file.path(system.file("extdata", package = "rjdworkspace"), "WS"),
#'         new_path = dir_ws)
#' rjdworkspace:::copy_ws(ws_name = "ws_input",
#'         path_ws = file.path(system.file("extdata", package = "rjdworkspace"), "WS"),
#'         new_path = dir_ws)
#'
#' path_ws_from <- file.path(dir_ws, "ws_input.xml")
#' path_ws_to <- file.path(dir_ws, "ws_output.xml")
#'
#'
#' ws_input <- load_workspace(path_ws_from)
#' ws_output <- load_workspace(path_ws_to)
#'
#' # Existing SAP
#' transfer_series(
#'     ws_from = ws_input,
#'     ws_to = ws_output,
#'     name_sap_from = "SAProcessing-1",
#'     name_sap_to = "SAProcessing-1",
#'     print_indications = TRUE
#' )
#'
#' transfer_series(
#'     ws_from = ws_input,
#'     ws_to = ws_output,
#'     pos_sap_from = 1,
#'     pos_sap_to = 1,
#'     print_indications = TRUE
#' )
#'
#' # Existing series
#' transfer_series(
#'     ws_from = ws_input, ws_to = ws_output,
#'     pos_sap_from = 2,
#'     pos_sap_to = 2,
#'     print_indications = TRUE,
#'     replace_series = FALSE
#' )
#' transfer_series(
#'     ws_from = ws_input, ws_to = ws_output,
#'     pos_sap_from = 2,
#'     pos_sap_to = 2,
#'     print_indications = TRUE,
#'     replace_series = TRUE
#' )
#'
#' # Create a new SAP
#' # transfer_series(ws_from = ws_input, ws_to = ws_output,
#' #                 name_sap_from = "SAProcessing-1",
#' #                 name_sap_to = "New-SAProcessing-from-R",
#' #                 print_indications = TRUE,
#' #                 create = FALSE)
#'
#' transfer_series(
#'     ws_from = ws_input, ws_to = ws_output,
#'     name_sap_from = "SAProcessing-1",
#'     name_sap_to = "New-SAProcessing-from-R",
#'     print_indications = TRUE,
#'     create = TRUE
#' )
#'
#' RJDemetra::save_workspace(workspace = ws_output, file = path_ws_to)
#'
#' @export
#'
transfer_series <- function(ws_from, ws_to,
                            selected_series,
                            pos_sap_from, pos_sap_to,
                            name_sap_from, name_sap_to,
                            print_indications = FALSE,
                            create_sap = TRUE,
                            replace_series = TRUE) {
    # Verification of the parameters type
    if (!inherits(ws_from, "workspace")) {
        stop("The argument ws_from must be a workspace")
    }
    if (!inherits(ws_to, "workspace")) {
        stop("The argument ws_to must be a workspace")
    }

    if (missing(name_sap_from) && missing(pos_sap_from)) {
        if (!missing(name_sap_to)) {
            name_sap_from <- name_sap_to
        } else if (!missing(pos_sap_to)) {
            pos_sap_from <- pos_sap_to
        }
    }

    if (missing(name_sap_to) && missing(pos_sap_to)) {
        if (!missing(name_sap_from)) {
            name_sap_to <- name_sap_from
        } else if (!missing(pos_sap_from)) {
            pos_sap_to <- pos_sap_from
        }
    }

    # Identify object
    pos_sap_from <- identify_object(
        ws = ws_from,
        name_sap = name_sap_from,
        pos_sap = pos_sap_from,
        error_on_unknown = TRUE
    )

    sap_from <- RJDemetra::get_object(x = ws_from, pos = pos_sap_from)
    name_sap_from <- RJDemetra::get_name(sap_from)

    pos_sap_to <- identify_object(
        ws = ws_to,
        name_sap = name_sap_to,
        pos_sap = pos_sap_to,
        error_on_unknown = !create_sap
    )

    if (pos_sap_to == 0) {
        # A new SAP will be created
        if (missing(name_sap_to)) {
            print("The program stops without transferring the series.")
            stop("No name for sap_to has been specified.")
        } else {
            print(paste0("A new SAP named ", name_sap_to, " in ws_to will be created."))
            sap_to <- RJDemetra::new_multiprocessing(
                workspace = ws_to, name = name_sap_to
            )
            pos_sap_to <- RJDemetra::count(ws_to)
        }
    } else {
        sap_to <- RJDemetra::get_object(x = ws_to, pos = pos_sap_to)
        name_sap_to <- RJDemetra::get_name(sap_to)
    }


    if (print_indications) {
        cat(paste0(
            "First WS's SAP :\n",
            "\t- name : ", name_sap_from,
            "\n\t- pos : ", pos_sap_from,
            "\nand\n",
            "Second WS's SAP :\n",
            "\t- name : ", name_sap_to,
            "\n\t- pos : ", pos_sap_to
        ))
        cat("\n\n")
    }

    sa_items_from <- RJDemetra::get_all_objects(sap_from)
    names_series_from <- names(sa_items_from)

    sa_items_to <- RJDemetra::get_all_objects(sap_to)
    names_series_to <- names(sa_items_to)

    # Check de selected series
    if (missing(selected_series)) {
        selected_series <- names_series_from
    } else if (any(!selected_series %in% names_series_from)) {
        warning("The series ", setdiff(selected_series, names_series_from), " are not in the SAP ", name_sap_from, "from ws_from. They won't be transfered.")
        selected_series <- intersect(selected_series, names_series_from)
    }

    if (!replace_series) {
        selected_series <- setdiff(selected_series, names_series_to)
    }

    # Replacement of all series specified in the "selected_series" vector
    for (index in seq_along(selected_series)) {
        series_name <- selected_series[index]
        position <- which(names_series_from == series_name)
        if (print_indications) {
            cat(paste0(
                "Series n\u00B0", index,
                ", name: ", series_name,
                ", position: ", position
            ))
        }

        # The "up-to-date" series version
        extracted_sa_item <- RJDemetra::get_object(sap_from, position)

        # Cas de remplacement
        if (series_name %in% names_series_to) {
            if (print_indications) {
                cat(" - to replace...")
            }
            position <- which(names_series_to == series_name)
            replace_sa_item(sap = sap_to, pos = position, sa_item = extracted_sa_item)
        } else {
            if (print_indications) {
                cat(" - to add...")
            }
            add_new_sa_item(sap = sap_to, extracted_sa_item)
        }

        cat(" Successful transfer!\n")
    }

    cat("\nDone!")
    return(invisible(ws_to))
}
