#' Partial update of a workspace metadata
#'
#' `replace_series()` allows to update a selection of series by the same-named
#' series from another workspace. When only the metadata differs, it is the
#' partial version of the update_metadata function.
#'
#'
#' @param ws_from The workspace containing the most up-to-date version of the
#' selected_series series
#' @param ws_to The workspace to update
#' @param selected_series The vector containing the series-to-update's names.
#' @param mp_from_name The name of the SA-Processing containing the series to
#' update (optional)
#' @param mp_to_name The name of the SA-Processing to update (optional)
#' @param verbose A boolean to print indications on the processing
#' status (optional and TRUE by default)
#'
#' @details If the arguments `mp_from_name` & `mp_to_name` are unspecified, the
#' update will be performed using the workspaces' first SAProcessing.
#' If a series is specified in the selected_series vector is missing in a
#' workspace, no replacement will be performed and the function will return the
#' list of missing series. Otherwise, if all is well, the function returns the
#' workspace ws_to updated.
#'
#' @rdname replace_series
#' @return the updated `workspace`
#' @examples
#'
#' library("RJDemetra")
#' dir_ws <- tempdir()
#' template_ws <- file.path(system.file("extdata", package = "rjdworkspace"),
#'                          "WS")
#' # Moving the WS in a temporary environment
#' copy_ws(
#'     ws_name = "ws_output",
#'     from = template_ws,
#'     to = dir_ws
#' )
#' copy_ws(
#'     ws_name = "ws_input",
#'     from = template_ws,
#'     to = dir_ws
#' )
#' path_ws_from <- file.path(dir_ws, "ws_input.xml")
#' path_ws_to <- file.path(dir_ws, "ws_output.xml")
#' ws_input <- load_workspace(path_ws_from)
#' ws_output <- load_workspace(path_ws_to)
#'
#' replace_series(
#'     ws_from = ws_input,
#'     ws_to = ws_output,
#'     mp_from_name = "SAProcessing-2",
#'     mp_to_name = "SAProcessing-2",
#'     selected_series = c("RF1039", "RF1041"),
#'     verbose = TRUE
#' )
#'
#' @export
#'
replace_series <- function(ws_from,
                           ws_to,
                           selected_series,
                           mp_from_name,
                           mp_to_name,
                           verbose = TRUE) {
    .Deprecated(
        new = "transfer_series",
        msg = paste(
            "replace_series is replaced by transfer_series (with replace_series = TRUE).",
            "Functionality remains the same.",
            "The new function add new functionnalities.",
            "Please adjust your code accordingly.", sep = "\n "
        )
    )

    # Verification of the parameters type
    if (!inherits(ws_to, "workspace")) {
        stop("The first argument must be a workspace")
    }
    if (!inherits(ws_from, "workspace")) {
        stop("The second argument must be a workspace")
    }

    if (is.null(selected_series)) {
        stop("The selected_series list is empty!")
    } else {
        if (!is.character(selected_series)) {
            stop("The selected_series list must contain characters (the series names).")
        }
    }

    # Check that the workspaces aren't empty
    if (is.null(count(ws_to))) {
        warning("Warning, the first workspace is empty!")
        return(FALSE)
    }

    if (is.null(count(ws_from))) {
        warning("Warning, the second workspace is empty!")
        return(FALSE)
    }

    # When both workspaces aren't empty:

    # Retrieving all SAPs and their names
    # "sap" refers to all SAPs of each workspace
    # "mp" refers to the specified "mp_name" SAP to update
    saps_to <- RJDemetra::get_all_objects(ws_to)
    names_saps_to <- names(saps_to)

    saps_from <- RJDemetra::get_all_objects(ws_from)
    names_saps_from <- names(saps_from)


    # Verification that both workspaces contain the SAP to be updated
    # and storage of its position in each workspace

    # if no SAP has been declared, the first of each workspaces will be used
    pos_mp_to <- 1L
    pos_mp_from <- 1L


    # otherwise, we retrieve the SAP's position in each workspace
    if (!missing(mp_to_name)) {
        pos_mp_to <- which(names_saps_to == mp_to_name)
    }
    if (!missing(mp_from_name)) {
        pos_mp_from <- which(names_saps_from == mp_from_name)
    }

    if (verbose) {
        cat("pos_mp_to=", pos_mp_to, "pos_mp_from=", pos_mp_from, "\n", sep = "")
    }

    # End of the program if the SAP can't be found in one of the workspaces
    # ie. an unexisting SAP was specified
    if (sum(pos_mp_to) == 0L || is.null(pos_mp_to) || is.na(pos_mp_to)) {
        warning("The chosen SAP couldn't be found in the first workspace.")
        return(FALSE)
    }

    if (sum(pos_mp_from) == 0L || is.null(pos_mp_from) || is.na(pos_mp_from)) {
        warning("The chosen SAP couldn't be found in the second workspace.")
        return(FALSE)
    }

    # If a corresponding SAP is found in both workspaces, verification that they are both non empty
    if (is.null(count(saps_to[[pos_mp_to]]))) {
        warning("The chosen SAP of the first workspace is empty.")
        return(FALSE)
    }

    if (is.null(count(saps_from[[pos_mp_from]]))) {
        warning("The chosen SAP of the second workspace is empty.")
        return(FALSE)
    }

    # If all is well:
    # Retrieving the SAPs series...
    count(saps_to[[pos_mp_to]])
    series_saps_to <- RJDemetra::get_all_objects(saps_to[[pos_mp_to]])
    series_saps_from <- RJDemetra::get_all_objects(saps_from[[pos_mp_from]])

    # ... and their names
    names_series_to <- names(series_saps_to)
    names_series_from <- names(series_saps_from)

    # Verification that all series in the list "selected_series" are present in both SAPs
    nb_series <- length(selected_series)
    pos_table <- data.frame(
        selected_series = selected_series,
        pos_series_to = rep(NA, nb_series),
        pos_series_from = rep(NA, nb_series)
    )

    for (i in seq_len(nb_series)) {
        if (selected_series[i] %in% names_series_to) {
            pos_table$pos_series_to[i] <- which(selected_series[i] == names_series_to)
        } else {
            pos_table$pos_series_to[i] <- 0L
            warning("Warning, series ", selected_series[i],
                    " is not in the destination (to) workspace's SAP `",
                    names_saps_to, "`")
        }

        if (selected_series[i] %in% names_series_from) {
            pos_table$pos_series_from[i] <- which(selected_series[i] == names_series_from)
        } else {
            pos_table$pos_series_from[i] <- 0L
            warning("Warning, series ", selected_series[i],
                    " is not in the origin (from) workspace's SAP `",
                    names_saps_to, "`")
        }
    }

    # If a series is absent from at least one workspace/SAP:
    # Its name is stored
    verif <- unique(c(pos_table[pos_table$pos_series_to == 0L, ]$selected_series, pos_table[pos_table$pos_series_from == 0L, ]$selected_series))

    # It is returned by the function
    if (length(verif) != 0L) {
        warning("The replacement wasn't performed: fix the selected_series vector or the workspace(s) and recompile!")
        return(invisible(verif))
    } else {
        # Otherwise, if all is good:
        # Retrieving both SAPs that will be used (possibly identically named)
        mp_to <- saps_to[[pos_mp_to]]
        mp_from <- saps_from[[pos_mp_from]]

        # Replacement of all series specified in the "selected_series" vector
        for (i in seq_len(nb_series)) {

            if (verbose) {
                cat("Series", i, "\n")
            }

            # The "up-to-date" series version
            replacement_series <- RJDemetra::get_object(
                x = mp_from,
                pos = pos_table$pos_series_from[i]
            )

            if (verbose) {
                print(get_name(replacement_series))
            }

            # Replacement of the series by its updated version (change made in the reference workspace)
            replace_sa_item(
                sap = mp_to,
                pos = pos_table$pos_series_to[i],
                sa_item = replacement_series
            )

            if (verbose) {
                cat("Done!\n")
            }
        }

        if (verbose) {
            if (missing(mp_to_name)) {
                cat("Update done for the first workspaces' SA-Processing.\n")
            } else {
                cat("Series updating done for the SAP ", mp_to_name, ".\n")
            }
        }

        return(invisible(ws_to))
    }
}

#' Auxiliary functions
#'
#' Generic function to identify and return the duplicates in a list
#'
#' @details
#' `verif_duplicates()` identifies and returns the duplicates in a list
#' `verif_ws_duplicates()` identifies duplicated series in a SAProcessing (SAP)
#' and SAProcessings in a workspace
#'
#' @param s a list of characters
#'
#' @rdname replace_series
#' @return If there are no duplicates, the function returns an empty data frame.
#' Otherwise, it returns a data frame giving the name and number of duplicates found within the argument (list).
#' @examples
#' s <- c("a", "b", "a", "c", "a", "c")
#' print(rjdworkspace:::verif_duplicates(s))
#'
verif_duplicates <- function(s) {
    s <- as.factor(s)
    ta <- table(s)
    lev <- levels(s)
    res <- data.frame(lev, ta)
    colnames(res) <- c("name", "s", "Freq")
    res2 <- res[res$Freq > 1L, c("name", "Freq")]
    return(res2)
}
#'
#'
#' @param ws The workspace to scan
#' @param verbose A boolean to print indications on the processing
#' status (optional and TRUE by default)
#' @rdname replace_series
#' @return a list containing the name and number of occurences of duplicated SAPs and series
#' @export
verif_ws_duplicates <- function(ws, verbose = TRUE) {
    if (!inherits(ws, "workspace")) {
        stop("The argument must be a workspace")
    }

    RJDemetra::compute(ws)
    nb_sap <- RJDemetra::count(ws)
    result <- NULL

    # Error message if the workspace is empty
    if (is.na(nb_sap) || nb_sap <= 0L) {
        warning("The workspace is empty, no duplicates could be found.")
        return(FALSE)
    } else {
        if (verbose) {
            cat("The workspace contains", nb_sap, "SAP(s).\n")
        }
    }

    # If the workspace contains more than one SAP, we verify that no two have the same name
    sap_duplicates <- NULL
    if (nb_sap >= 1L) {
        sap_ws <- RJDemetra::get_all_objects(ws)
        # if (is.null(count(sap_ws))) {return()}
        sap_names <- names(sap_ws)
        sap_duplicates <- verif_duplicates(sap_names)
        ## "sap_duplicates" is expected to be FALSE
    }

    # Then all series are scanned, one SAP at a time :
    if (nb_sap != 1L && sum(sap_duplicates$Freq) != 0L) {
        warning("Warning, the following SAPs are duplicated in the workspace: ", sap_duplicates$nom)
    } else {
        if (verbose) {
            cat("Ok: no SAP duplicate.\n")
        }

        # If the workspace isn't empty and there are no SAP duplicates, the series scan begins
        sap_ws <- RJDemetra::get_all_objects(ws)

        # For each sap
        for (i in seq_len(nb_sap)) { # i=2
            # The name of the processed sap is retrived
            sap_name <- RJDemetra::get_name(sap_ws[[i]])
            sap_name
            # As well as its series
            series <- RJDemetra::get_all_objects(sap_ws[[i]])
            # and their names
            series_names <- names(series)
            series_names
            # And the verif_duplicates() function is applied to the list of SAP series names.
            v <- verif_duplicates(series_names)
            ## We want v to be empty
            if (nrow(v) == 0L) {
                if (verbose) {
                    cat("The SAP", sap_name, "does not contain any duplicated series.\n")
                }
            } else {
                if (verbose) {
                    cat("Attention! The SAP", sap_name, "(position ", i, ") contains at least one duplicated series:\n")
                    print(v)
                }
                result <- c(result, v)
            }
        }
    }
    if (sum(sap_duplicates$Freq) != 0L) {
        result <- c(sap_duplicates, result)
    }
    return(result)
}
