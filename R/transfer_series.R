#' Transfer_series
#' 
#' To copy&paste series from one workspace to another
#'
#' @param ws_from The workspace containing the additionnal series
#' @param ws_to The workspace to add series to
#' @param name_mp_to The name of the multiprocessing to transfer the series to
#' @param name_mp_from The name of the multipricessing to transfer the series from
#' @param print_indications A boolean to print indications on the processing status (optional)
#' @param selected_series The vector containing the series-to-update's names.
#'  
#' @details If `name_mp_to` and `name_mp_from` are unspecified, the update will be performed using 
#' the workspaces' first SAProcessing. If only one SAProcessing name is specified but the workspace 
#' it refers to is not (e.g. argument `SAP1` instead of `name_mp_from="SAP1"`), it will be 
#' attributed by default to the first worskpace (`name_mp_to="SAP1"`). 
#'
#' @return the `workspace` ws_to augmented with series present in ws_from and not already in ws_to
#' @examples \dontrun{transfer_series(ws_from, ws_to, "SAProcessing-1", "MP2", TRUE)}
#' @export
#'
#'
transfer_series <- function(
        ws_from, ws_to, 
        selected_series, 
        name_mp_from, name_mp_to, 
        pos_mp_from, pos_mp_to, 
        print_indications = FALSE, 
        create_mp = TRUE, 
        replace_series = TRUE) {
    
    # Verification of the parameters type
    if (!inherits(ws_from, "workspace")) {
        stop("The argument ws_from must be a workspace")
    }
    if (!inherits(ws_to, "workspace")) {
        stop("The argument ws_to must be a workspace")
    }
    
    # Check that the workspaces aren't empty
    if (is.null(RJDemetra::count(ws_from))) {
        warning("Attention, the workspace ws_from is empty!")
        return(FALSE)
    }
    
    # When both workspaces aren't empty:
    
    # Retrieving all SAPs and their names
    # "sap" refers to all SAPs of each workspace
    # "mp" refers to the specified "mp_name" SAP to update
    
    saps_from <- RJDemetra::get_all_objects(ws_from)
    names_saps_from <- names(saps_from)
    
    # Check MP selection
    # WS from
    if (missing(name_mp_from) && missing(pos_mp_from)) {
        pos_mp_from <- 1L
        name_mp_from <- names_saps_from[1L]
        print(paste0("No sap has been selected, the first one (", name_mp_from, ") will be used."))
    } else if (missing(name_mp_from)) {
        if (pos_mp_from <= RJDemetra::get_all_objects(ws_from) && pos_mp_from > 0L) {
            name_mp_from <- names_saps_from[pos_mp_from]
        } else {
            warning(paste0("There is no SAP n°", pos_mp_from))
            print("The program stops without transferring the series.")
            return(FALSE)
        }
    } else if (missing(pos_mp_from)) {
        if (name_mp_from %in% names_saps_from) {
            pos_mo_from <- which(names_saps_from == name_mp_from)
        } else {
            warning(paste0("There is no SAP named ", name_mp_from, "in ws_from."))
            print("The program stops without transferring the series.")
            return(FALSE)
        }
    } else {
        if (!name_mp_from %in% names_saps_from) {
            warning(paste0("There is no SAP named ", name_mp_from, "in ws_from."))
            print("The program stops without transferring the series.")
            return(FALSE)
        } else if (pos_mp_from > RJDemetra::get_all_objects(ws_from) || pos_mp_from <= 0L) {
            warning(paste0("There is no SAP n°", pos_mp_from))
            print("The program stops without transferring the series.")
            return(FALSE)
        } else if (names_saps_from[pos_mp_from] != name_mp_from) {
            warning("The arguments pos_mp_from and name_mp_from are refering to differents objects.")
            print("The program stops without transferring the series.")
            return(FALSE)
        }
    }
    
    
    saps_to <- RJDemetra::get_all_objects(ws_to)
    names_saps_to <- names(saps_to)
    
    # WS to
    
    # First case : empty ws
    if (RJDemetra::count(ws_to) == 0) {
        print("The ws_to is empty.")
        if (create) {
            if (missing(name_mp_to)) {
                warning("No name for mp_to has been specified.")
                print("The program stops without transferring the series.")
                return(FALSE)
            } else {
                print("A new SAP in ws_to will be created.")
                RJDemetra::new_multiprocessing(
                    workspace = ws_to, name = name_mp_to)
                pos_mp_to <- RJDemetra::count(ws_to)
            }
        } else {
            print("The program stops without transferring the series.")
            return(FALSE)
        }
    } else if (missing(name_mp_to) && missing(pos_mp_to)) {
        pos_mp_to <- 1L
        name_mp_to <- names_saps_to[1L]
        print(paste0("No sap has been selected, the first one (", name_mp_to, ") will be used."))
    } else if (missing(name_mp_to)) {
        if (pos_mp_to <= RJDemetra::get_all_objects(ws_to) && pos_mp_to > 0L) {
            name_mp_to <- names_saps_to[pos_mp_to]
        } else {
            warning(paste0("There is no SAP n°", pos_mp_to))
            print("The program stops without transferring the series.")
            return(FALSE)
        }
    } else if (missing(pos_mp_to)) {
        if (name_mp_to %in% names_saps_to) {
            pos_mp_to <- which(names_saps_to == name_mp_to)
        } else if (create) {
            print(paste0("There is no SAP named ", name_mp_to, " in ws_to."))
            print("A new SAP in ws_to will be created.")
            RJDemetra::new_multiprocessing(
                workspace = ws_to, name = name_mp_to)
            pos_mp_to <- RJDemetra::count(ws_to)
        } else {
            print(paste0("There is no SAP named ", name_mp_to, " in ws_to."))
            print("The program stops without transferring the series.")
            return(FALSE)
        }
    } else {
        if (!name_mp_to %in% names_saps_to) {
            if (create) {
                print(paste0("There is no SAP named ", name_mp_to, " in ws_to."))
                print("A new SAP in ws_to will be created.")
                RJDemetra::new_multiprocessing(
                    workspace = ws_to, name = name_mp_to)
                pos_mp_to <- RJDemetra::count(ws_to)
            } else {
                print(paste0("There is no SAP named ", name_mp_to, " in ws_to."))
                print("The program stops without transferring the series.")
                return(FALSE)
            }
        } else if (pos_mp_to > RJDemetra::get_all_objects(ws_to) || pos_mp_to <= 0L) {
            warning(paste0("There is no SAP n°", pos_mp_to))
            print("The program stops without transferring the series.")
            return(FALSE)
        } else if (names_saps_to[pos_mp_to] != name_mp_to) {
            warning("The arguments pos_mp_to and name_mp_to are refering to differents objects.")
            print("The program stops without transferring the series.")
            return(FALSE)
        }
    }
    
    
    if (print_indications) {
        cat(paste0("Position of the first WS's SAP : ", pos_mp_from, 
                   "\nand\n", 
                   "Position of the second WS's SAP : ", pos_mp_to, "\n"))
    }
    
    sap_from <- RJDemetra::get_object(ws_from, pos_mp_from)
    sap_to <- RJDemetra::get_object(ws_to, pos_mp_to)
    
    # If a corresponding SAP is found in both workspaces, verification that they are both non empty
    if (RJDemetra::count(sap_from) == 0) {
        warning("The chosen SAP of the workspace ws_to is empty.")
        return(FALSE)
    }
    
    # If all is well:
    # Retrieving the SAPs series...
    
    series_saps_from <- RJDemetra::get_all_objects(sap_from)
    series_saps_to <- RJDemetra::get_all_objects(sap_to)
    
    # ... and their names
    names_series_from <- names(series_saps_from)
    names_series_to <- names(series_saps_to)
    
    # Check de selected series
    if (missing(selected_series)) {
        selected_series <- names_series_from
    } else if (any(!selected_series %in% names_series_from)) {
        warning("The series ", selected_series[!selected_series %in% names_series_from], " are not in the SAP ", names_sap_from, "from ws_from")
    }
    
    if (!replace_series) {
        selected_series <- setdiff(selected_series, names_series_to)
    }
    
    # Replacement of all series specified in the "selected_series" vector
    for (index in seq_along(selected_series)) {
        
        series_name <- selected_series[index]
        position <- which(names_series_from == series_name)
        if (print_indications) {
            cat(paste0("Series n°", index, 
                         ", name: ", series_name, 
                         ", position: ", position))
        }
        
        # The "up-to-date" series version
        extracted_sa_item <- RJDemetra::get_object(sap_from, position)
        
        # Cas de remplacement
        if (series_name %in% names_series_to) {
            cat(" - to add...")
            add_new_sa_item(sap_to, extracted_sa_item)
        } else {
            cat(" - to replace...")
            position <- which(names_series_to == series_name)
            replace_sa_item(mp_to, pos = position, sa_item = extracted_sa_item)
        }
        
        cat(" Successful transfer!\n")
    }
    
    return(TRUE)
}
