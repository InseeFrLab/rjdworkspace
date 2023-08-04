#' Transfer_series
#' 
#' To copy&paste series from one workspace to another
#'
#' @param ws_from The workspace containing the additionnal series
#' @param ws_to The workspace to add series to
#' @param mp_to_name The name of the multiprocessing to transfer the series to
#' @param mp_from_name The name of the multipricessing to transfer the series from
#' @param print_indications A boolean to print indications on the processing status (optional)
#'  
#' @details If `mp_to_name` and `mp_from_name` are unspecified, the update will be performed using 
#' the workspaces' first SAProcessing. If only one SAProcessing name is specified but the workspace 
#' it refers to is not (e.g. argument `SAP1` instead of `mp_from_name="SAP1"`), it will be 
#' attributed by default to the first worskpace (`mp_to_name="SAP1"`). 
#'
#' @return the `workspace` ws_to augmented with series present in ws_from and not already in ws_to
#' @examples \dontrun{transfer_series(ws_from, ws_to, "SAProcessing-1", "MP2", TRUE)}
#' @export
#'
#'
transfer_series <- function(
        ws_from, ws_to, 
        mp_from_name = NULL, mp_to_name = NULL, 
        print_indications = FALSE, 
        create = TRUE) {
    
    # Verification of the parameters type
    if (!inherits(ws_to, "workspace")) {
        stop("The argument ws_to must be a workspace")
    }
    if (!inherits(ws_from, "workspace")) {
        stop("The argument ws_from must be a workspace")
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
    
    saps_to <- RJDemetra::get_all_objects(ws_to)
    names_saps_to <- names(saps_to)
    
    saps_from <- RJDemetra::get_all_objects(ws_from)
    names_saps_from <- names(saps_from)
    
    # if SAPs have been specified, verification that both workspaces contain the SAP
    # and storage of its position in each workspace
    
    # if no SAP has been declared, the first of each workspaces will be used
    pos_mp_to <- 1  
    pos_mp_from <- 1
    
    # otherwise, we retrieve the SAP's position in each workspace 
    if (!missing(mp_to_name)) {
        pos_mp_to <- which(names_saps_to == mp_to_name)
    }
    if (!missing(mp_from_name)) {
        pos_mp_from <- which(names_saps_from == mp_from_name)
    }
    
    # End of the program if the SAP can't be found in one of the workspaces
    # ie. an unexisting SAP was specified
    if (all(pos_mp_from == 0)) {
        warning("The chosen SAP couldn't be found in the first workspace.")
        return(FALSE)
    }
    
    if (all(pos_mp_to == 0) || RJDemetra::count(ws_to) == 0) {
        print("The chosen SAP doesn't exist in the second workspace.")
        if (create) {
            if (missing(mp_to_name)) {
                warning("No name for mp_to has been specified.")
                print("The program stops without transferring the series.")
                return(FALSE)
            } else {
                print("It will be created.")
                RJDemetra::new_multiprocessing(
                    workspace = ws_to, name = mp_to_name)
                pos_mp_to <- RJDemetra::count(ws_to)
            }
        } else {
            warning("The program stops without transferring the series.")
            return(FALSE)
        }
    }
    
    if (print_indications) {
        cat(paste0("Position of the first WS's SAP : ", pos_mp_from, 
                   "\nand\n", 
                   "Position of the second WS's SAP : ", pos_mp_to, "\n"))
    }
    
    sap_to <- RJDemetra::get_object(ws_to, pos_mp_to)
    sap_from <- RJDemetra::get_object(ws_from, pos_mp_from)
    
    # If a corresponding SAP is found in both workspaces, verification that they are both non empty
    if (RJDemetra::count(sap_from) == 0) {
        warning("The chosen SAP of the workspace ws_to is empty.")
        return(FALSE)
    }
    
    # If all is well:
    # Retrieving the SAPs series...
    # RJDemetra::count(saps_to[[pos_mp_to]])
    # RJDemetra::count(saps_from[[pos_mp_from]])
    
    series_saps_to <- RJDemetra::get_all_objects(sap_to)
    series_saps_from <- RJDemetra::get_all_objects(sap_from)
    
    # ... and their names
    names_series_to <- names(series_saps_to)
    names_series_from <- names(series_saps_from)
    
    # The list of series to transfer: the series in ws_from that are not in ws_to
    selected_series <- setdiff(names_series_from, names_series_to)
    
    # Replacement of all series specified in the "selected_series" vector
    for (index in seq_along(selected_series)) {
        
        series_name <- selected_series[index]
        position <- which(names_series_from == series_name)
        if (print_indications) {
            print(paste0("Series n°", index, 
                         ", name: ", series_name, 
                         ", position: ", position))
        }
        
        # The "up-to-date" series version
        extracted_sa_item <- RJDemetra::get_object(sap_from, position)
        
        # Replacement of the series by its updated version (change made in the reference workspace)
        add_new_sa_item(sap_to, extracted_sa_item)
        
        print("Successful transfer")
    }
    
    return(TRUE)
}
