
# Cette fonction retourne :
#   - la position du SAP
#   - une erreur ou un warning si le nom du SAP et de la position ne correspondent à rien
#   - une erreur si le WS est vide (création d'un MP) avec error_on_unknown
#   - sinon 0

identify_object <- function(ws,
                            name_mp, pos_mp, 
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
    names_saps <- RJDemetra::get_all_name(ws)
    
    if (missing(name_mp) && missing(pos_mp)) {
        pos_mp <- 1L
        name_mp <- names_saps[1L]
        print(paste0("No sap has been selected, the first one (", name_mp, ") will be used."))
    } else if (missing(name_mp)) {
        if (pos_mp <= RJDemetra::get_all_objects(ws) && pos_mp > 0L) {
            name_mp <- names_saps[pos_mp]
        } else {
            print("The program stops without transferring the series.")
            stop(paste0("There is no SAP n\u00B0", pos_mp))
        }
    } else if (missing(pos_mp)) {
        if (name_mp %in% names_saps) {
            pos_mp <- which(names_saps == name_mp)
        } else if (error_on_unknown) {
            print("The program stops without transferring the series.")
            stop(paste0("There is no SAP named ", name_mp, "in ws."))
        } else {
            return(0)
        }
    } else {
        if (!name_mp %in% names_saps) {
            if (error_on_unknown) {
                print("The program stops without transferring the series.")
                stop(paste0("There is no SAP named ", name_mp, "in ws."))
            } else {
                return(0)
            }
        } else if (pos_mp > RJDemetra::get_all_objects(ws) || pos_mp <= 0L) {
            print("The program stops without transferring the series.")
            stop(paste0("There is no SAP n\u00B0", pos_mp))
        } else if (names_saps[pos_mp] != name_mp) {
            print("The program stops without transferring the series.")
            stop("The arguments pos_mp and name_mp are refering to differents objects.")
        }
    }
    
    return(pos_mp)
}


#' Transfer_series
#' 
#' To copy & paste series from one workspace to another
#'
#' @param ws_from The workspace containing the additionnal series
#' @param ws_to The workspace to add series to
#' @param selected_series The vector containing the series-to-update's names.
#' @param pos_mp_from The position of the multiprocessing to transfer the series from
#' @param pos_mp_to The position of the multiprocessing to transfer the series to
#' @param name_mp_to The name of the multiprocessing to transfer the series to (optional)
#' @param name_mp_from The name of the multiprocessing to transfer the series from (optional)
#' @param print_indications A boolean to print indications on the processing status (optional)
#' @param create_mp A boolean to create a new multiprocessing if not existing (optional)
#' @param replace_series A boolean to replace existing series (optional)
#'  
#' @details 
#' The transfer will fail if:
#'      - `name_mp_from` doesn't exist
#'      - `pos_mp_from` < 0 or exceed the maximum number of SAP
#'      - `pos_mp_to` < 0 or exceed the maximum number of SAP
#'      - The arguments `pos_mp_from` and `name_mp_from` are refering to differents objects.
#'      - The arguments `pos_mp_to` and `name_mp_to` are refering to differents objects.
#' 
#' If `name_mp_to` and `pos_mp_to` are unspecified, the update will be performed using 
#' the workspaces' first SAProcessing (same for the SAP from).
#' However if the informations of one on the two SAP (from or to) are specified (name or position), they will be attributed by default to the other worskpace. 
#'
#' @return the `workspace` ws_to augmented with series present in ws_from and not already in ws_to
#' @examples \dontrun{transfer_series(ws_from, ws_to, "SAProcessing-1", "MP2", TRUE)}
#' @export
#'
#'
transfer_series <- function(
        ws_from, ws_to, 
        selected_series, 
        pos_mp_from, pos_mp_to, 
        name_mp_from, name_mp_to, 
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
    
    if (missing(name_mp_from) && missing(pos_mp_from)) {
        if (!missing(name_mp_to)) {
            name_mp_from <- name_mp_to
        } else if (!missing(pos_mp_to)) {
            pos_mp_from <- pos_mp_to
        }
    }
    
    if (missing(name_mp_to) && missing(pos_mp_to)) {
        if (!missing(name_mp_from)) {
            name_mp_to <- name_mp_from
        } else if (!missing(pos_mp_from)) {
            pos_mp_to <- pos_mp_from
        }
    }
    
    # Identify object
    pos_mp_from <- identify_object(ws = ws_from,
                                   name_mp = name_mp_from, 
                                   pos_mp = pos_mp_from, 
                                   error_on_unknown = TRUE)
    name_mp_from <- RJDemetra::get_all_name(ws_from)[pos_mp_from]
    
    pos_mp_to <- identify_object(ws = ws_to,
                                 name_mp = name_mp_to, 
                                 pos_mp = pos_mp_to, 
                                 error_on_unknown = !create_mp)
    
    if (pos_mp_to == 0) {
        # A new SAP will be created
        if (missing(name_mp_to)) {
            print("The program stops without transferring the series.")
            stop("No name for mp_to has been specified.")
        } else {
            print(paste0("A new SAP named ", name_mp_to," in ws_to will be created."))
            RJDemetra::new_multiprocessing(
                workspace = ws_to, name = name_mp_to)
            pos_mp_to <- RJDemetra::count(ws_to)
        }
    } else {
        name_mp_to <- RJDemetra::get_all_name(ws_to)[pos_mp_to]
    }
    
    if (print_indications) {
        cat(paste0("First WS's SAP :\n", 
                   "\t- name : ", name_mp_from, 
                   "\n\t- pos : ", pos_mp_from, 
                   "\nand\n", 
                   "Second WS's SAP :\n", 
                   "\t- name : ", name_mp_to, 
                   "\n\t- pos : ", pos_mp_to))
    }
    
    mp_from <- RJDemetra::get_object(x = ws_from, pos = pos_mp_from)
    mp_to <- RJDemetra::get_object(x = ws_to, pos = pos_mp_to)
    
    names_series_from <- RJDemetra::get_all_name(x = mp_from)
    names_series_to <- RJDemetra::get_all_name(x = mp_to)
    
    # Check de selected series
    if (missing(selected_series)) {
        selected_series <- names_series_from
    } else if (any(!selected_series %in% names_series_from)) {
        warning("The series ", setdiff(selected_series, names_series_from), " are not in the SAP ", name_mp_from, "from ws_from. They won't be transfered.")
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
            cat(paste0("Series n\u00B0", index, 
                       ", name: ", series_name, 
                       ", position: ", position))
        }
        
        # The "up-to-date" series version
        extracted_sa_item <- RJDemetra::get_object(mp_from, position)
        
        # Cas de remplacement
        if (series_name %in% names_series_to) {
            if (print_indications) { cat(" - to add...") }
            add_new_sa_item(mp_to, extracted_sa_item)
        } else {
            if (print_indications) { cat(" - to replace...") }
            position <- which(names_series_to == series_name)
            replace_sa_item(mp_to, pos = position, sa_item = extracted_sa_item)
        }
        
        cat(" Successful transfer!\n")
    }
    
    cat("\nDone!")
    return(invisible(ws_to))
}
