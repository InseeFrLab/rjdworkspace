#' Transfer_series
#' 
#' To copy&paste series from one workspace to another
#'
#' @param ws1 The workspace to add series to
#' @param ws2 The workspace containing the additionnal series
#' @param mp_to The name of the multiprocessing to transfer the series to
#' @param mp_from The name of the multipricessing to transfer the series from
#' @param print_indications A boolean to print indications on the processing status (optional)
#'  
#' @details If `mp_to` and `mp_from` are unspecified, the update will be performed using 
#' the workspaces' first SAProcessing. If only one SAProcessing name is specified but the workspace 
#' it refers to is not (e.g. argument `SAP1` instead of `mp_from="SAP1"`), it will be 
#' attributed by default to the first worskpace (`mp_to="SAP1"`). 
#'
#' @return the `workspace` ws1 augmented with series present in ws2 and not already in ws1
#' @examples \dontrun{replace_series(ws1, ws2, "SAProcessing-1", "MP2", TRUE)}
#' @export
#'
#'
transfer_series <- function(ws1, ws2, mp_to=NA, mp_from=NA, print_indications=FALSE){
  
  # Verification of the parameters type
  if(!inherits(ws1, "workspace")){stop("The first argument must be a workspace")}
  if(!inherits(ws2, "workspace")){stop("The second argument must be a workspace")}
  
  # if no SAProcessing name is specified but "print_indications" is
  # ie. if mp_to is TRUE or FALSE
  if(!is.na(mp_to)&inherits(mp_to,"logical")){
    print_indications <- mp_to 
    mp_to <- NA}
  
  
  # Check that the workspaces aren't empty
  if(is.null(count(ws1))){
    warning("Attention, the first workspace is empty!")
    return(FALSE)
  }
  
  if(is.null(count(ws2))){
    warning("Attention, the second workspace is empty!")
    return(FALSE)
  }
  
  # When both workspaces aren't empty:

  # Retrieving all SAPs and their names
  # "sap" refers to all SAPs of each workspace
  # "mp" refers to the specified "mp_name" SAP to update
  
  saps1 <- RJDemetra::get_all_objects(ws1)
  names_saps1 <- names(saps1)
  
  saps2 <- RJDemetra::get_all_objects(ws2)
  names_saps2 <- names(saps2)
  
  # if SAPs have been specified, verification that both workspaces contain the SAP
  # and storage of its position in each workspace
  
  # if no SAP has been declared, the first of each workspaces will be used
  pos_mp1 <- 1  
  pos_mp2 <- 1
  
  
  # otherwise, we retrieve the SAP's position in each workspace 
  if(!is.na(mp_to)){pos_mp1 <- which(names_saps1 == mp_to)}
  if(!is.na(mp_from)){pos_mp2 <- which(names_saps2 == mp_from)}
  
  if(print_indications){print(paste0("pos_mp1=",pos_mp1, " and pos_mp2=",pos_mp2))}
  
  
  # End of the program if the SAP can't be found in one of the workspaces
  # ie. an unexisting SAP was specified
  if(sum(pos_mp1)==0||is.null(pos_mp1)||is.na(pos_mp1)){
    print("The chosen SAP couldn't be found in the first workspace.")
    return(FALSE)
  }
  
  if(sum(pos_mp2)==0||is.null(pos_mp2)||is.na(pos_mp2)){
    print("The chosen SAP couldn't be found in the second workspace.")
    return(FALSE)
  }
  
  # If a corresponding SAP is found in both workspaces, verification that they are both non empty
  if (is.null(count(saps1[[pos_mp1]]))){
    print("The chosen SAP of the first workspace is empty.")
    return(FALSE)
  }
  
  if (is.null(count(saps2[[pos_mp2]]))){
    print("The chosen SAP of the second workspace is empty.")
    return(FALSE)
  }
  
  # If all is well:
  # Retrieving the SAPs series...
  count(saps1[[pos_mp1]])
  count(saps2[[pos_mp2]])
  series_saps1 <- RJDemetra::get_all_objects(saps1[[pos_mp1]])
  series_saps2 <- RJDemetra::get_all_objects(saps2[[pos_mp2]])
  
  # ... and their names
  names_series1 <- names(series_saps1)
  names_series2 <- names(series_saps2)
  
  # The list of series to transfer: the series in ws2 that are not in ws1
  selected_series <- names_series2[!(names_series2 %in% names_series1)]
  
  # Position of these series in the ws2
  L <- length(selected_series)
  pos_table <- data.frame(selected_series, rep(NA,L))
  names(pos_table) <- c("selected_series", "pos_series2")
  
  for (i in seq_len(L)){
    pos_table$pos_series2[i] <- which(selected_series[i] == names_series2)
  }
  
  verif <- pos_table[sum(pos_table$pos_series2)==0,]
  if (nrow(verif) != 0){
    print("Attention, the following series are missing in the ws2: fix it and recompile the program!")
    return(verif$selected_series)
    
  } else {
    
    # Retrieving both SAPs that will be used (possibly identically named)
    mp1 <- saps1[[pos_mp1]]
    mp2 <- saps2[[pos_mp2]]
    
    # Replacement of all series specified in the "selected_series" vector
    for (i in seq_len(L)){
      #i=1
      if (print_indications) {print(paste("Series", i))}
      
      # The "up-to-date" series version
      replacement_series <- RJDemetra::get_object(mp2, pos_table$pos_series2[i])
      series_name <- get_name(replacement_series)
      
      if (print_indications) {print(series_name)}
      
      # Replacement of the series by its updated version (change made in the reference workspace)
      #add_sa_item(ws1, mp_to, replacement_series, series_name)
      add_new_sa_item(mp1, replacement_series)
      
      if (print_indications) {print("ok")}
    }
    
    print("Successful transfer")
    
    
    return(ws1)
    
  }
}
