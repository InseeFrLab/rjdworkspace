
#' Function to automate the replacement of selected series and their metadata by the same series from another workspace


#' This function allows to update a selection of series. It is the partial version of the update_metadata function,
#' of the rjdworkspace package.

#' @param ws1 The workspace to update
#' @param ws2 The workspace containing the most up-to-date version of the selected_series series
#' @param mp_name The name of the multiprocessing containing the series to update
#' @param selected_series The vector containing the series-to-update's names.
#' @param print_indications A boolean to print indications on the processing status

#' @rdname replace_series
#' @return the updated \code{workspace}
#' @examples \dontrun{replace_series(ws1, ws2, "SAProcessing-1", c("serie1", "serie2"), TRUE)}
#' @export
#'
replace_series <- function(ws1, ws2, mp_name, selected_series, print_indications=FALSE){
  
  # Verification of the parameters type
  if(!inherits(ws1, "workspace")){stop("The first argument must be a workspace")}
  if(!inherits(ws2, "workspace")){stop("The second argument must be a workspace")}
  if(is.null(selected_series)){stop("The selected_series list is empty!")}
    else {
      if(!is.character(selected_series)){stop("The selected_series list must contain characters (the series names).")}
      }
  
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
  # "sap" refers to all SAPs of each workspace
  # "mp" refers to the specified "mp_name" SAP to update
  
  # Retrieving all SAPs and their names
  saps1 <- RJDemetra::get_all_objects(ws1)
  names_saps1 <- names(saps1)
  
  saps2 <- RJDemetra::get_all_objects(ws2)
  names_saps2 <- names(saps2)
  
  # Verification that both workspaces contain the SAP to be updated
  # and storage of its position in each workspace
  pos_mp1 <- which(names_saps1 == mp_name)
  pos_mp2 <- which(names_saps2 == mp_name)
  
  # End of the program if the SAP can't be found in one of the workspaces
  if(is.null(pos_mp1)){
    print("The chosen SAP couldn't be found in the first workspace.")
    return(FALSE)
  }
  
  if(is.null(pos_mp2)){
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
  series_saps1 <- RJDemetra::get_all_objects(saps1[[pos_mp1]])
  series_saps2 <- RJDemetra::get_all_objects(saps2[[pos_mp2]])
  
  # ... and their names
  names_series1 <- names(series_saps1)
  names_series2 <- names(series_saps2)
  
  # Verification that all series in the list "selected_series" are present in both SAPs
  L <- length(selected_series)
  pos_table <- data.frame(selected_series,  rep(NA,L),  rep(NA,L))
  names(pos_table) <- c("selected_series", "pos_series1", "pos_series2")
  
  for (i in seq_len(L)){
    pos_table$pos_series1[i] <- which(selected_series[i] == names_series1)
    pos_table$pos_series2[i] <- which(selected_series[i] == names_series2)
  }
  
  verif <- pos_table[is.na(pos_table$pos_series1)||is.na(pos_table$pos_series2),]
  if (nrow(verif) != 0){
    print("Attention, the following series are missing in at least one SAP: fix it and recompile the program!")
    return(verif)
  } else {
    
    # Retrieving both SAPs that will be used (possibly identically named)
    mp1 <- saps1[[pos_mp1]]
    mp2 <- saps2[[pos_mp2]]
    
    # Replacement of all series specified in the "selected_series" vector
    for (i in seq_len(L)){
      #i=1
      if (print_indications) {print(paste("Series", i))}
      
      # The "up-to-date" series version
      replacing_series <- RJDemetra::get_object(mp2, pos_table$pos_series2[i])
      
      if (print_indications) {print(get_name(replacing_series))}
      
      # Replacement of the series by its updated version (change made in the reference workspace)
      replace_sa_item(mp1, pos_table$pos_series1[i], replacing_series)
      
      if (print_indications) {print("ok")}
    }
    
    print(paste0("Series updating done for the SAP ", mp_name,"."))
    
    return(ws1)
    
  }
}


#' Auxiliary functions

#' Generic function to identify and return the duplicates in a list

#' @param s a list of characters
#' 
#' @rdname replace_series
#' @return If there are no duplicates, the function returns an empty data frame. 
#' Otherwise, it returns a data frame giving the name and number of duplicates found within the argument (list).
#' @examples \dontrun{
#' s <- c("a","b","a","c","a","c")
#'result <- verif_duplicates(s)
#'result
#' }
verif_duplicates <- function(s){
  s <- as.factor(s)
  ta <- table(s)
  lev <- levels(s)
  res <- data.frame(levels(s), ta) 
  colnames(res) <- c("name","s","Freq")
  res2 <- res[res$Freq>1,c("name","Freq")]
  return(res2)  
}



#' Function to identify duplicated series in a SAProcessing (SAP) or of SAProcessings in a workspace

#' @param ws The workspace to scan
#' @param print_indications A boolean to print indications on the processing status
#' @rdname replace_series
#' @return a list containing the name and number of occurences of duplicated SAPs and series 
#' @export
verif_ws_duplicates <- function(ws, print_indications=FALSE){
  
  if(!inherits(ws, "workspace")){stop("The argument must be a workspace")}
  
  RJDemetra::compute(ws)
  nb_sap <- RJDemetra::count(ws)
  result <- c()
  
  # Error message if the workspace is empty
  if (is.na(nb_sap) || nb_sap <=0) {
    warning("The workspace is empty, no duplicates could be found.") 
    return(FALSE)
  } else { 
    if (print_indications){print(paste("The workspace contains", nb_sap,"SAP(s)"))}
  }
  
  # If the workspace contains more than one SAP, we verify that no two have the same name 
  sap_duplicates <- c()
  if (nb_sap >= 1) {
    sap_ws <- RJDemetra::get_all_objects(ws)
    # if (is.null(count(sap_ws))){return()}
    sap_names <- names(sap_ws)
    sap_duplicates <- verif_duplicates(sap_names)
    ## "sap_duplicates" is expected to be FALSE
  }
  
  # Then all series are scanned, one SAP at a time :
  if (nb_sap != 1 && sum(sap_duplicates$Freq) != 0) {
    (sprintf('Attention, the following SAPs are duplicated in the workspace: "%s"', sap_duplicates$nom))
  } else {
    
    print("Ok: no SAP duplicate")
    
    # If the workspace isn't empty and there are no SAP duplicates, the series scan begins
    sap_ws <- RJDemetra::get_all_objects(ws)
    
    # For each sap
    for(i in seq_len(nb_sap)){# i=2
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
      if (nrow(v) == 0) {print(paste("Ok: the SAP", sap_name, "does not contain any duplicated series."))
      } else { 
        # pb with warnings: they are printed last, after the output vector v and all at once
        # warning(sprintf('Attention, the SAP "%s" (position %d) contains multiples of at least one series (cf. supra)', sap_name, i))
        print(paste0("Attention! The SAP ", sap_name, " (position ", i, ") contains at least one duplicated series:"))
        print(v)
        result <- c(result,v)
      } 
    }
  }
  if(sum(sap_duplicates$Freq) != 0){result <- c(sap_duplicates,result)}
  return(result)
}

