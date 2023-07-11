format_path_to_xml <- function(path) {
    return(path |> 
               normalizePath() |> 
               gsub(pattern = ":", replacement = "%3A") |> 
               gsub(pattern = "/", replacement = "%5C") |> 
               gsub(pattern = "\\\\", replacement = "%5C") |> 
               gsub(pattern = " ", replacement = "+"))
}

update_one_xml <- function(xml_path, pos_sa_item, formated_data_path) {
    
    a <- XML::xmlParse(xml_path)
    
    nodes_metadata <- a |> 
        XML::xmlChildren() |> base::`[[`("informationSet") |> 
        # Premier SA-ITEM (l'indice 1 est réservé pour les metadata du SA-processing)
        XML::xmlChildren() |> base::`[[`(1 + pos_sa_item) |> 
        # Metadata file
        XML::xmlChildren() |> 
        base::`[[`("subset") |> 
        base::`[[`("item") |> 
        base::`[[`("ts") |> 
        base::`[[`("metaData") |> 
        XML::xmlChildren()
    
    id_pos <- which(sapply(X = nodes_metadata, FUN = XML::xmlAttrs)["name", ] == "@id")
    
    node_to_change <- nodes_metadata |> 
        # path node
        base::`[[`(id_pos)
    
    attrib <- node_to_change |> XML::xmlAttrs()
    
    chain_temp <- attrib["value"] |> strsplit(split = "file=") |> unlist()
    chain1 <- chain_temp[1]
    chain_temp <- chain_temp[2] |> strsplit(split = "#series") |> unlist()
    chain2 <- chain_temp[2]
    
    attrib["value"] <- paste0(chain1, "file=", formated_data_path, "#series", chain2)
    XML::xmlAttrs(node_to_change) <- attrib
    XML::saveXML(a, file = xml_path)
    
    return(invisible(NULL))
}

check_information <- function(ws_xml_path, pos_mp, pos_sa_item) {
    
    # Verification that the ws_xml_path leads to a valid workspace 
    ws <- RJDemetra::load_workspace(ws_xml_path)
    compute(ws)
    if (!inherits(ws, "workspace")) {
        stop("There is an error in the workspace path")
    }
    
    ws_folder_path <- gsub(pattern = "\\.xml$", replacement = "", 
                           x = ws_xml_path)
    all_xml_sap <- list.files(sprintf("%s/SAProcessing", ws_folder_path), 
                              pattern = "\\.xml$")
    
    if ((!missing(pos_mp)) && (!paste0("SAProcessing-", pos_mp, ".xml") %in% all_xml_sap)) {
        stop("Le multiprocessing n'existe pas.")
    }
    
    if ((!missing(pos_sa_item)) && missing(pos_mp)) {
        stop("Il faut pr\u00e9ciser un multiprocessing.")
    }
    
    nb_sap <- ws |> get_all_objects() |> length()
    if (!missing(pos_mp)) {
        if (nb_sap < pos_mp) {
            stop("Le multiprocessing n'existe pas.")
        }
        
        nb_sa_item <- ws |> get_object(pos_mp) |> get_all_objects() |> length()
        if (!missing(pos_sa_item)) {
            if (nb_sa_item < pos_sa_item) {
                stop("Le SA Item n'existe pas.")
            }
        }
    }
    
    return(invisible(NULL))
}

#' Update the path to the raw series file
#' 
#' @description
#' Fonction de changement de chemin (pour les données brutes)
#' Cette fonction fonctionne pour les formats csv, xls et xlsx
#' 
#' Function to change the path to the raw series file in a workspace
#' 
#' @export
update_path <- function(ws_xml_path, raw_data_path, pos_mp, pos_sa_item) {
    
    if (!tools::file_ext(raw_data_path) %in% c("csv", "xls", "xlsx")) {
        stop("Les seuls formats de donn\u00e9e accept\u00e9s sont csv, xls, xlsx.")
    }
    
    # Verification that the ws_xml_path leads to a valid workspace 
    ws <- RJDemetra::load_workspace(ws_xml_path)
    compute(ws)
    if (!inherits(ws, "workspace")) {
        stop("There is an error in the workspace path")
    }
    
    ws_folder_path <- gsub(pattern = "\\.xml$", replacement = "", 
                           x = ws_xml_path)
    all_xml_sap <- list.files(sprintf("%s/SAProcessing", ws_folder_path), 
                              pattern = "\\.xml$")
    
    if ((!missing(pos_mp)) && (!paste0("SAProcessing-", pos_mp, ".xml") %in% all_xml_sap)) {
        stop("Le multiprocessing n'existe pas.")
    }
    
    if ((!missing(pos_sa_item)) && missing(pos_mp)) {
        stop("Il faut pr\u00e9ciser un multiprocessing.")
    }
    
    nb_sap <- ws |> get_all_objects() |> length()
    if (!missing(pos_mp)) {
        if (nb_sap < pos_mp) {
            stop("Le multiprocessing n'existe pas.")
        }
        
        nb_sa_item <- ws |> get_object(pos_mp) |> get_all_objects() |> length()
        if (!missing(pos_sa_item)) {
            if (nb_sa_item < pos_sa_item) {
                stop("Le SA Item n'existe pas.")
            }
        }
    }
    
    new_raw_data_path <- format_path_to_xml(raw_data_path)
    
    if (missing(pos_mp)) {
        
        all_xml_sap <- list.files(sprintf("%s/SAProcessing", ws_folder_path), 
                                  pattern = "\\.xml$", full.names = TRUE)
        for (pos_mp in seq_along(all_xml_sap)) {
            
            xml_path <- all_xml_sap[pos_mp]
            nb_sa_item <- ws |> get_object(pos_mp) |> get_all_objects() |> length()
            
            for (pos2 in seq_len(nb_sa_item)) {
                update_one_xml(xml_path = xml_path, 
                               pos_sa_item = pos2, 
                               formated_data_path = new_raw_data_path)
            }
        }
    } else {
        xml_path <- paste0(sprintf("%s/SAProcessing/", ws_folder_path), 
                           "SaProcessing-", pos_mp, ".xml")
        
        if (missing(pos_sa_item)) {
            for (pos in seq_len(nb_sa_item)) {
                update_one_xml(xml_path = xml_path, 
                               pos_sa_item = pos, 
                               formated_data_path = new_raw_data_path)
            }
        } else {
            update_one_xml(xml_path = xml_path, 
                           pos_sa_item = pos_sa_item, 
                           formated_data_path = new_raw_data_path)
        }
    }
    
    print("Done!")
    return(invisible(NULL))
}

