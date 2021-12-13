#' Update the metadata from a workspace
#'
#' Functions to update the metadata of a workspace by those contained in another one
#'
#' @param workspace1 Workspace that contains the new metadata.
#' @param workspace2 Workspace to update.
#'
#'
#' @details `update_metadata()` checks the multiprocessings and SaItems' names within
#' the two workspaces before updating workspace2's metadata. `update_metadata_roughly()` does not do any
#' checks: `workspace2`'s first multiprocessing's first SaItem metadata 
#' is updated with `workspace1`'s first multiprocessing's first SaItem metadata.
#' Both functions create and return a new workspace containing the updated series.
#'
#' @examples 
#' workspace1 <- load_workspace("D:/test_metadata/reference.xml")
#' compute(workspace1)
#' workspace2 <- load_workspace("D:/test_metadata/ws_wk.xml")
#' compute(workspace2)
#' 
#' updated_workspace <- update_metadata_roughly(workspace1, workspace2)
#' save_workspace(updated_workspace, "D:/test_metadata/ws_wk_metadata.xml")
#' 
#' updated_workspace <- update_metadata(workspace1, workspace2)
#' save_workspace(updated_workspace, "D:/test_metadata/ws_wk_metadata.xml")
#'
#' @name update_metadata
#' @rdname update_metadata
#' @export
update_metadata <- function(workspace1, workspace2){
  # wk1 <- RJDemetra::load_workspace(workspace1)
  # wk2 <- RJDemetra::load_workspace(workspace2)
  all_mp_wk1 <- RJDemetra::get_all_objects(workspace1)
  all_mp_wk1_names <- names(all_mp_wk1)

  for(i_mp in seq_len(RJDemetra::count(workspace2))){
    mp_wk2 <- RJDemetra::get_object(workspace2, i_mp)
    mp_name <- RJDemetra::get_name(mp_wk2)
    mp_wk1_i <- which(all_mp_wk1_names %in% mp_name)
    if(length(mp_wk1_i) > 0){
      if(length(mp_wk1_i) > 1)
        warning(sprintf('At least 2 multiprocessing called "%s" were found in the workspace1: the first object will be used', mp_name))

      mp_wk1 <- all_mp_wk1[[mp_wk1_i[1]]]
      all_sa_wk1_names <- names(RJDemetra::get_all_objects(mp_wk1))
      for(i_sa in seq_len(RJDemetra::count(mp_wk2))){
        # i_sa <- 2
        sa_wk2 <- RJDemetra::get_object(mp_wk2, i_sa)
        sa_name <- RJDemetra::get_name(sa_wk2)
        sa_wk1_i <- which(all_sa_wk1_names %in% sa_name)
        if(length(sa_wk1_i) > 0){
          if(length(sa_wk1_i) > 1)
            warning(sprintf('At least 2 SaItem called "%s" were found in the workspace1: the first object will be used',sa_name))

          sa_wk1 <- RJDemetra::get_object(mp_wk1, sa_wk1_i[1])
          new_sa_item <- set_metadata(sa_to = sa_wk2,
                                      sa_from = sa_wk1)
          replace_sa_item(mp = mp_wk2, sa_item = new_sa_item,
                          pos = i_sa)
# 
#           sa_def1 <- .jcall(sa_wk1, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
#           jts1 <- .jcall(sa_def1, "Ldemetra/datatypes/Ts;", "getTs")
# 
#           sa_def2 <- .jcall(sa_wk2, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
#           jts2 <- .jcall(sa_def2, "Ldemetra/datatypes/Ts;", "getTs")
#           
          # builder_ts <- jts2$toBuilder()
          # builder_ts$metaData(jts1$getMetaData())
          # jts_temp <- builder_ts$build()
          # builder_sa <- sa_def2$toBuilder()
          # builder_sa$ts(jts_temp)
          # builder_sa$metaData(sa_def1$getMetaData())
          # sa_def_temp <- builder_sa$build()
# 
#           jts_temp <- builder_from_ts(jts2, metaData =  jts1$getMetaData())
#           sa_def_temp <- builder_from_sa(sa_def2, ts = jts_temp,
#                                          metaData = sa_def1$getMetaData())
#           
#           new_sa_item <- .jnew("ec/tstoolkit/jdr/ws/SaItem", sa_def_temp)
#           replace_sa_item(mp = mp_wk2, sa_item = new_sa_item,
#                           pos = i_sa)
#           sa_wk2 <- RJDemetra::get_object(mp_wk2, i_sa)
#           sa_wk1 <- RJDemetra::get_object(mp_wk1, i_sa)
          # builder_ts <- jts2$toBuilder()
          # builder_ts$metaData(jts1$getMetaData())
          # jts_temp <- builder_ts$build()
          # 
          # builder_sa <- sa_def2$toBuilder()
          # builder_sa$ts(jts_temp)
          # builder_sa$metaData(sa_def1$getMetaData())
          # sa_def_temp <- builder_sa$build()


        }else{
          warning(sprintf('The SaItem "%s" is not found in the workspace1',sa_name))
        }
      }

    }else{
      warning(sprintf('The multiprocessing "%s" is not found in the workspace1',mp_name))
    }
  }
  return(workspace2)
}
#' @name update_metadata
#' @rdname update_metadata
#' @export
update_metadata_roughly <- function(workspace1, workspace2){
  # wk1 <- RJDemetra::load_workspace(workspace1)
  # wk2 <- RJDemetra::load_workspace(workspace2)

  for(i_mp in seq_len(RJDemetra::count(workspace2))){
    mp_wk2 <- RJDemetra::get_object(workspace2, i_mp)
    mp_wk1 <- RJDemetra::get_object(workspace1, i_mp)

    for(i_sa in seq_len(RJDemetra::count(mp_wk2))){
      sa_wk2 <- RJDemetra::get_object(mp_wk2, i_sa)
      sa_wk1 <- RJDemetra::get_object(mp_wk1, i_sa)
      # builder_ts <- jts2$toBuilder()
      # builder_ts$metaData(jts1$getMetaData())
      # jts_temp <- builder_ts$build()
      # 
      # builder_sa <- sa_def2$toBuilder()
      # builder_sa$ts(jts_temp)
      # builder_sa$metaData(sa_def1$getMetaData())
      # sa_def_temp <- builder_sa$build()
      new_sa_item <- set_metadata(sa_to = sa_wk2,
                                  sa_from = sa_wk1)
      replace_sa_item(mp = mp_wk2, sa_item = new_sa_item,
                      pos = i_sa)
    }
  }
  return(workspace2)
}

#' Set the metadata of a SaItem
#' 
#' Function to set the name of a `"sa_item"` from the one contained in another `"sa_item"`.
#' 
#' @param sa_to the `"sa_item"` object to modify.
#' @param sa_from the `"sa_item"` object from which the desired metadata is retrieved.
#' 
#' @return a new `"sa_item"` with the model of `sa_to` and the metadata of `sa_from`.
#' @export
set_metadata <- function(sa_to, sa_from){
  sa_def_from <- .jcall(sa_from, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
  jts_from <- .jcall(sa_def_from, "Ldemetra/datatypes/Ts;", "getTs")
  
  sa_def_to <- .jcall(sa_to, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
  jts_to <- .jcall(sa_def_to, "Ldemetra/datatypes/Ts;", "getTs")
  
  jts_update <- builder_from_ts(jts_to, metaData =  jts_from$getMetaData())
  sa_def_update <- builder_from_sa(sa_def_to, ts = jts_update,
                                   metaData = sa_def_from$getMetaData())
  .jnew("ec/tstoolkit/jdr/ws/SaItem", sa_def_update)
}

#' Extract comments
#' 
#' Function to extract the comments of a workspace
#' 
#' @param x the object from which the comments are retrieved.
#' 
#' @export
get_comment <- function(x){
  UseMethod("get_comment", x)
}
#' @export
get_comment.workspace <- function(x){
  multiprocessings <- RJDemetra::get_all_objects(x)
  lapply(multiprocessings, get_comment)
}
#' @export
get_comment.multiprocessing <- function(x){
  all_sa_objects <- RJDemetra::get_all_objects(x)
  lapply(all_sa_objects, get_comment)
}
#' @export
get_comment.sa_item <- function(x){
  sa_def <- .jcall(x, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
  metadata <- sa_def$getMetaData()
  metadata$get("comment")
}

#' Change comment
#' 
#' Function to change the comments of a sa_item object
#' 
#' @param x the sa_item of which the comments will be changed.
#' @param comment the new comment.
#' 
#' @return a new sa_item.
#' 
#' @export
set_comment <- function(x, comment){
  UseMethod("set_comment", x)
}
#' @export
set_comment.sa_item <- function(x, comment){
  sa_def <- .jcall(x, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
  jts <- .jcall(sa_def, "Ldemetra/datatypes/Ts;", "getTs")
  
  metadata <- sa_def$getMetaData()
  
  jmap<-.jnew("java/util/LinkedHashMap")
  jmap <- .jcast(jmap, "java/util/Map")
  jmap$putAll(metadata)
  jmap$put("comment", comment)
  
  sa_def_temp <- builder_from_sa(sa_def,
                                 metaData = jmap)
  new_sa_item <- .jnew("ec/tstoolkit/jdr/ws/SaItem", sa_def_temp)
  new_sa_item
}


#' Update the path to the raw series file
#' 
#' Function to change the path to the raw series file in a workspace
#' 
#' @param ws_path the path to the workspace
#' @param raw_data_path the path to the raw data file
#' @param param the number of lines between a series name and the path to its raw file in the xml file. The default value is 8.
#' @param print_log a boolean to indicate whether to print progress indications.
#' 
#' @return the eventual list of series from the workspace that weren't found in the raw series file
#' 
#' @details - The paths' translation into ASCII is done by the function.
#' - The function operates on a workspace's first SAProcessing (ie. on a whole workspace when it contains one SAP).
#' - The raw series file must be a csv file.
#' - The paths must be "full" (as opposed to relative to the setwd directory, for example).
#' 
#' @examples 
#' # Minimal syntax
#' update_path("my_folder_path/my_workspace.xml","my_folder_path/raw_data_file.csv")
#' # Customisation of the param parameter
#' update_path("my_folder_path/my_workspace.xml","my_folder_path/raw_data_file.csv",5,TRUE)

#' @export

update_path <- function(ws_path, raw_data_path, param=8, print_log = FALSE){
  
  # Verification that the ws_path leads to a valid workspace 
  ws <- load_workspace(ws_path)
  compute(ws)
  if(!inherits(ws, "workspace")){stop("There is an error in the workspace path")}
  # print(ws)
  
  # If the default value of param is used and only print_log is indicated,
  # The logical parameter is re-attributed to print_logical and the default value (of 8), to param
  if(is.logical(param)){print_log <- param ; param=8}
  
  # Conversion of the raw data path into ASCII/JD+ format (folder separators: / or \\)
  raw_data_path_JD5<-gsub(":","%3A", raw_data_path)
  raw_data_path_JD4<-gsub("/","%5C", raw_data_path_JD5)
  raw_data_path_JD3<-gsub("\\\\","%5C", raw_data_path_JD4)
  raw_data_path_JD2<-gsub(" ","+", raw_data_path_JD3)
  raw_data_path_JD1<-gsub(c("_a1"),c(""), raw_data_path_JD2)
  raw_data_path_JD<-gsub(c("_a2"),c(""), raw_data_path_JD1)
  raw_data_path_JD
  
  # Extraction of the path to the workspace main folder
  ws_folder <- gsub("\\.xml$","",ws_path)
  
  # Retrieval of the xml file(s) within the SAProcessing sub-directory
  fic_xml<- list.files(sprintf("%s/SAProcessing",ws_folder),pattern = "\\.xml$")
  if(print_log){print(fic_xml)}
  # "SAProcessing-1.xml"
  
  # Complete path to the xml file(s) within the SAProcessing sub-directory
  ch_fic_xml<-paste0(sprintf("%s/SAProcessing/",ws_folder),fic_xml)
  ch_fic_xml
  # "my_workspace/SAProcessing/SAProcessing-1.xml"
  
  # Import of said xml file(s)
  fic_xml<-readLines(ch_fic_xml)
  
  # Retrieval of the series' names from the csv file
  raw_data<-read.csv2(raw_data_path)
  series_names_csv<-colnames(raw_data)[-1]# date removal
  series_names_csv
  
  # And from the workspace
  series_names_ws <- names(get_all_objects(get_all_objects(ws)[[1]]))
  
  # Series in the workspace that aren't in the csv file
  untreated_series <- series_names_ws[!(series_names_ws %in% series_names_csv)]
  
  # For each series in the workspace:
  for (i in seq(1,length(series_names_ws))){
    # i=1
    series_i <- series_names_ws[i]
    # if it is in the csv file
    if(series_i %in% series_names_csv){
      if(print_log){print(paste0(i,"  ",series_i))}
      # We retrieve the position of the line containing the serie's name (<ts name="C4511"> for ex) in the xml file
      # +8 or +param  = position of the line containing the path to the raw data file 
      pos_path<-grep(paste0('"',series_i,'"'),fic_xml,fixed = TRUE)+param
      
      # We extract the path to the raw data file
      chain1<-unlist(str_split(fic_xml[pos_path],"file="))
      chain2<-unlist(str_split(chain1[2],".csv"))
      # We insert the new raw data file path and put "the line back in place"
      fic_xml[pos_path]<-paste0(chain1[1],"file=",raw_data_path_JD,chain2[2])
    }
  }
  # We save the updated xml file 
  writeLines(fic_xml,ch_fic_xml)
  print("Update completed")
  
  if(length(untreated_series) != 0){
    print("The following series of the workspace aren't in the csv file:")
    return(untreated_series)}
}