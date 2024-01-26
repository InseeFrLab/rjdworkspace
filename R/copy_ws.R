#' Copy a WS
#'
#' @param path_ws the path to the folder containing the WS
#' (the XML file + the WS folder)
#' @param new_path the path to the new folder which will contains the WS
#' (the XML file + the WS folder)
#' @param ws_name the name of the WS
#'
#' @return TRUE
#'
#' @examples
#'
#' # Déplacement d'un WS dans un environnement temporaire
#' rjdworkspace:::copy_ws(
#'   ws_name = "ws_output",
#'   path_ws = file.path(system.file("extdata", package = "rjdworkspace"), "WS")
#' )
#'
copy_ws <- function(ws_name, path_ws, new_path = tempdir()) {
    path_ws <- normalizePath(path_ws)
    new_path <- normalizePath(new_path)

    dir_path <- normalizePath(file.path(path_ws, ws_name))
    xml_path <- normalizePath(file.path(path_ws, paste0(ws_name, ".xml")))

    if (!(dir.exists(dir_path) && file.exists(xml_path))) {
        stop("Le WS n'existe pas.")
    }

    file.copy(
        from = dir_path,
        to = new_path,
        recursive = TRUE,
        overwrite = TRUE
    )
    file.copy(
        from = xml_path,
        to = new_path,
        recursive = TRUE,
        overwrite = TRUE
    )

    return(TRUE)
}
