#' Copy a WS
#'
#' @param ws_name the name of the WS
#' @param from the path to the folder containing the WS (the XML file + the WS
#' folder)
#' @param to the path to the new folder which will contains the WS (the XML
#' file + the WS folder)
#' @param overwrite Overwrite existing file (Defaults to TRUE)
#' @param verbose A boolean to print indications on the processing status
#' (optional and TRUE by default)
#'
#' @export
#'
#' @returns the function returns invisibly (with \code{invisible()}) a boolean
#' specifying if the transfer was done or an error if the specified paths or
#' workspace don't exists
#'
#' @examples
#' # Déplacement d'un WS dans un environnement temporaire
#' destination_dir <- tempdir()
#'
#' # Copy of a worspace in a temporary environment
#' copy_ws(
#'   ws_name = "ws_output",
#'   from = file.path(system.file("extdata", package = "rjdworkspace"), "WS"),
#'   to = destination_dir
#' )
#'
copy_ws <- function(ws_name,
                    from,
                    to = tempdir(),
                    overwrite = TRUE,
                    verbose = TRUE) {

    from <- normalizePath(from, mustWork = TRUE)
    to <- normalizePath(to, mustWork = TRUE)

    from_ws_dir <- normalizePath(file.path(from, ws_name), mustWork = TRUE)
    from_ws_xml <- normalizePath(file.path(from, paste0(ws_name, ".xml")), mustWork = TRUE)

    to_ws_dir <- normalizePath(file.path(to, ws_name), mustWork = FALSE)
    to_ws_xml <- normalizePath(file.path(to, paste0(ws_name, ".xml")), mustWork = FALSE)

    if ((dir.exists(to_ws_dir) && file.exists(to_ws_xml))) {
        message("A workspace already exists in the destination folder.")
        if (overwrite) {
            unlink(to_ws_dir, recursive = TRUE, force = TRUE)
            unlink(to_ws_xml, recursive = TRUE, force = TRUE)
            message("It will be rewritten.")
        } else {
            message("it will not be rewritten.")
            return(invisible(FALSE))
        }
    }

    file.copy(
        from = from_ws_dir,
        to = to,
        recursive = TRUE,
        overwrite = TRUE
    )
    file.copy(
        from = from_ws_xml,
        to = to,
        recursive = TRUE,
        overwrite = TRUE
    )

    if (verbose) {
        cat(" Successful copy!\n")
    }

    return(invisible(TRUE))
}
