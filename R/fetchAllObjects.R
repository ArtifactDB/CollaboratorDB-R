#' Fetch all objects for a project
#'
#' Fetch and load all objects for a project or one of its versions.
#'
#' @param project String containing the name of the project.
#' @param version String containing the name of the version.
#' @param ... Further arguments to pass to \code{\link{fetchObject}}.
#'
#' @return If \code{version} is specified, a named list is returned containing all (non-child) R objects generated loading each resource in that version of the project.
#' The relative path to each resource is used as the name of the corresponding list element.
#'
#' If \code{version=NULL}, a named list is returned containing one inner list per version.
#' Each inner list is named after its version and contains all (non-child) R objects for that version.
#'
#' @details
#' Only non-child objects are returned in each list.
#' Children are used to assemble their parent objects and are not reported here.
#' 
#' For redirections, a copy of the destination object is reported under the redirection's path.
#' This means that some resources may be reported multiple times - once for each redirection, and one for its \dQuote{true} path.
#'
#' @author Aaron Lun
#' @examples
#' # Specific version:
#' fetchAllObjects("dssc-test_basic-2023", version="2023-01-19")
#'
#' # All available versions:
#' fetchAllObjects("dssc-test_basic-2023")
#' 
#' @export
#' @importFrom zircon getProjectMetadata
fetchAllObjects <- function(project, version=NULL, ...) {
    meta <- getProjectMetadata(project, version=version, url=restURL())

    collected <- list()
    redirects <- list()
    for (m in meta) {
        v <- m[["_extra"]][["version"]]

        if (startsWith(m[["$schema"]], "redirection/")) {
            if (!(v %in% names(collected))) {
                redirects[[v]] <- list()
            }
            redirects[[v]][[m$path]] <- m$redirection$targets[[1]]$location
            next
        } 

        if (isTRUE(m$is_child)) {
            next
        }

        out <- fetchObject(packID(project, m$path, v), ...)
        if (!(v %in% names(collected))) {
            collected[[v]] <- list()
        }
        collected[[v]][[m$path]] <- out
    }

    for (v in names(redirects)) {
        if (!(v %in% names(collected))) {
            next
        }

        current.redirects <- redirects[[v]]
        for (n in names(current.redirects)) {
            p <- current.redirects[[n]]
            if (p %in% names(collected[[v]])) {
                collected[[v]][[n]] <- collected[[v]][[p]]
            }
        }
    }

    if (!is.null(version)) {
        collected[[1]]
    } else {
        collected
    }
}


