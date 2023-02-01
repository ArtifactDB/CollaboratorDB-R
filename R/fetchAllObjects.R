#' Fetch all objects for a project
#'
#' Fetch and load all (non-child) objects for a project or one of its versions.
#'
#' @param project String containing the name of the project.
#' @param version String containing the name of the version.
#' @param ... Further arguments to pass to \code{\link{fetchObject}}.
#' @param ignore.redirection.target Logical scalar indicating whether redirection targets should be ignored in the returned listing.
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
#' By default, redirection targets are ignored when reporting objects.
#' For example, if there is a redirection from path A to path B, the redirected path A is reported in the listing, and the redirection target B is ignored.
#' Setting \code{ignore.redirection.target=FALSE} will cause both A and B to be reported, so the same object will be present under different paths.
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
fetchAllObjects <- function(project, version=NULL, ..., ignore.redirection.target=TRUE) {
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
        current <- collected[[v]]

        host <- names(redirects[[v]])
        targets <- unlist(redirects[[v]])

        m <- match(targets, names(current))
        keep <- !is.na(m)
        m <- m[keep]
        host <- host[keep]

        if (ignore.redirection.target) {
            names(current)[m] <- host
        } else {
            copies <- current[m]
            names(copies) <- host
            current <- c(current, copies)
        }

        collected[[v]] <- current
    }

    if (!is.null(version)) {
        collected[[1]]
    } else {
        collected
    }
}


