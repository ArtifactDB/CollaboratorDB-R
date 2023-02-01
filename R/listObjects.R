#' List objects for a project
#'
#' List available objects for a project or one of its versions.
#'
#' @param project String containing the name of the project.
#' @param version String containing the name of the version.
#' @param exclude.child Logical scalar indicating whether to exclude child objects.
#' @param ignore.redirection.target Logical scalar indicating whether redirection targets should be ignored in the returned listing.
#'
#' @return If \code{version} is specified, a data frame is returned containing the path and ID for each object,
#' along with the metadata fields described in \code{\link{annotateObject}} (if available).
#'
#' If \code{version=NULL}, a named list is returned containing one data frame per version.
#' Each data frame is named after its version and lists all objects for that version.
#'
#' @details
#' Only non-child objects are guaranteed to contain the additional metadata fields described in \code{\link{annotateObject}}.
#' Child objects may or may not contain extra metadata, as their parents are assumed to take responsibility for describing them.
#'
#' By default, redirection targets are ignored when reporting objects.
#' For example, if there is a redirection from path A to path B, the redirected path A is reported in the listing, and the redirection target B is ignored.
#' Setting \code{ignore.redirection.target=FALSE} will cause both A and B to be reported, so the same object will be present under different paths.
#'
#' @author Aaron Lun
#' @examples
#' # Specific version:
#' listObjects("dssc-test_basic-2023", version="2023-01-19")
#'
#' # All available versions:
#' listObjects("dssc-test_basic-2023")
#' 
#' @export
#' @importFrom zircon getProjectMetadata packID
#' @importFrom S4Vectors DataFrame List
listObjects <- function(project, version=NULL, exclude.child=TRUE, ignore.redirection.target=TRUE) {
    meta <- getProjectMetadata(project, version=version, url=restURL())

    collected <- list()
    redirects <- list()
    defaulter <- function(x, default) { 
        if (is.null(x)) {
            default
        } else {
            x
        }
    }

    for (m in meta) {
        v <- m[["_extra"]][["version"]]

        if (startsWith(m[["$schema"]], "redirection/")) {
            if (!(v %in% names(collected))) {
                redirects[[v]] <- list()
            }
            redirects[[v]][[m$path]] <- m$redirection$targets[[1]]$location
            next
        } 

        if (isTRUE(m$is_child) && exclude.child) {
            next
        }

        if (!(v %in% names(collected))) {
            collected[[v]] <- list(
                path = character(0),
                title = character(0),
                description = character(),
                authors = list(),
                species = list(),
                genome = list(),
                origin = list(),
                terms = list()
            )
        }

        current <- collected[[v]]
        current$path <- c(current$path, m$path)
        current$title <- c(current$title, defaulter(m$title, NA_character_))
        current$description <- c(current$description, defaulter(m$description, NA_character_))
        current$authors <- c(current$authors, list(defaulter(m$authors, list())))
        current$species <- c(current$species, list(defaulter(m$species, numeric(0))))
        current$genome <- c(current$genome, list(defaulter(m$genome, list())))
        current$origin <- c(current$origin, list(defaulter(m$origin, list())))
        current$terms <- c(current$terms, list(defaulter(m$terms, list())))
        collected[[v]] <- current
    }

    for (v in names(collected)) {
        current <- collected[[v]]
        for (col in names(current)) {
            curcol <- current[[col]]
            if (is.list(curcol)) {
                current[[col]] <- List(curcol)
            }
        }
        collected[[v]] <- DataFrame(current)
    }

    # Adding redirections.
    for (v in names(redirects)) {
        if (!(v %in% names(collected))) {
            next
        }
        current <- collected[[v]]

        current.redirects <- redirects[[v]]
        host <- names(current.redirects)
        dest <- unlist(current.redirects)

        m <- match(dest, current$path)
        keep <- !is.na(m)
        m <- m[keep]
        host <- host[keep]

        if (ignore.redirection.target) {
            current$path[m] <- host
        } else {
            redirected <- current[m,,drop=FALSE]
            redirected$path <- host
            current <- rbind(current, redirected)
        }

        collected[[v]] <- current
    }

    # Adding IDs.
    for (v in names(collected)) {
        current <- collected[[v]]
        current$id <- packID(project, current$path, v)
        collected[[v]] <- current
    }

    if (!is.null(version)) {
        collected[[1]]
    } else {
        collected
    }
}
