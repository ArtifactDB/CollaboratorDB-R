#' Clone a project to a directory
#' 
#' Clone a specified version of an existing project into a directory,
#' typically for modifications prior to uploading a new version of the project via \code{\link{uploadDirectory}}.
#'
#' @inheritParams zircon::cloneProjectVersion
#' @param cache Logical scalar indicating whether caching should be performed.
#' If \code{TRUE}, a default cache location is chosen.
#'
#' Alternatively, a string specifying the path to a cache directory.
#'
#' Alternatively, a \linkS4class{BiocFileCache} object.
#' @param force.update Logical scalar indicating whether cache entries should be forcibly updated.
#' Useful for fixing corrupted or incomplete files in the cache.#'
#'
#' @return A directory is created at \code{dir} with the contents of the specified project version.
#' \code{NULL} is invisibly returned.
#'
#' @author Aaron Lun
#' @examples
#' tmp <- tempfile()
#' cloneDirectory(tmp, "dssc-test_basic-2023", "2023-01-19")
#' list.files(tmp, recursive=TRUE)
#'
#' @export
#' @importFrom zircon cloneProjectVersion
cloneDirectory <- function(dir, project, version, link.only=TRUE, cache=TRUE, force.update=FALSE) {
    cfun <- .configure_cache(cache, force.update=force.update)
    on.exit(cfun(), add=TRUE)
    cloneProjectVersion(dir, restURL(), project, version, link.only=link.only, cache=.create_cache_function())
}
