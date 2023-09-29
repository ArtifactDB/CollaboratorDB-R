globals <- new.env()
globals$cache.object <- NULL
globals$cache.update <- FALSE
globals$rest.url <- "https://demodb.api.artifactdb.io"

#' @importFrom tools R_user_dir
.cache_directory <- function() {
    R_user_dir("CollaboratorDB", which="cache")
}
