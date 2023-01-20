#' @export
#' @import DelayedArray methods
#' @importClassesFrom alabaster.matrix WrapperArraySeed
setClass("CollaboratorDBArraySeed", contains="WrapperArraySeed", slots=c(id="character"))

#' @export
#' @importClassesFrom DelayedArray DelayedArray
setClass("CollaboratorDBArray", contains="DelayedArray", slots=c(seed="CollaboratorDBArraySeed"))

#' @export
#' @importClassesFrom DelayedArray DelayedMatrix
setClass("CollaboratorDBMatrix", contains=c("DelayedMatrix", "CollaboratorDBArray"), slots=c(seed="CollaboratorDBArraySeed"))

#' The CollaboratorDBArray class
#'
#' The CollaboratorDBArray is a \pkg{DelayedArray} wrapper around a on-disk array downloaded from the CollaboratorDB backend.
#' Specifically, it remembers the zircon ID that was used to download the array, which can be used to avoid a redundant write and upload in \code{\link{saveObject}} and \code{\link{uploadDirectory}}, respectively.
#' The actual heavy-lifting is done by forwarding all operations to the internal \pkg{DelayedArray}-compatible seed object,
#' which may be any of the usual classes, e.g., a HDF5ArraySeed, H5SparseMatrixSeed or any of its delayed operations.
#'
#' @param id String containing the CollaboratorDB identifier, or a list containing the unpacked components of an identifier (from \code{\link{unpackID}}).
#' For \code{CollaboratorDBArray}, this can also be a CollaboratorDBArraySeed object.
#' @param seed A CollaboratorDBArraySeed object.
#' @param ... Further arguments to pass to the CollaboratorDBArraySeed constructor.
#'
#' @return
#' The \code{CollaboratorDBArraySeed} constructor returns a CollaboratorDBArraySeed object.
#'
#' The \code{CollaboratorDBArray} constructor and the \code{\link{DelayedArray}} method return a CollaboratorDBArray object (or a CollaboratorDBMatrix, for two dimensions).
#' 
#' @details
#' If \code{id} uses the \code{latest} version alias, this is automatically resolved to a full version by the \code{CollaboratorDBArraySeed} constructor.
#' This ensures that the version is explicitly pinned in downstream applications and when saving to file.
#' 
#' @docType class
#' @aliases
#' CollaboratorDBArray-class
#' CollaboratorDBMatrix-class
#' matrixClass,CollaboratorDBArray-method
#' DelayedArray,CollaboratorDBArraySeed-method
#' CollaboratorDBArraySeed-class
#' loadArray
#'
#' @examples
#' \dontshow{fun <- CollaboratorDB:::.configure_cache()}
#' id <- "dssc-test_basic-2023:my_first_sce/assay-1/matrix.h5@2023-01-19"
#' mat <- CollaboratorDBArray(id)
#' mat
#' \dontshow{fun()}
#'
#' @name CollaboratorDBArray
NULL

#' @export
#' @rdname CollaboratorDBArray
#' @importFrom alabaster.matrix .createRawArraySeed
#' @importFrom alabaster.base acquireMetadata acquireFile
#' @importFrom zircon resolveLatestVersion
CollaboratorDBArraySeed <- function(id) {
    if (!is.list(id)) {
        unpacked <- unpackID(id)
    } else {
        unpacked <- id
        id <- do.call(packID, unpacked[c("project", "path", "version")])
    }

    if (unpacked$version == "latest") {
        unpacked$version <- resolveLatestVersion(unpacked$project, restURL())
        id <- do.call(packID, unpacked[c("project", "path", "version")])
    }

    proj <- new("CollaboratorDBHandler", project=unpacked$project, version=unpacked$version)
    info <- acquireMetadata(proj, unpacked$path)
    seed <- .createRawArraySeed(info, acquireFile(proj, unpacked$path))

    new("CollaboratorDBArraySeed", id=id, seed=seed)
}

#' @export
#' @rdname CollaboratorDBArray
CollaboratorDBArray <- function(id, ...) {
    if (!is(id, "CollaboratorDBArraySeed")) {
        id <- CollaboratorDBArraySeed(id, ...)
    }
    DelayedArray(id)
}

#' @export
#' @rdname CollaboratorDBArray
setMethod("DelayedArray", "CollaboratorDBArraySeed", function(seed) new_DelayedArray(seed, Class="CollaboratorDBArray"))

#' @export
setMethod("matrixClass", "CollaboratorDBArray", function(x) "CollaboratorDBMatrix")

#' @export
loadArray <- function(info, project) {
    CollaboratorDBArray(list(project=project@project, path=info$path, version=project@version))
}
