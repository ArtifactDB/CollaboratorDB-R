#' Add CollaboratorDB annotation
#'
#' Add CollaboratorDB's required annotation to a Bioconductor \linkS4class{Annotated} object.
#'
#' @param x An \linkS4class{Annotated} object.
#' @param title String containing the object's title.
#' @param description String containing the description of the object.
#' @param authors  List containing the identity of the maintainers.
#' Each entry may be a \code{\link{person}}-formatted string, or a list containing the \code{name}, \code{email} and \code{orcid} strings.
#' @param species An integer vector of NCBI taxonomy IDs for the species relevant to the data in \code{x}. 
#' @param genome List describing the genomes involved in constructing \code{x}.
#' Each entry should be a list containing:
#' \itemize{
#' \item \code{id}, a string containing the genome build ID (e.g., hg38, NCBIm37)
#' \item \code{type}, a string specifying the type of ID (e.g., UCSC, Ensembl)
#' }
#' Check out \url{https://artifactdb.github.io/CollaboratorDB-schemas/array/v1.html#allOf_i0_genome} for details.
#' @param origin List describing the origin of \code{x}.
#' Each entry should be a list containing:
#' \itemize{
#' \item \code{source}, a string describing the source repository, e.g., PubMed, GEO, ArrayExpress.
#' \item \code{id}, a string containing an identifier within \code{source}.
#' }
#' Check out \url{https://artifactdb.github.io/CollaboratorDB-schemas/array/v1.html#allOf_i0_origin} for details.
#' @param terms List of ontology terms relevant to this resource.
#' Each entry should be a list containing:
#' \itemize{
#' \item \code{id}, a string containing an ID for an ontology term, e.g., \code{"EFO:0008913"}, \code{"DOID:13250"}, \code{"CL:0000097"}, \code{"UBERON:0005870"}.
#' \item \code{source}, a string containing the name of the ontology.
#' Currently, only Experimental Factor Ontology, Human Disease Ontology, Cell Ontology and UBERON are supported.
#' \item \code{version}, a string containing the version of the ontology.
#' This may be any tag on the corresponding GitHub repository for that ontology, e.g., \url{https://github.com/EBISPOT/efo/releases}.
#' }
#' Check out \url{https://artifactdb.github.io/CollaboratorDB-schemas/array/v1.html#allOf_i0_terms} for details.
#' @param annotation List of CollaboratorDB-relevant metadata to use for annotating \code{x}.
#' 
#' @return 
#' For \code{annotateObject}, \code{x} is returned with extra fields in its \code{\link{metadata}}.
#'
#' For \code{objectAnnotation}, a list of CollaboratorDB-relevant metadata is returned.
#'
#' For \code{setAnnotation}, \code{x} is returned after replacing the CollaboratorDB-relevant metadata with \code{annotation}.
#' 
#' @examples
#' library(S4Vectors)
#' df <- DataFrame(X=1:10, Y=LETTERS[1:10], Z=factor(letters[1:10]))
#'
#' df <- annotateObject(df,
#'     title="FOO",
#'     description="I am a data frame",
#'     authors="Aaron Lun <luna@gene.com>",
#'     species=9606,
#'     genome=list(list(id="hg38", source="UCSC")),
#'     origin=list(list(source="PubMed", id="123456789")),
#'     terms=list(list(id="EFO:0008913", source="Experimental Factor Ontology", version="v3.46.0"))
#' )
#'
#' anno <- objectAnnotation(df)
#' str(anno)
#'
#' anno$authors <- c(anno$authors, 
#'     list(list(name="Darth Vader", email="vader@empire.gov")))
#' df <- setAnnotation(df, anno)
#' 
#' @author Aaron Lun
#'
#' @export
#' @importFrom S4Vectors metadata<- metadata
#' @importFrom utils as.person
annotateObject <- function(x, title, description, authors, species, genome, origin, terms=list()) {
    meta <- list(
        title=title,
        description=description,
        authors=authors,
        species=species,
        genome=genome,
        origin=origin,
        terms=terms
    )

    setAnnotation(x, meta)
}

#' @export
#' @rdname annotateObject
#' @importFrom S4Vectors metadata
objectAnnotation <- function(x) metadata(x)[[".internal"]][["CollaboratorDB"]]

#' @export
#' @rdname annotateObject
#' @importFrom S4Vectors metadata<- metadata
setAnnotation <- function(x, annotation) {
    meta <- metadata(x)
    if (!(".internal" %in% names(meta))) {
        meta[[".internal"]] <- list()        
    }
    meta[[".internal"]][["CollaboratorDB"]] <- annotation
    metadata(x) <- meta
    x
}
