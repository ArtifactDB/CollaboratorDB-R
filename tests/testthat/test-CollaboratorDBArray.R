# Check that the CollaboratorDBArray works as expected.
# library(testthat); library(CollaboratorDB); source("test-CollaboratorDBArray.R")

fun <- CollaboratorDB:::.configure_cache()

library(DelayedArray)
test_that("CollaboratorDBArraySeed constructors works as expected", {
    id <- "dssc-test_basic-2023:my_first_sce/assay-1/matrix.h5@2023-01-19"
    mat <- CollaboratorDBArray(id)
    expect_s4_class(mat, "CollaboratorDBArray")
    expect_s4_class(mat, "CollaboratorDBMatrix")
    expect_identical(seed(mat)@id, id)

    # Works with the seed in the constructor.
    mat2 <- CollaboratorDBArray(seed(mat))
    expect_identical(mat2, mat)

    # Resolves latest aliases.
#    lid <- "dssc-test_basic-2023:my_first_sce/assay-1/matrix.h5@latest"
#    lmat <- CollaboratorDBArray(lid)
#    expect_s4_class(lmat, "CollaboratorDBArray")
#    expect_identical(seed(lmat)@id, id)
})

library(DelayedArray)
test_that("CollaboratorDBArraySeed savers works as expected", {
    id <- "dssc-test_basic-2023:my_first_sce/assay-1/matrix.h5@2023-01-19"
    mat <- CollaboratorDBArray(id)

    obj <- List(thingy=mat)
    obj <- annotateObject(obj,
        title="FOO",
        description="I am a list",
        authors="Aaron Lun <infinite.monkeys.with.keyboards@gmail.com>",
        species=9606,
        genome=list(list(id="hg38", source="UCSC")),
        origin=list(list(source="PubMed", id="123456789"))
    )

    dir <- tempfile()
    dir.create(dir)
    saveObject(obj, dir, "bar")

    expect_identical(zircon::extractLinkedID(dir, "bar/child-1/array"), id)

    meta <- jsonlite::fromJSON(file.path(dir, "bar", "child-1", "array.json"))
    expect_identical(meta$path, "bar/child-1/array")
    expect_match(meta[["$schema"]], "sparse_matrix")
})

library(DelayedArray)
test_that("fetchObject redirects to the CollaboratorDB loaders", {
    id <- "dssc-test_basic-2023:my_first_sce/assay-1/matrix.h5@2023-01-19"
    obj <- fetchObject(id)
    expect_s4_class(obj, "CollaboratorDBArray")
})

fun()

