# Check that cloneDirectory works as expected.
# library(testthat); library(CollaboratorDB); source("test-cloneDirectory.R")

test_that("cloning a project works as expected", {
    tmp <- tempfile()
    cloneDirectory(tmp, "dssc-test_basic-2023", "2023-01-19")
    expect_true(length(list.files(tmp)) > 0)

    # Check that we can actually load stuff.
    tmp <- tempfile()
    cloneDirectory(tmp, "dssc-test_basic-2023", "2023-01-19", link.only=FALSE)
    meta <- alabaster.base::acquireMetadata(tmp, "my_first_sce")
    obj <- alabaster.base::loadObject(meta, tmp)
    expect_s4_class(obj, "SingleCellExperiment")
})

