# This tests the multi-object fetching function.
# library(testthat); library(CollaboratorDB); source("test-fetchAllObjects.R")

test_that("fetchAllObjects works as expected", {
    objects <- fetchAllObjects("dssc-test_basic-2023", version="2023-01-19")
    expect_s4_class(objects$my_first_sce, "SingleCellExperiment")
    expect_s4_class(objects$my_first_df, "DataFrame")

    more.objects <- fetchAllObjects("dssc-test_basic-2023")
    expect_identical(names(more.objects), "2023-01-19")
    expect_identical(objects, more.objects[[1]])
})

test_that("fetchAllObjects includes redirection targets if requested", {
    objects <- fetchAllObjects("dssc-test_basic-2023", version="2023-01-19", ignore.redirection.target=FALSE)
    expect_s4_class(objects$my_first_sce, "SingleCellExperiment")
    expect_s4_class(objects$my_first_df, "DataFrame")

    expect_identical(objects$my_first_sce, objects[["my_first_sce/experiment.json"]])
    expect_identical(objects$my_first_df, objects[["my_first_df/simple.csv.gz"]])
})
