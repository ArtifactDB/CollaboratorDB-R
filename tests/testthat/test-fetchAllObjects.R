# This tests the multi-object fetching function.
# library(testthat); library(CollaboratorDB); source("test-fetchAllObjects.R")

test_that("fetchAllObjects works as expected", {
    objects <- fetchAllObjects("dssc-test_basic-2023", version="2023-01-19")
    expect_s4_class(objects$my_first_sce, "SingleCellExperiment")
    expect_identical(objects$my_first_sce, objects[["my_first_sce/experiment.json"]])
    expect_s4_class(objects$my_first_df, "DataFrame")
    expect_identical(objects$my_first_df, objects[["my_first_df/simple.csv.gz"]])

    more.objects <- fetchAllObjects("dssc-test_basic-2023")
    expect_identical(objects, more.objects[[1]])
})
