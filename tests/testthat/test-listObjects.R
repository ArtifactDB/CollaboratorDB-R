# This tests the object listing function.
# library(testthat); library(CollaboratorDB); source("test-listObjects.R")

test_that("listObjects works as expected", {
    listing <- listObjects("dssc-test_basic-2023", version="2023-01-19")

    expect_true("my_first_sce" %in% listing$path)
    expect_true("my_first_df" %in% listing$path)
    expect_false(any(is.na(listing$title)))
    expect_false(any(is.na(listing$description)))

    more.listing <- listObjects("dssc-test_basic-2023")
    expect_identical(names(more.listing), "2023-01-19")
    expect_identical(listing, more.listing[[1]])
})

test_that("listObjects happily reports all child objects as well", {
    listing <- listObjects("dssc-test_basic-2023", version="2023-01-19", exclude.child=FALSE)
    expect_true(nrow(listing) > 10)

    expect_true("my_first_sce" %in% listing$path)
    expect_true("my_first_df" %in% listing$path)

    expect_true(any(grepl("/", listing$path)))
    expect_true(any(is.na(listing$title)))
    expect_true(any(is.na(listing$description)))
})

test_that("listObjects will include redirection targets", {
    listing <- listObjects("dssc-test_basic-2023", version="2023-01-19", ignore.redirection.target=FALSE)
    everything.else <- setdiff(colnames(listing), c("path", "id"))

    expect_true("my_first_sce" %in% listing$path)
    expect_identical(listing[listing$path == "my_first_sce",everything.else], listing[listing$path == "my_first_sce/experiment.json",everything.else])

    expect_true("my_first_df" %in% listing$path)
    expect_identical(listing[listing$path == "my_first_df",everything.else], listing[listing$path == "my_first_df/simple.csv.gz",everything.else])
})
