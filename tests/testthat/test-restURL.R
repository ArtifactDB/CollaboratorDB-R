# This tests that the restURL getter/setter works correctly.
# library(testthat); library(CollaboratorDB); source("test-restURL.R")

test_that("restURL getter/setter works correctly", {
    existing <- restURL()
    expect_match(existing, "collaboratordb")

    old <- restURL("https://foo")
    expect_identical(old, existing)
    expect_match(restURL(), "foo")

    restURL(old)
    expect_identical(restURL(), existing)
})
