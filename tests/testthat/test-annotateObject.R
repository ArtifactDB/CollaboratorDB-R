# This tests the various annotation utilities.
# library(testthat); library(CollaboratorDB); source("test-annotateObject.R")

library(S4Vectors)
test_that("annotation utilities work as expected", {
    df <- exampleObject()
    expect_true(".internal" %in% names(metadata(df)))

    meta <- objectAnnotation(df)
    expect_type(meta$authors, "character")

    # Annotation is wiped.
    wipe <- setAnnotation(df, NULL)
    expect_null(objectAnnotation(wipe))
})
