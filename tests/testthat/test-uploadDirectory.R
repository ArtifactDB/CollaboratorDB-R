# Tests that the uploadDirectory function works as expected.
# library(testthat); library(CollaboratorDB); source("setup-private.R"); source("test-uploadDirectory.R")

token <- Sys.getenv("GITHUB_TOKEN", NA)
if (is.na(token)) {
    skip("skipping upload tests because GITHUB_TOKEN is absent")
}

setAccessToken(token, cache=FALSE)
tmp <- tempfile()
dir.create(tmp)
df <- exampleObject()
saveObject(df, tmp, "my_first_df")

tmp_project <- "test-upload-2019"

test_that("uploading works correctly for a new project", {
    v <- as.integer(Sys.time())
    uploadDirectory(tmp, tmp_project, v, expires=1)

    cache <- tempfile()
    stuff <- fetchObject(zircon::packID(tmp_project, "my_first_df", v), cache=cache)

    expect_identical(stuff$X, df$X)
    expect_identical(stuff$Y, df$Y)

    anno <- objectAnnotation(stuff)
    expect_identical(anno[["_extra"]][["version"]], as.character(v))
    expect_type(anno[["_extra"]][["transient"]][["expires_in"]], "character")

    expect_true(length(zircon::getPermissions(tmp_project, restURL())$owners) == 1L)
})

test_that("uploading works correctly for an existing project", {
    v <- as.integer(Sys.time())
    uploadDirectory(tmp, tmp_project, v, expires=1)

    cache <- tempfile()
    stuff <- fetchObject(zircon::packID(tmp_project, "my_first_df", v), cache=cache)

    expect_identical(stuff$X, df$X)
    expect_identical(stuff$Y, df$Y)

    anno <- objectAnnotation(stuff)
    expect_identical(anno[["_extra"]][["version"]], as.character(v))
    expect_type(anno[["_extra"]][["transient"]][["expires_in"]], "character")
})
