# This tests the various saveObject utilities.
# library(testthat); library(CollaboratorDB); source("test-saveObject.R")

library(S4Vectors)
test_that("saveObject works as expected", {
    df <- exampleObject()

    tmp <- tempfile()
    dir.create(tmp)
    saveObject(df, tmp, "foo")

    f <- list.files(tmp)
    expect_true("foo.json" %in% f)
    expect_true("foo" %in% f)

    meta <- alabaster.base::acquireMetadata(tmp, "foo")
    roundtrip <- CollaboratorDB:::cdbLoadObject(meta, tmp)

    expect_identical(objectAnnotation(roundtrip)$authors[[1]]$name, "Aaron Lun")
    expect_match(objectAnnotation(roundtrip)$authors[[1]]$email, ".com")

    # Fixing the metadata before comparison.
    metadata(df)$.internal$CollaboratorDB$authors <- objectAnnotation(roundtrip)$authors
    metadata(df)$.internal$CollaboratorDB$species <- as.list(metadata(df)$.internal$CollaboratorDB$species)
    expect_equal(df, roundtrip)
})

test_that("saveObject fails on attempts to save inside subdirectories", {
    df <- exampleObject()

    tmp <- tempfile()
    dir.create(tmp)
    saveObject(df, tmp, "foo1")
    saveObject(df, tmp, "foo2")
    dir.create(file.path(tmp, "foo3"))
    saveObject(df, tmp, "foo3/boo")

    f <- list.files(tmp)
    expect_true("foo1.json" %in% f)
    expect_true("foo1" %in% f)
    expect_true("foo2.json" %in% f)
    expect_true("foo2" %in% f)
    expect_true("foo3" %in% f)
    expect_true(file.exists(file.path(tmp, "foo3/boo.json")))

    expect_error(saveObject(df, tmp, "foo1/boo"), "cannot save")
})

test_that("saveObject emits verbose messages properly", {
    df <- exampleObject()

    tmp <- tempfile()
    dir.create(tmp)
    expect_message(saveObject(df, tmp, "foo", verbose=TRUE), "staging <DFrame>")

    # Checking that the object is still okay.
    meta <- alabaster.base::acquireMetadata(tmp, "foo")
    roundtrip <- CollaboratorDB:::cdbLoadObject(meta, tmp)

    expect_identical(objectAnnotation(roundtrip)$authors[[1]]$name, "Aaron Lun")
    expect_match(objectAnnotation(roundtrip)$authors[[1]]$email, ".com")

    # Fixing the metadata before comparison.
    metadata(df)$.internal$CollaboratorDB$authors <- objectAnnotation(roundtrip)$authors
    metadata(df)$.internal$CollaboratorDB$species <- as.list(metadata(df)$.internal$CollaboratorDB$species)
    expect_equal(df, roundtrip)
})


