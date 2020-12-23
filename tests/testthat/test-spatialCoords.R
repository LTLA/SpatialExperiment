data(ve)

test_that("'cd_keep = NULL' keeps spatial coordinates only", {
    x <- colnames(spatialCoords(ve))
    y <- grep("^(x|y|z)_coord$", names(colData(ve)), value = TRUE)
    expect_identical(x, y)
})

test_that("invalid 'cd_keep' throws error", {
    expect_error(spatialCoords(ve, cd_keep = "foo"))
})

test_that("valid 'cd_keep' are retained", {
    cd_keep <- sample(names(colData(ve)), 3)
    x <- spatialCoords(ve, cd_keep = cd_keep)
    expect_true(all(cd_keep %in% colnames(x)))
})

test_that("'as.df = FALSE' returns matrix", {
    x <- spatialCoords(ve, as.df = FALSE)
    expect_is(x, "matrix")
})

test_that("'as.df = TRUE' returns data.frame", {
    x <- spatialCoords(ve, as.df = TRUE)
    expect_is(x, "data.frame")
})

test_that("'sample_id = TRUE' retains all samples", {
    x <- spatialCoords(ve, sample_id = TRUE)
    expect_identical(nrow(x), ncol(ve))
})

test_that("'sample_id = NULL' retains first sample", {
    x <- spatialCoords(ve, sample_id = NULL)
    expect_identical(nrow(x), table(ve$sample_id)[[1]])
})

test_that("'sample_id = c(.)' retains specified sample", {
    s <- sample(unique(ve$sample_id), 1)
    x <- spatialCoords(ve, sample_id = s)
    y <- colData(ve)[ve$sample_id == s, colnames(x)]
    expect_identical(x, as.matrix(y))
})
