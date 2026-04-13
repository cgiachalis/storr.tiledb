test_that("'StorrFragments' object", {

  uri <- file.path(withr::local_tempdir(), "sto1")
  uri_nostorr <- file.path(withr::local_tempdir(), "not-a-storr")

  sto <- storr_tiledb(uri, init = TRUE)

  expect_error(StorrFragments$new(uri_nostorr),
               "'storr' not found, please create one.", class = "error", fixed = TRUE)

  expect_no_error(fosto <- StorrFragments$new(uri))
  expect_s3_class(fosto, c("StorrFragments", "R6"), exact = TRUE)

  # Active fields
  expect_s3_class(fosto$size, "vfs_size")
  expect_s3_class(fosto$fob_keys, c("TileDBFragments", "R6"), exact = TRUE)
  expect_s3_class(fosto$fob_data, c("TileDBFragments", "R6"), exact = TRUE)
  expect_error(fosto$size <- 1)
  expect_error(fosto$fob_keys <- 1)
  expect_error(fosto$fob_data <- 1)

  # basic methods
  expect_equal(fosto$frag_num(), 0)
  expect_equal(fosto$to_vacuum_num(), 0)
  expect_true(fosto$consolidate())
  expect_true(fosto$vacuum())
  expect_invisible(fosto$reload_finfo())

})

test_that("'StorrFragments' with actual data", {

  uri <- file.path(withr::local_tempdir(), "sto1")
  sto <- storr_tiledb(uri, init = TRUE)

  sto$set("a", 1)
  sto$set("b", 2)
  sto$set("c", 3)

  expect_no_error(fosto <- StorrFragments$new(uri))

  expect_equal(fosto$frag_num(), 6)
  expect_equal(fosto$to_vacuum_num(), 0)

  sto$clr_keymeta("a")

  # Fragment object is out of sync
  expect_equal(fosto$frag_num(), 6)

  # Reload fragment info object to pick the new commits
  expect_no_error(fosto$reload_finfo())
  expect_equal(fosto$frag_num(), 7)

  # Write new key, but with existing value
  sto$set("d", 3)
  # out of sync
  expect_equal(fosto$frag_num(), 7)
  expect_no_error(fosto$reload_finfo())
  # in sync
  expect_equal(fosto$frag_num(), 8)

  # Write new key, new value
  sto$set("e", 4)
  # out of sync
  expect_equal(fosto$frag_num(), 8)
  expect_no_error(fosto$reload_finfo())
  # in sync
  expect_equal(fosto$frag_num(), 10)

  # Just in case ...
  expect_equal(fosto$to_vacuum_num(), 0)
})

test_that("'StorrFragments' consolidate method", {

  uri <- file.path(withr::local_tempdir(), "sto1")
  sto <- storr_tiledb(uri, init = TRUE)

  sto$set("a", 1)
  sto$set("b", 2)
  sto$set("c", 3)

  fosto <- StorrFragments$new(uri)

  # 'all' consolidation  ---
  expect_true(fosto$consolidate(what = "all"))
  expect_equal(fosto$frag_num(), 2)
  expect_equal(fosto$to_vacuum_num(), 6)

  # New commit
  sto$set("d", 4)
  fosto$reload_finfo()
  expect_equal(fosto$frag_num(), 4)
  expect_equal(fosto$to_vacuum_num(), 6)

  # 'keys' consolidation ---
  expect_true(fosto$consolidate(what = "keys"))
  expect_equal(fosto$frag_num(), 3)
  # 2 frag merged, so vacuum num goes by two
  expect_equal(fosto$to_vacuum_num(), 8)

  # 'data' consolidation  ---
  expect_true(fosto$consolidate(what = "data"))
  expect_equal(fosto$frag_num(), 2)

  # 2 frag merged, so vacuum num goes by two
  expect_equal(fosto$to_vacuum_num(), 10)
})

test_that("'StorrFragments' consolidate method with vacuum operation", {

  uri <- file.path(withr::local_tempdir(), "sto1")
  sto <- storr_tiledb(uri, init = TRUE)

  sto$set("a", 1)
  sto$set("b", 2)
  sto$set("c", 3)

  fosto <- StorrFragments$new(uri)

  # 'all' consolidation  ---
  expect_true(fosto$consolidate(what = "all", vacuum = TRUE))
  expect_equal(fosto$frag_num(), 2)
  expect_equal(fosto$to_vacuum_num(), 0)

  # New commit
  sto$set("d", 4)
  fosto$reload_finfo()
  expect_equal(fosto$frag_num(), 4)
  expect_equal(fosto$to_vacuum_num(), 0)

  # 'keys' consolidation ---
  expect_true(fosto$consolidate(what = "keys", vacuum = TRUE))
  expect_equal(fosto$frag_num(), 3)
  expect_equal(fosto$to_vacuum_num(), 0)

  # 'data' consolidation  ---
  expect_true(fosto$consolidate(what = "data", vacuum = TRUE))
  expect_equal(fosto$frag_num(), 2)
  expect_equal(fosto$to_vacuum_num(), 0)
})

test_that("'StorrFragments' vacuum method", {

  uri <- file.path(withr::local_tempdir(), "sto1")
  sto <- storr_tiledb(uri, init = TRUE)

  sto$set("a", 1)
  sto$set("b", 2)
  sto$set("c", 3)

  fosto <- StorrFragments$new(uri)

  # 'all' consolidation  ---
  expect_true(fosto$consolidate(what = "all"))
  expect_true(fosto$vacuum(what = "all"))
  expect_equal(fosto$to_vacuum_num(), 0)

  # New commit
  sto$set("d", 4)
  fosto$reload_finfo()

  expect_true(fosto$consolidate(what = "all"))
  expect_equal(fosto$frag_num(), 2)
  expect_equal(fosto$to_vacuum_num(), 4)

  expect_true(fosto$vacuum(what = "keys"))
  expect_equal(fosto$to_vacuum_num(), 2)

  expect_true(fosto$vacuum(what = "data"))
  expect_equal(fosto$to_vacuum_num(), 0)

})

# Test config is respected
