
test_that("driver_tiledb", {

  uri <- file.path(withr::local_tempdir(), "test-driver")

  expect_error(driver_tiledb(uri),
               "'storr' not found, please create one.",
               class = "error")

  expect_no_error(dr <- driver_tiledb(uri, init = TRUE, keep_open = FALSE,
                                   hash_algorithm = "sha1"))

  expect_s3_class(dr, "TileDBDriver")
  expect_false(dr$is_open())
  expect_equal(dr$hash_algorithm, "sha1")
  expect_false(dr$members_instantiated)

})


test_that("driver_tiledb_create", {

  uri <- file.path(withr::local_tempdir(), "test-driver")

  expect_true(driver_tiledb_create(uri, hash_algorithm = "sha1"))

  expect_equal(driver_tiledb(uri)$hash_algorithm, "sha1")

})

test_that("driver_tiledb_copy", {

  uri <- file.path(withr::local_tempdir(), "test-driver")
  to_uri <- file.path(withr::local_tempdir(), "test-driver2")

  driver_tiledb_create(uri)
  expect_equal(driver_tiledb_copy(uri, to_uri = to_uri), to_uri)
  expect_no_error(driver_tiledb(to_uri))

})


test_that("driver_tiledb_move", {

  uri <- file.path(withr::local_tempdir(), "test-driver")
  newuri <- file.path(withr::local_tempdir(), "test-driver2")

  driver_tiledb_create(uri)
  expect_equal(driver_tiledb_move(uri, newuri = newuri), newuri)
  expect_no_error(driver_tiledb(newuri))
  expect_error(driver_tiledb(uri),
               "'storr' not found, please create one.",
               class = "error", fixed = TRUE)

})


test_that("driver_tiledb_rename", {

  uri <- file.path(withr::local_tempdir(), "test-driver")
  newname <- "newstorr"
  newuri <- file.path(dirname(uri), newname)

  driver_tiledb_create(uri)
  expect_equal(driver_tiledb_rename(uri, newname = newname), newuri)
  expect_no_error(driver_tiledb(newuri))
  expect_error(driver_tiledb(uri),
               "'storr' not found, please create one.",
               class = "error", fixed = TRUE)

})


test_that("Encrypted storr works with new_context", {

  uri <- file.path(withr::local_tempdir(), "test-cas")

  key <- "5b643a5e173c27d76b3f2af01fcb327b"
  config <- tiledb::tiledb_config()
  config["sm.encryption_type"] <- "AES_256_GCM";
  config["sm.encryption_key"] <- key
  ctx <- R6.tiledb::new_context(config) # not cached, only within driver

  dr <- driver_tiledb(uri, init = TRUE, context = ctx, keep_open = FALSE)
  dr$open(instantiate = TRUE)
  dr$set_hash(key = "a", namespace = "boo", hash = "0102020")
  dr$close()

  # This fails as storr arrays are encrypted and context cannot be found
  expect_error(dr_nokey <- driver_tiledb(uri, init = FALSE, context = NULL))

   # This works because ctx encapsulates encryption key
  expect_no_error(dr_withkey <- driver_tiledb(uri, init = FALSE, context = ctx))

  # Verify
  expect_equal(dr_withkey$get_hash("a", "boo")[], "0102020")

  # Verify that is not leaked to package cached ctx
  ctx2 <- tiledb::tiledb_get_context()
  config2 <- tiledb::config(ctx2)
  expect_equal(config2["sm.encryption_key"][[1]], "")

})
