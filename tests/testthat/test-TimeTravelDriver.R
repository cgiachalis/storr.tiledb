test_that("'TimeTravelBDriver'", {

  uri <- file.path(withr::local_tempdir(), "test-driver")
  expect_no_error(dr <- TimeTravelDriver$new(uri))

  expect_r6_class(dr, "TimeTravelDriver")
  expect_false(dr$exists())
  expect_equal(dr$mode, "CLOSED")
  expect_equal(dr$type(), "tiledb")

  # public fields
  expect_equal(dr$traits, list(accept = "string",
                               throw_missing = TRUE))
  # public fields are locked
  expect_error(dr$traits <- "boo")

  # Nothing to retrieve - <TimeDriver> object does not exist
  expect_error(dr$tiledb_timestamp)
  expect_error(dr$members_instantiated)
  expect_error(dr$hash_algorithm)

  # create a storr
  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE)

  dr <- TimeTravelDriver$new(uri)

  expect_equal(dr$tiledb_timestamp, R6.tiledb::set_tiledb_timestamp())
  expect_true(dr$members_instantiated)
  expect_equal(dr$hash_algorithm, "md5")

  # Cannot change hash algo
  expect_error(dr$hash_algorithm <- "sha1")

  # Cannot open a TimeTravel when 'hash_algo' is missing (broken storr)
  dr$close()
  grp <- R6.tiledb::tdb_group(uri)
  R6.tiledb::delete_metadata(grp, "hash_algo")
  expect_null(R6.tiledb::metadata(grp, "hash_algo"))

  expect_error(dr$open(),
               "Hash algorithm not found, cannot open TileDB 'storr'",
               class = "error", fixed = TRUE)

})


test_that("'TimeTravelBDriver' with missing 'serial_format'", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, serial_format = "qdata")

  # delete serial_format metadata from group (we could do it on CAS directly)
  grp <- R6.tiledb::tdb_group(uri)
  expect_equal(grp$get_metadata("serial_format"), "qdata")
  R6.tiledb::delete_metadata(grp, "serial_format")
  expect_null(R6.tiledb::metadata(grp, "serial_format"))

  # 'serial_format' is not found, error
  expect_error( TimeTravelDriver$new(uri), "Serialisation format not found.",
               class = "error", fixed = TRUE)

})



test_that("'TimeTravelBDriver' with invalid 'serial_format'", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, serial_format = "qdata")

  # delete serial_format metadata from group (we could do it on CAS directly)
  grp <- R6.tiledb::tdb_group(uri)
  expect_equal(grp$get_metadata("serial_format"), "qdata")
  grp$reopen("WRITE")
  grp$set_metadata(list(serial_format = "invalid"))
  grp$close()

  # Unknown serialization format, error
  expect_error(TimeTravelDriver$new(uri))

})


# NB: 'TimeTravelBDriver' is a subset (copy) of 'TileDBDriver'. Here, we're
# performing basic testing in order catch / isolate any issue early.
# Time-travel testing will be carried out with 'StorrTimeTravel' class.

test_that("'get_hash'/'mget_hash'", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, default_namespace = "ns1")
  sto$mset(c("a", "b"), c("a", "b"))
  hashes <- sto$mget_hash(c("a", "b"))

  dr <- TimeTravelDriver$new(uri)

  expect_equal(dr$mget_hash(c("a", "b"), "ns1"), hashes)
  expect_equal(dr$get_hash("a", "ns1"), hashes[1])

  # exists_hash
  expect_true(dr$exists_hash("a", "ns1"))
  expect_equal(dr$exists_hash(c("a", "c"), "ns1"), c(TRUE, FALSE))

  # listing methods
  expect_equal(dr$list_hashes(), hashes)
  expect_equal(dr$list_keys("ns1"), c("a", "b"))
  expect_equal(dr$list_namespaces(), "ns1")
  expect_equal(dr$list_unused_hashes(), character(0))

})

test_that("'get_object'/'mget_object'", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, default_namespace = "ns1")
  sto$mset(c("a", "b"), c("a", "b"))
  hashes <- sto$mget_hash(c("a", "b"))

  dr <- TimeTravelDriver$new(uri)

  expect_equal(dr$mget_object(hashes), list("a", "b"))
  expect_equal(dr$mget_object(c(hashes, "no-hash")), list("a", "b", NULL))
  expect_equal(dr$get_object(hashes[1]), "a")

  # exists_object
  expect_all_true(dr$exists_object(hashes))
  expect_equal(dr$exists_object(c(hashes, "no-hash")), c(TRUE, TRUE, FALSE))

})

test_that("'get_object'/'mget_object' with 'qs2' and 'qdata' serialization format", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)

  # 'qs2' format
  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, serial_format = "qs2")
  sto$mset(c("a", "b"), c("a", "b"))
  hashes <- sto$mget_hash(c("a", "b"))

  dr <- TimeTravelDriver$new(uri)

  expect_equal(dr$mget_object(hashes), list("a", "b"))
  expect_equal(dr$mget_object(c(hashes, "no-hash")), list("a", "b", NULL))
  expect_equal(dr$get_object(hashes[1]), "a")

  # exists_object
  expect_all_true(dr$exists_object(hashes))
  expect_equal(dr$exists_object(c(hashes, "no-hash")), c(TRUE, TRUE, FALSE))


  sto$destroy()


  # 'qdata' format
  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, serial_format = "qdata")
  sto$mset(c("a", "b"), c("a", "b"))
  hashes <- sto$mget_hash(c("a", "b"))

  dr <- TimeTravelDriver$new(uri)

  expect_equal(dr$mget_object(hashes), list("a", "b"))
  expect_equal(dr$mget_object(c(hashes, "no-hash")), list("a", "b", NULL))
  expect_equal(dr$get_object(hashes[1]), "a")

  # exists_object
  expect_all_true(dr$exists_object(hashes))
  expect_equal(dr$exists_object(c(hashes, "no-hash")), c(TRUE, TRUE, FALSE))


})

test_that("'get_keymeta'/'mget_keymeta' and friends", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, default_namespace = "ns1")
  sto$mset(c("a", "b"), c("a", "b"),
           notes = c("notes-a", "notes-b"),
           expires_at = c(as.POSIXct(NA), as.POSIXct("1990-01-01")))

  dr <- TimeTravelDriver$new(uri)

  trg <- list(list(as.POSIXct(NA), "notes-a"),
               list(as.POSIXct("1990-01-01"), "notes-b"))
  expect_equal(dr$mget_keymeta(c("a", "b"), "ns1"), trg, ignore_attr = TRUE)

  trg <- list(list(as.POSIXct(NA), "notes-a"),
              list(NULL))
  expect_equal(dr$mget_keymeta(c("a", "c"), "ns1"), trg, ignore_attr = TRUE)


  expect_equal(dr$get_keymeta("a", "ns1"), trg[[1]], ignore_attr = TRUE)

  expect_equal(dr$keys_with_expiration("ns1")[][["key"]]$as_vector(), "b")
  expect_equal(dr$keys_without_expiration("ns1")[][["key"]]$as_vector(), "a")

  expect_equal(dr$expired_keys("ns1")[][["key"]]$as_vector(), "b")
  expect_equal(dr$unexpired_keys("ns1")[][["key"]]$as_vector(), character(0))

  expect_equal(dr$num_expired_keys("ns1"), 1)
  expect_equal(dr$num_unexpired_keys("ns1"), 0)

  expect_true(dr$has_expired_keys("ns1"))
  expect_false(dr$has_unexpired_keys("ns1"))

})


test_that("'get_keymeta_unit'/'mget_keymeta_unit'", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, default_namespace = "ns1")
  sto$mset(c("a", "b"), c("a", "b"),
           notes = c("notes-a", "notes-b"),
           expires_at = c(as.POSIXct(NA), as.POSIXct("1990-01-01")))

  dr <- TimeTravelDriver$new(uri)

  # expires_at
  expect_equal(dr$get_keymeta_unit("b", "ns1", "expires_at"), as.POSIXct("1990-01-01"), ignore_attr = TRUE)

  trg <- structure(list(as.POSIXct(NA), as.POSIXct("1990-01-01")), missing = integer(0))
  expect_equal(dr$mget_keymeta_unit(c("a","b"), "ns1", "expires_at"), trg, ignore_attr = TRUE)


  # notes
  expect_equal(dr$get_keymeta_unit("b", "ns1", "notes"), "notes-b")

  trg <- structure(list("notes-a", "notes-b"), missing = integer(0))
  expect_equal(dr$mget_keymeta_unit(c("a","b"), "ns1", "notes"), trg)

  trg <- structure(list("notes-a", "notes-b", "no-val"), missing = 3L)
  expect_equal(dr$mget_keymeta_unit(c("a","b", "d"), "ns1", nomatch = "no-val",
                                    meta_col = "notes"), trg)

})
