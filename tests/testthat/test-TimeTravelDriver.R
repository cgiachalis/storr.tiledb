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

})

# NB: 'TimeTravelBDriver' is a subset (copy) of 'TileDBDriver'. Here, we're
# performing basic testing in order catch / isolate any issue early.
# Time-travel testing will be carried out with 'StorrTimeTravel' class.

test_that("'get_hash'/'mget_hash'", {
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


test_that("'get_keymeta'/'mget_keymeta' and friends", {

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
