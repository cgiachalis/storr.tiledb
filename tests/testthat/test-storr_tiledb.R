
test_that("storr_tiledb", {

  uri <- file.path(withr::local_tempdir(), "test-driver")

  expect_error(storr_tiledb(uri),
               "'storr' not found, please create one.",
               class = "error")

  expect_no_error(st <- storr_tiledb(uri,
                                     init = TRUE,
                                     keep_open = FALSE,
                                     hash_algorithm = "sha1"))

  expect_s3_class(st, c("TileDBStorr", "R6"), exact = TRUE)
  # expect_true(st$driver$is_open())
  # expect_true(st$driver$members_instantiated)
  # expect_equal(st$hash_algorithm, "sha1")

  rm(st)
  uri <- file.path(withr::local_tempdir(), "test-driver")

  driver_tiledb_create(uri)
  expect_error(storr_tiledb(uri, init = TRUE))

  expect_s3_class(storr_tiledb(uri, init = FALSE), c("TileDBStorr", "R6"), exact = TRUE)

})


test_that("storr_tiledb with custom schemas", {

  uri <- file.path(withr::local_tempdir(), "test-driver")

  ctx <- new_context()
  cdr <- driver_schemas(none_filter = TRUE, ctx = ctx)

  # Set up a ZSTD filter with high compression
  flt <- tiledb::tiledb_filter("ZSTD", ctx = ctx)
  flt <- tiledb::tiledb_filter_set_option(flt,"COMPRESSION_LEVEL", 22)
  fl_list <- tiledb::tiledb_filter_list(flt, ctx = ctx)

  cdr$SchemaData$attr_value <- fl_list

  expect_no_error(st <- storr_tiledb(uri,
                                     init = TRUE,
                                     keep_open = FALSE,
                                     driver_schemas = cdr))

  # Check created driver
  dr <- driver_schemas(uri, ctx = ctx)

  trg_filters <- data.frame(
    list(
      hash = c("NONE", "NONE"),
      value = c("ZSTD", "22"),
      coords = c("NONE", "NONE"),
      offsets = c("NONE", "NONE"),
      validity = c("NONE", "NONE")
    )
  )
  res_filters <- .schema_filters(dr$SchemaData$schema())

  expect_equal(res_filters, trg_filters)

})

# NB: methods for key expiration management is tested with TileDBDriver;
# Here, we test again those we expose to storr.

test_that("keys_with_expiration", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)

  keys <- c("a", "b", "c", "d")
  t0 <- Sys.time()
  expires_at <- c(t0, t0, as.POSIXct("2250-05-28"), as.POSIXct(NA))
  sto$mset(keys, 1:4, namespace = c("ns1", "ns2", "ns3", "ns4"), expires_at = expires_at)

  # keys with expiration
  expect_no_error(arrw <- sto$keys_with_expiration(NULL, datetimes = TRUE))
  expect_s3_class(arrw, c("data.table"))

  expect_equal(dim(arrw), c(3, 3))
  expect_equal(arrw$key, c("a", "b", "c"))
  expect_equal(colnames(arrw), c("namespace", "key", "expires_at"))

  # Without 'expires_at' column
  expect_no_error(arrw <- sto$keys_with_expiration(NULL, datetimes = FALSE))
  expect_s3_class(arrw, c("data.table"))

  expect_equal(dim(arrw), c(3, 2))
  expect_equal(arrw$key, c("a", "b", "c"))
  expect_equal(colnames(arrw), c("namespace", "key"))

})

test_that("expired_keys and has_expired_keys", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)

  keys <- c("a", "b", "c", "d")
  t0 <- Sys.time()
  expires_at <- c(t0, t0, as.POSIXct("2250-05-28"), as.POSIXct(NA))
  sto$mset(keys, 1:4, namespace = c("ns1", "ns2", "ns3", "ns4"), expires_at = expires_at)


  expect_no_error(arrw <- sto$expired_keys(NULL, datetimes = TRUE))
  expect_s3_class(arrw, c("data.table"))

  expect_equal(dim(arrw), c(2, 3))

  expect_equal(arrw$key, c("a", "b"))
  expect_equal(colnames(arrw), c("namespace", "key", "expires_at"))

  # Without 'expires_at' column
  expect_no_error(arrw <- sto$expired_keys(NULL, datetimes = FALSE))
  expect_s3_class(arrw, c("data.table"))

  expect_equal(dim(arrw), c(2, 2))
  expect_equal(arrw$key, c("a", "b"))
  expect_equal(colnames(arrw), c("namespace", "key"))

  # Test for expired keys
  expect_true(sto$has_expired_keys(NULL))
  expect_true(sto$has_expired_keys("ns1"))
  expect_false(sto$has_expired_keys("ns3"))
  expect_false(sto$has_expired_keys("ns4"))

})

test_that("clear_expired_keys", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)

  keys <- c("a", "b", "c", "d")
  t0 <- Sys.time()
  expires_at <- c(t0, t0, as.POSIXct("2250-05-28"), as.POSIXct(NA))
  sto$mset(keys, 1:4, namespace = c("ns1", "ns2", "ns3", "ns4"), expires_at = expires_at)

  expect_equal(numhash(sto$envir_metadata), 4)

  # Clear all expired keys
  expect_invisible(bool <- sto$clear_expired_keys(NULL))
  expect_true(bool)

  # Test for expired keys
  expect_false(sto$has_expired_keys(NULL))

  # Check cache - expect minus two keys
  expect_equal(numhash(sto$envir_metadata), 2)

  # ----------------------------------------------------------------------------
  # Lets redo it again..

  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)

  keys <- c("a", "b", "c", "d")
  t0 <- Sys.time()
  expires_at <- c(t0, t0, as.POSIXct("2250-05-28"), as.POSIXct(NA))
  sto$mset(keys, 1:4, namespace = c("ns1", "ns2", "ns3", "ns4"), expires_at = expires_at)

  # GC without clearing all expired keys
  expect_invisible(unused <- sto$gc())
  expect_equal(unused, character(0))

  # GC and clear all expired keys
  expect_invisible(unused <- sto$gc(clear_expired = TRUE))
  expect_length(unused, 2)

  # Test for expired keys
  expect_false(sto$has_expired_keys(NULL))

  # ----------------------------------------------------------------------------
  # Check that the expired key is correctly removed from cache

  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)

  keys <- c("a", "a2", "b", "c", "d")
  t0 <- Sys.time()
  expires_at <- c(t0, t0, t0,  as.POSIXct("2250-05-28"), as.POSIXct(NA))
  sto$mset(keys, 1:5, namespace = c("ns1", "ns1", "ns2", "ns3", "ns4"), expires_at = expires_at)

  expect_equal(numhash(sto$envir_metadata), 5)

  # Clear expired keys from "ns1"
  expect_invisible(bool <- sto$clear_expired_keys("ns1"))
  expect_true(bool)

  expect_error(sto$get_keymeta("a", "ns1"),
               "key 'a' ('ns1') not found",
               fixed = TRUE,
               class = "error")

  expect_error(sto$get_keymeta("aa", "ns1"),
               "key 'aa' ('ns1') not found",
               fixed = TRUE,
               class = "error")

  # Test for expired keys
  expect_true(sto$has_expired_keys(NULL))

  # Check cache - expect minus one key
  expect_equal(numhash(sto$envir_metadata), 3)

  # Clear expired keys from "ns2"
  expect_true(sto$clear_expired_keys("ns2"))

  expect_error(sto$get_keymeta("b", "ns2"),
               "key 'b' ('ns2') not found",
               fixed = TRUE,
               class = "error")

  expect_false(sto$has_expired_keys(NULL))

  expect_equal(numhash(sto$envir_metadata), 2)
})

test_that("is_key_expired", {
  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)

  keys <- c("a", "b", "c", "d")
  t0 <- Sys.time()
  expires_at <- c(t0, t0, as.POSIXct("2250-05-28"), as.POSIXct(NA))
  sto$mset(keys, 1:4, namespace = c("ns1", "ns2", "ns3", "ns4"), expires_at = expires_at)

  expect_true(sto$is_key_expired("a", "ns1", use_cache = FALSE))
  expect_true(sto$is_key_expired("b", "ns2", use_cache = FALSE))
  expect_false(sto$is_key_expired("c", "ns3", use_cache = FALSE))
  expect_false(sto$is_key_expired("d", "ns4", use_cache = FALSE))

  expect_true(sto$is_key_expired("a", "ns1", use_cache = TRUE))
  expect_true(sto$is_key_expired("b", "ns2", use_cache = TRUE))
  expect_false(sto$is_key_expired("c", "ns3", use_cache = TRUE))
  expect_false(sto$is_key_expired("d", "ns4", use_cache = TRUE))

  # check for not existent key
  expect_false(sto$is_key_expired("e", "ns4", use_cache = FALSE, check = FALSE))
  expect_false(sto$is_key_expired("e", "ns4", use_cache = TRUE, check = FALSE))

  expect_error(sto$is_key_expired("e", "ns4", use_cache = FALSE, check = TRUE),
               "key 'e' ('ns4') not found",
               fixed = TRUE,
               class = "error")

  expect_error(sto$is_key_expired("ee", "ns4", use_cache = TRUE, check = TRUE),
                                  "key 'ee' ('ns4') not found",
                                  fixed = TRUE,
                                  class = "error")
})

test_that("cache global option", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)

  sto$set("a", 1)

  withr::with_options(list(storr.tiledb.cache = FALSE), {
    sto$set("b", 2)
    res <- sto$get(c("a"))
    res <- sto$mget(c("a", "b"))
  })


  expect_equal(numhash(sto$envir), 1)
  expect_equal(numhash(sto$envir_metadata), 1)


  withr::with_options(list(storr.tiledb.cache = TRUE), {
    sto$set("b", 2)
    res <- sto$get(c("a"))
    res <- sto$mget(c("a", "b"))
  })

  expect_equal(numhash(sto$envir), 2)
  expect_equal(numhash(sto$envir_metadata), 2)

})


test_that("'list_unused_hashes'", {

  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)

  sto$mset(letters[1:3], 1:3)

  sto$del("a")

  expect_length(sto$list_hashes(), 3)
  expect_equal(sto$list_unused_hashes(), "9dc695ac953ca975b83c673f7144cffb")

  sto$gc()
  expect_length(sto$list_hashes(), 2)

  expect_equal(sto$list_unused_hashes(), character(0))

})


test_that("'get_all' and 'mget_all'", {
  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)

  sto$set("a",
          value = 1,
          expires_at = as.POSIXct("2026-02-25"),
          notes = "Yeah")

  sto$set("b", value = 2)


  # Check 'get_all'
  trg1 <- list(keyval = 2, keymeta = list(expires_at = structure(NA_real_, class = c("POSIXct",
                                                                                     "POSIXt"), tzone = ""), notes = NA_character_))
  expect_equal(sto$get_all("b"), trg1)
  expect_equal(sto$get_all("b", use_cache = FALSE), trg1)


  trg2 <- list(list(keyval = 1, keymeta = list(expires_at = structure(1771970400, class = c("POSIXct",
                                                                                            "POSIXt"), tzone = ""), notes = "Yeah")), list(keyval = 2, keymeta = list(
                                                                                              expires_at = structure(NA_real_, class = c("POSIXct", "POSIXt"
                                                                                              ), tzone = ""), notes = NA_character_)), NULL)
  expect_equal(sto$mget_all(c("a", "b", "c")), trg2)
  expect_equal(sto$mget_all(c("a", "b", "c"), use_cache = FALSE), trg2, ignore_attr = TRUE)

  expect_equal(sto$mget_all("nope"), list(NULL))
  expect_equal(sto$mget_all("nope", missing = "noval"), list(list(keyval = "noval", keymeta = "noval")))

})

