test_that("'TileDBDriver'", {

  uri <- file.path(withr::local_tempdir(), "test-driver")
  expect_no_error(dr <- TileDBDriver$new(uri))

  expect_s3_class(dr, "TileDBDriver")
  expect_false(dr$exists())
  expect_equal(dr$mode, "CLOSED")
  expect_equal(dr$type(), "tiledb")

  # public fields
  expect_false(dr$binary)
  expect_equal(dr$traits, list(accept = "string",
                               throw_missing = TRUE))

  # public fields are locked
  expect_error(dr$binary <- TRUE)
  expect_error(dr$traits <- "boo")

})

test_that("m/get_keymeta", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)

  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)
  dr <- driver_tiledb(uri)

  # add some keys
  sto$set("x", 1)
  t0 <- Sys.time()+100
  sto$set("y", 1, expires_at = t0, note = "name:Bob")

  # expected outputs
  expval1 <- list(expires_at = as.POSIXct(NA),
                  notes = NA_character_)
  expval2 <- list(expires_at = t0,
                  notes = "name:Bob")

  expval3 <- list(expval1, expval2)
  attr(expval3, "missing") <- integer(0)


  # test standard cases
  expect_equal(dr$get_keymeta("x", "objects"), expval1)
  expect_equal(dr$get_keymeta("y", "objects"), expval2)

  # test multiple keys
  expect_no_error(mget_result <- dr$mget_keymeta(c("x", "y"),
                                                 rep("objects", 2)))
  expect_equal(mget_result, expval3, ignore_attr = TRUE)

  # "y:other" not found
  expval4 <- list(expval1, list(NULL))
  attr(expval4, "missing") <- 2

  expect_no_error(mget_result2 <- dr$mget_keymeta(c("x", "y"),
                                                 c("objects", "other")))
  expect_equal(mget_result2, expval4, ignore_attr = TRUE)

  # "x:other" not found with nomatch
  expval5 <- list(expval2, "hey, is missing")
  attr(expval5, "missing") <- 2
  expect_no_error(mget_result3 <- dr$mget_keymeta(c("y", "x"),
                                                  c("objects", "other"),
                                                  nomatch = "hey, is missing"))
  expect_equal(mget_result3, expval5, ignore_attr = TRUE)

  })

test_that("set_keymeta", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)
  dr <- driver_tiledb(uri)

  # add some keys
  sto$set("x", 1)
  t0 <- Sys.time()+100
  sto$set("y", 1,namespace = "obj2", expires_at = t0, note = "name:Bob")

  # test standard cases
  expect_true(dr$set_keymeta("x", "objects", expires_at = as.POSIXct(1), notes = "simple"))
  expect_equal(dr$get_keymeta("x", "objects"), list(expires_at = as.POSIXct(1, tz = NULL),
                                                   notes = "simple"))
  # update note only
  expect_true(dr$set_keymeta("x", "objects", expires_at = NULL, notes = "no simple"))

  # test is updated
  expect_equal(dr$get_keymeta("x", "objects"), list(expires_at = as.POSIXct(1, tz = NULL),
                                                    notes = "no simple"))

  # update datetime only
  expect_true(dr$set_keymeta("x", "objects", expires_at = as.POSIXct(NA), notes = NULL))
  # test is updated
  expect_equal(dr$get_keymeta("x", "objects"), list(expires_at = as.POSIXct(NA),
                                                    notes = "no simple"))

})

test_that("mset_keymeta", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)
  dr <- driver_tiledb(uri)

  # add some keys
  sto$set("x", 1)
  t0 <- Sys.time()+100
  sto$set("y", 1,namespace = "obj2", expires_at = t0, note = "name:Bob")


  expval0 <- list(expires_at = as.POSIXct(NA, tz = NULL),
                  notes = NA_character_)

  expval <- list(expval0, expval0)
  attr(expval, "missing") <- integer(0)

  # update (set) multiple keymeta
  expect_true(dr$mset_keymeta(c("x", "y"),
                              c("objects", "obj2"),
                              expires_at = c(as.POSIXct(NA), as.POSIXct(NA)),
                              notes = c(NA_character_, NA_character_)))


  # test for correctness
  expect_equal(dr$mget_keymeta(c("x", "y"), c("objects", "obj2")), expval)

  # now update only notes
  expect_true(dr$mset_keymeta(c("x", "y"),
                              c("objects", "obj2"),
                              expires_at = NULL,
                              notes = c("object", "obj2")))

  # test for correctness
  expval[[1]]$notes <- "object"
  expval[[2]]$notes <- "obj2"
  expect_equal(dr$mget_keymeta(c("x", "y"), c("objects", "obj2")), expval)


  # next update only datetimes
  expect_true(dr$mset_keymeta(c("x", "y"),
                              c("objects", "obj2"),
                              expires_at = c(as.POSIXct(1), as.POSIXct(1)),
                              notes = NULL))

  # test for correctness
  expval[[1]]$expires_at <- as.POSIXct(1, tz = NULL)
  expval[[2]]$expires_at  <- as.POSIXct(1, tz = NULL)
  expect_equal(dr$mget_keymeta(c("x", "y"), c("objects", "obj2")), expval)

  # test for errors
  expect_error(dr$mset_keymeta(c("x", "y"), c("objects", "obj22"),
                              expires_at = c(as.POSIXct(1), as.POSIXct(1)),
                              notes = NULL),
               "key 'y' ('obj22') not found",
               fixed = TRUE,
               class = "KeyError")

  expect_error(dr$mset_keymeta(c("z", "y"),
                               c("objects", "obj22"),
                               expires_at = c(as.POSIXct(1), as.POSIXct(1)),
                               notes = NULL),
               "key 'z,y' ('objects,obj22') not found",
               fixed = TRUE,
               class = "KeyError")
  })

test_that("keys_with_expiration", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)
  dr <- driver_tiledb(uri)

  keys <- c("a", "b", "c", "d")
  t0 <- Sys.time()
  expires_at <- c(t0, t0, as.POSIXct("2250-05-28"), as.POSIXct(NA))
  sto$mset(keys, 1:4, namespace = c("ns1", "ns2", "ns3", "ns4"), expires_at = expires_at)

  # keys with expiration
  expect_error(dr$keys_with_expiration(1),
               "`namespace` should be a character vector or NULL.",
               fixed = TRUE,
               class = "error")
  expect_no_error(arrw <- dr$keys_with_expiration(NULL, datetimes = TRUE))
  expect_s3_class(arrw, c("Table", "ArrowTabular", "ArrowObject", "R6"), exact = TRUE)

  expect_equal(arrw$num_rows, 3)
  expect_equal(arrw$num_columns, 3)
  expect_equal(arrw$GetColumnByName("key")$as_vector(), c("a", "b", "c"))
  expect_equal(arrw$ColumnNames(), c("namespace", "key", "expires_at"))

  # Without 'expires_at' column
  expect_no_error(arrw <- dr$keys_with_expiration(NULL, datetimes = FALSE))
  expect_s3_class(arrw, c("Table", "ArrowTabular", "ArrowObject", "R6"), exact = TRUE)

  expect_equal(arrw$num_rows, 3)
  expect_equal(arrw$num_columns, 2)
  expect_equal(arrw$GetColumnByName("key")$as_vector(), c("a", "b", "c"))
  expect_equal(arrw$ColumnNames(), c("namespace", "key"))

})

test_that("keys_without_expiration", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)
  dr <- driver_tiledb(uri)

  keys <- c("a", "b", "c", "d")
  t0 <- Sys.time()
  expires_at <- c(t0, t0, as.POSIXct("2250-05-28"), as.POSIXct(NA))
  sto$mset(keys, 1:4, namespace = c("ns1", "ns2", "ns3", "ns4"), expires_at = expires_at)

  # keys with expiration
  expect_error(dr$keys_without_expiration(1),
               "`namespace` should be a character vector or NULL.",
               fixed = TRUE,
               class = "error")
  expect_no_error(arrw <- dr$keys_without_expiration(NULL, datetimes = TRUE))
  expect_s3_class(arrw, c("Table", "ArrowTabular", "ArrowObject", "R6"), exact = TRUE)

  expect_equal(arrw$num_rows, 1)
  expect_equal(arrw$num_columns, 3)
  expect_equal(arrw$GetColumnByName("key")$as_vector(), c("d"))
  expect_equal(arrw$ColumnNames(), c("namespace", "key", "expires_at"))

  # Without 'expires_at' column
  expect_no_error(arrw <- dr$keys_without_expiration(NULL, datetimes = FALSE))
  expect_s3_class(arrw, c("Table", "ArrowTabular", "ArrowObject", "R6"), exact = TRUE)

  expect_equal(arrw$num_rows, 1)
  expect_equal(arrw$num_columns, 2)
  expect_equal(arrw$GetColumnByName("key")$as_vector(), c("d"))
  expect_equal(arrw$ColumnNames(), c("namespace", "key"))

})

test_that("expired_keys and friends", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)
  dr <- driver_tiledb(uri)

  keys <- c("a", "b", "c", "d")
  t0 <- Sys.time()
  expires_at <- c(t0, t0, as.POSIXct("2250-05-28"), as.POSIXct(NA))
  sto$mset(keys, 1:4, namespace = c("ns1", "ns2", "ns3", "ns4"), expires_at = expires_at)


  expect_error(dr$expired_keys(1),
               "`namespace` should be a character vector or NULL.",
               fixed = TRUE,
               class = "error")
  expect_no_error(arrw <- dr$expired_keys(NULL, datetimes = TRUE))
  expect_s3_class(arrw, c("Table", "ArrowTabular", "ArrowObject", "R6"), exact = TRUE)

  expect_equal(arrw$num_rows, 2)
  expect_equal(arrw$num_columns, 3)
  expect_equal(arrw$GetColumnByName("key")$as_vector(), c("a", "b"))
  expect_equal(arrw$ColumnNames(), c("namespace", "key", "expires_at"))

  # Without 'expires_at' column
  expect_no_error(arrw <- dr$expired_keys(NULL, datetimes = FALSE))
  expect_s3_class(arrw, c("Table", "ArrowTabular", "ArrowObject", "R6"), exact = TRUE)

  expect_equal(arrw$num_rows, 2)
  expect_equal(arrw$num_columns, 2)
  expect_equal(arrw$GetColumnByName("key")$as_vector(), c("a", "b"))
  expect_equal(arrw$ColumnNames(), c("namespace", "key"))

  # Test number of expired keys
  expect_equal(dr$num_expired_keys(NULL), 2)
  expect_equal(dr$num_expired_keys("ns2"), 1)
  expect_equal(dr$num_expired_keys("ns3"), 0)

  # Test for expired keys
  expect_true(dr$has_expired_keys(NULL))
  expect_true(dr$has_expired_keys("ns1"))
  expect_false(dr$has_expired_keys("ns3"))
  expect_false(dr$has_expired_keys("ns4"))

})

test_that("unexpired_keys and friends", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)
  dr <- driver_tiledb(uri)

  keys <- c("a", "b", "c", "d")
  t0 <- Sys.time()
  expires_at <- c(t0, t0, as.POSIXct("2250-05-28"), as.POSIXct(NA))
  sto$mset(keys, 1:4, namespace = c("ns1", "ns2", "ns3", "ns4"), expires_at = expires_at)

  expect_error(dr$unexpired_keys(1),
               "`namespace` should be a character vector or NULL.",
               fixed = TRUE,
               class = "error")
  expect_no_error(arrw <- dr$unexpired_keys(NULL, datetimes = TRUE))
  expect_s3_class(arrw, c("Table", "ArrowTabular", "ArrowObject", "R6"), exact = TRUE)

  expect_equal(arrw$num_rows, 1)
  expect_equal(arrw$num_columns, 3)
  expect_equal(arrw$GetColumnByName("key")$as_vector(), c("c"))
  expect_equal(arrw$ColumnNames(), c("namespace", "key", "expires_at"))

  # Without 'expires_at' column
  expect_no_error(arrw <- dr$unexpired_keys(NULL, datetimes = FALSE))
  expect_s3_class(arrw, c("Table", "ArrowTabular", "ArrowObject", "R6"), exact = TRUE)

  expect_equal(arrw$num_rows, 1)
  expect_equal(arrw$num_columns, 2)
  expect_equal(arrw$GetColumnByName("key")$as_vector(), c("c"))
  expect_equal(arrw$ColumnNames(), c("namespace", "key"))

  # Test number of unexpired keys
  expect_equal(dr$num_unexpired_keys(NULL), 1)
  expect_equal(dr$num_unexpired_keys("ns2"), 0)
  expect_equal(dr$num_unexpired_keys("ns3"), 1)

  # Test for unexpired keys
  expect_true(dr$has_unexpired_keys(NULL))
  expect_false(dr$has_unexpired_keys("ns1"))
  expect_true(dr$has_unexpired_keys("ns3"))
  expect_false(dr$has_unexpired_keys("ns4"))

})

test_that("delete_expired_keys", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)
  dr <- driver_tiledb(uri)

  keys <- c("a", "b", "c", "d")
  t0 <- Sys.time()
  expires_at <- c(t0, t0, as.POSIXct("2250-05-28"), as.POSIXct(NA))
  sto$mset(keys, 1:4, namespace = c("ns1", "ns2", "ns3", "ns4"), expires_at = expires_at)

  # Clear all expired keys
  expect_invisible(bool <- dr$delete_expired_keys(NULL))
  expect_true(bool)

  # Test for expired keys
  expect_false(dr$has_expired_keys(NULL))

  # ----------------------------------------------------------------------------
  # Lets redo it again..

  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)
  dr <- driver_tiledb(uri)

  keys <- c("a", "b", "c", "d")
  t0 <- Sys.time()
  expires_at <- c(t0, t0, as.POSIXct("2250-05-28"), as.POSIXct(NA))
  sto$mset(keys, 1:4, namespace = c("ns1", "ns2", "ns3", "ns4"), expires_at = expires_at)

  # Check storr for expired keys
  expect_true(dr$has_expired_keys(NULL))
  expect_equal(dr$num_expired_keys(NULL), 2)

  # Clear expired keys for specific namespace
  expect_invisible(bool <- dr$delete_expired_keys("ns1"))
  expect_true(bool)

  # Test that storr has left with one expired key
  expect_true(dr$has_expired_keys(NULL))
  expect_equal(dr$num_expired_keys(NULL), 1)

  # Clear all expired keys
  expect_invisible(bool <- dr$delete_expired_keys(NULL))
  expect_true(bool)

  # Test that storr has no expired keys
  expect_false(dr$has_expired_keys(NULL))
  expect_equal(dr$num_expired_keys(NULL), 0)

})

test_that("mset_object with dupes", {

  # Ensure mset_object removes duplicate coordinates
  uri <- file.path(withr::local_tempdir(), "test-driver")
  dr <- driver_tiledb(uri, init = TRUE, keep_open = FALSE)
  dr$open(instantiate = TRUE)

  hash_in <- "0012"
  expect_no_error(dr$mset_object(rep(hash_in, 3) , rep("xxxx", 3)))

  hash <- dr$members$tbl_data$object$object[]$hash

  expect_equal(hash, hash_in)

})

test_that("mget_object", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  st <- storr_tiledb(uri, init = TRUE)
  dr <- driver_tiledb(uri)

  st$set("a", 1, use_cache = FALSE)
  st$set("a", 1, namespace = "ns1", use_cache = F)
  st$set("b", 3, namespace = "ns1", use_cache = F)
  st$set("d", 4, namespace = "ns1", use_cache = F)

  hashes <- st$index_export()[, 1:3][["hash"]]

  expect_equal(dr$mget_object(hashes), list(1, 1, 3, 4))
  expect_equal(dr$mget_object(hashes[c(4,3,2)]), list(4, 3, 1))

})


test_that("export_tdb - identical hash algo", {

  # Temp URIs
  uri <- file.path(withr::local_tempdir(), "test-storr")
  uri_dest <- file.path(withr::local_tempdir(), "test-storr_dest")

  # Create storr source
  st <- storr_tiledb(uri, init = TRUE, default_namespace = "ns0")

  # Create storr destination
  driver_tiledb_create(uri_dest, hash_algorithm = "md5")

  st$set("a", 1)
  st$set("a", 1, namespace = "ns1")
  st$set("b", 3, namespace = "ns1")
  st$set("c", 2, namespace = "ns2")

  expect_error(st$export_tdb(uri_dest = uri),
               "Destination URI can not be the same as source.",
               class = "error",
               fixed = TRUE)

  # Export all to destination
  expect_no_error(out <- st$export_tdb(uri_dest = uri_dest, namespace = NULL))
  expect_true(out)

  # Test export was successful
  std <- storr_tiledb(uri_dest)
  expect_equal(std$mget(c("a", "a", "b", "c"),
                        c("ns0", "ns1", "ns1", "ns2"), use_cache = F),
               list(1, 1, 3, 2))

  # Test for identical hashes
  expect_equal(st$list_hashes(), std$list_hashes())

  ##
  # Create new storr destination ---
  ##
  uri_dest <- file.path(withr::local_tempdir(), "test-storr_dest2")
  driver_tiledb_create(uri_dest, hash_algorithm = "md5")

  # Export namespace "ns1" to destination
  expect_no_error(st$export_tdb(uri_dest = uri_dest, namespace = "ns1"))

  # Test they're copied over
  std <- storr_tiledb(uri_dest)
  expect_equal(std$mget(c("a", "b"), "ns1", use_cache = F), list(1, 3))

  # Export a key from namespace "ns0" to destination
  expect_no_error(st$export_tdb(key = c("a"), namespace = "ns0", uri_dest = uri_dest))
  # Test it is exported as expected
  expect_equal(std$get(c("a"), "ns0", use_cache = F), 1)
  # Test that dest has 2 hashes
  expect_length(std$list_hashes(), 2)
  # Need to test that process didn't write the object as it exists in 'tbl_data'
  sfo <- storr_fragments(uri_dest)
  expect_equal(sfo$fob_data$frag_num(), 1)
  # But it always writes the index even if data exists
  expect_equal(sfo$fob_keys$frag_num(), 2)

  # Write new namespace to source
  st$mset(c("aa", "bb"), list("aa", 200), namespace = "ns4")

  # Export namespace "ns4" to destination
  st$export_tdb(uri_dest = uri_dest, namespace = "ns4")
  expect_equal(std$mget(c("aa", "bb"), "ns4", use_cache = F), list("aa", 200))

})


test_that("export_tdb - different hash algo", {

  # Temp URIs
  uri <- file.path(withr::local_tempdir(), "test-storr")
  uri_dest <- file.path(withr::local_tempdir(), "test-storr_dest")

  # Create storr source
  st <- storr_tiledb(uri, init = TRUE, default_namespace = "ns0")

  # Create storr destination with different hash algo
  driver_tiledb_create(uri_dest, hash_algorithm = "sha256")

  st$set("a", 1)
  st$set("a", 1, namespace = "ns1")
  st$set("b", 3, namespace = "ns1")
  st$set("c", 2, namespace = "ns2")

  # Export all to destination
  expect_no_error(st$export_tdb(uri_dest = uri_dest, namespace = NULL))

  # Test export was successful
  std <- storr_tiledb(uri_dest)
  expect_equal(std$mget(c("a", "a", "b", "c"),
                        c("ns0", "ns1", "ns1", "ns2"), use_cache = F),
               list(1, 1, 3, 2))

  # Test no hash from src is in dest
  expect_disjoint(st$list_hashes(), std$list_hashes())

  ##
  # Create new storr destination ---
  ##
  uri_dest <- file.path(withr::local_tempdir(), "test-storr_dest2")
  driver_tiledb_create(uri_dest, hash_algorithm = "sha256")

  # Export namespace "ns1" to destination
  expect_no_error(st$export_tdb(uri_dest = uri_dest, namespace = "ns1"))

  # Test they're copied over
  std <- storr_tiledb(uri_dest)
  expect_equal(std$mget(c("a", "b"), "ns1", use_cache = F), list(1, 3))

  # Export a key from namespace "ns0" to destination
  expect_no_error(st$export_tdb(key = c("a"), namespace = "ns0", uri_dest = uri_dest))
  # Test it is exported as expected
  expect_equal(std$get(c("a"), "ns0", use_cache = F), 1)
  # Test that dest has 2 hashes
  expect_length(std$list_hashes(), 2)
  # Need to test that process didn't write the object as it exists in 'tbl_data'
  sfo <- storr_fragments(uri_dest)
  expect_equal(sfo$fob_data$frag_num(), 1)
  # But it always writes the index even if data exists
  expect_equal(sfo$fob_keys$frag_num(), 2)

  # Write new namespace to source
  st$mset(c("aa", "bb"), list("aa", 200), namespace = "ns4")

  # Export namespace "ns4" to destination
  st$export_tdb(uri_dest = uri_dest, namespace = "ns4")
  expect_equal(std$mget(c("aa", "bb"), "ns4", use_cache = F), list("aa", 200))
})

test_that("export_tdb - Nothing to export", {

  # Temp URIs
  uri <- file.path(withr::local_tempdir(), "test-storr")
  uri_dest <- file.path(withr::local_tempdir(), "test-storr_dest")

  # Create storr source
  st <- storr_tiledb(uri, init = TRUE, default_namespace = "ns0")

  # Create storr destination with different hash algo
  driver_tiledb_create(uri_dest, hash_algorithm = "sha256")

  expect_warning(st$export_tdb(uri_dest = uri_dest, namespace = NULL),
                  "Nothing to export for the selected key-namespace.")
})
