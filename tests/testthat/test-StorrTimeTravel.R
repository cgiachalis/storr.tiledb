test_that("'storr_timetravel()' and 'StorrTimeTravel'", {

  uri <- file.path(withr::local_tempdir(), "test-driver")
  dr <- TimeTravelDriver$new(uri)

  # R6Class: <TimeTravelDriver> object does not exist.
  expect_error(StorrTimeTravel$new(dr, "ns1"))

  # Set up 'storr'
  driver_tiledb_create(uri)
  dr <- TimeTravelDriver$new(uri)

  # 'StorrTimeTravel'
  expect_no_error(sto <- StorrTimeTravel$new(dr, "ns1"))
  expect_r6_class(sto, "StorrTimeTravel")
  expect_error(StorrTimeTravel$new("invalid-driver", "ns1"),
               "Not valid driver. Please use a 'TimeTravelDriver' object.",
               class = "error",
               fixed = TRUE)


  # Driver is open and members not instantiated, 'StorrTimeTravel' should
  # instantiate members
  StorrTimeTravelMock <- R6::R6Class(
    cloneable = FALSE,
    "Mocked",
    inherit = StorrTimeTravel,
    public = list(
      getdriver = function() {
        private$DRIVER
      }
    ),
  )

  sto2 <- StorrTimeTravelMock$new(dr, default_namespace = "objects")
  cl <- c("TimeTravelDriver", "TileDBGroup", "TileDBObject", "R6")
  expect_s3_class(sto2$getdriver(), cl, exact = TRUE)


  # driver is modified in place inside TileDBStorr,
  # because we instantiate members if needed
  expect_true(dr$is_open())
  expect_true(sto2$getdriver()$members_instantiated)
  expect_true(dr$members_instantiated)

  dr$reopen()
  expect_false(dr$members_instantiated)

  # case: driver is opened but members are not cached,
  # TileDBStorr will have to reopen and instantiate members
  sto3 <- StorrTimeTravelMock$new(dr, "objects")
  expect_s3_class(sto3$getdriver(), cl, exact = TRUE)

  expect_true(dr$is_open())
  expect_true(sto3$getdriver()$members_instantiated)
  expect_true(dr$members_instantiated)


  # 'storr_timetravel' wrapper
  expect_no_error(sto <- storr_timetravel(uri))
  expect_r6_class(sto, "StorrTimeTravel")

  # Test active field 'timestamp'
  expect_s3_class(sto$timestamp, "tiledb_timestamp")

  t1 <- Sys.time()
  expect_no_error(sto$timestamp <- t1)
  expect_equal(sto$timestamp$timestamp_end, t1, ignore_attr = TRUE)

  # Error: 'storr' didn't exist at 1970-01-01
  expect_error(sto$timestamp <- 0)

})

test_that("'get'/'mget' with time-travel", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, default_namespace = "ns1")

  t0 <- Sys.time()
  sto$set("a", 1)
  t1 <- Sys.time()
  sto$set("a", 2)
  sto$set("b", 3, namespace = "ns2")
  t2 <- Sys.time()

  hashes <- sto$list_hashes()

  # Open at t0 ---
  stott <- storr_timetravel(uri, timestamp = t0, default_namespace = "ns1")

  # Expect nothing at t0
  expect_error(stott$get("a"), class = "error", "key 'a' ('ns1') not found", fixed = TRUE)
  expect_equal(stott$mget(c("a", "b")), structure(list(NULL, NULL), missing = 1:2))

  expect_all_false(stott$exists(c("a", "b")))
  expect_all_false(stott$exists_object(hashes))

  expect_equal(stott$list(), character())
  expect_equal(stott$list_hashes(), character())
  expect_equal(stott$list_namespaces(), character())

  # Open at t1
  stott$timestamp <- t1
  expect_equal(stott$get("a"), 1)
  expect_equal(stott$mget(c("a", "b")), structure(list(1, NULL), missing = 2L))

  expect_equal(stott$exists(c("a", "b")), c(TRUE, FALSE))
  expect_equal(stott$list(), "a")
  expect_equal(stott$list_hashes(), "38e42db36c4414f7bbc19d750f71a721")
  expect_equal(stott$list_namespaces(), "ns1")

  # Open at t2
  stott$timestamp <- t2
  expect_equal(stott$get("a"), 2)
  expect_equal(stott$mget(c("a", "b"), namespace = c("ns1", "ns2")), list(2, 3))

  expect_all_true(stott$exists(c("a", "b"), namespace = c("ns1", "ns2")))
  expect_all_true(stott$exists_object(hashes))
  expect_equal(stott$list("ns2"), "b")
  expect_equal(stott$list_hashes(), hashes)
  expect_equal(stott$list_namespaces(), c("ns1", "ns2"))

})

test_that("'list_unsed_hashes' with time-travel", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, default_namespace = "ns1")

  t0 <- Sys.time()
  sto$mset(c("a", "b"), 1:2)
  t1 <- Sys.time()
  del_hash <- sto$get_hash("a")
  sto$del("a")
  sto$set("b", 3, namespace = "ns2")
  t2 <- Sys.time()

  sto$gc()
  t3 <- Sys.time()

  hashes <- sto$list_hashes()

  # Open at t0 ---
  stott <- storr_timetravel(uri, timestamp = t0, default_namespace = "ns1")

  # Expect nothing at t0
  expect_equal(stott$list_hashes(), character())

  # Open at t1
  stott$timestamp <- t1
  expect_equal(stott$mget(c("a", "b")), list(1, 2))
  expect_length(stott$list_hashes(), 2)

  # Open at t2
  stott$timestamp <- t2
  expect_equal(stott$list_unused_hashes(),  del_hash)
  expect_length(stott$list_hashes(), 3)

  stott$timestamp <- t3
  expect_equal(stott$list_unused_hashes(),  character(0))
  expect_length(stott$list_hashes(), 2)

})

test_that("'get_keymeta'/'mget_keymeta' and friends with time-travel", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, default_namespace = "ns1")

  t0 <- Sys.time()
  sto$set("a", 1, notes = "a1")
  t1 <- Sys.time()
  sto$update_keymeta("a", notes = "a2", expires_at = as.POSIXct(t1))
  sto$set("b", 3, namespace = "ns2", notes = "b3")
  t2 <- Sys.time()

  hashes <- sto$list_hashes()

  # Open at t0 ---
  stott <- storr_timetravel(uri, timestamp = t0, default_namespace = "ns1")

  # Expect nothing at t0
  expect_error(stott$get_keymeta("a"), class = "error", "key 'a' ('ns1') not found", fixed = TRUE)
  expect_equal(stott$mget_keymeta(c("a", "b")), structure(list(list(NULL), list(NULL)), missing = 1:2))

  expect_all_false(stott$exists(c("a", "b"),  namespace = c("ns1", "ns2")))

  df_trg <- structure(list(namespace = character(0), key = character(0),
                           expires_at = structure(numeric(0), class = c("POSIXct", "POSIXt"
                           ))), row.names = integer(0), class = c("data.table", "data.frame"
                           ))
  expect_equal(stott$keys_with_expiration(), df_trg)
  expect_equal(stott$expired_keys(), df_trg)
  expect_false(stott$has_expired_keys())
  expect_error(stott$is_key_expired("a", "ns1"),
               "key 'a' ('ns1') not found",
               fixed = TRUE,
               class = "error"
               )
  expect_false(stott$is_key_expired("a", "ns1", check = FALSE))

  # Open at t1
  stott$timestamp <- t1
  expect_equal(stott$get_keymeta("a"), list(expires_at = as.POSIXct(NA), notes = "a1"))
  expect_equal(stott$mget_keymeta(c("a", "b")), structure(list(
    list(
      expires_at = structure(
        NA_real_,
        class = c("POSIXct", "POSIXt"),
        tzone = ""
      ),
      notes = "a1"
    ), list(NULL)
  ), missing = 2L))

  expect_equal(stott$exists(c("a", "b"),  namespace = c("ns1", "ns2")), c(TRUE, FALSE))

  expect_equal(stott$keys_with_expiration(), df_trg)
  expect_equal(stott$expired_keys(), df_trg)
  expect_false(stott$has_expired_keys())
  expect_false(stott$is_key_expired("a", "ns1"))

  # Open at t2
  stott$timestamp <- t2
  expect_equal(stott$get_keymeta("a"), list(expires_at = as.POSIXct(t1), notes = "a2"))
  expect_equal(stott$get_keymeta("b", namespace = "ns2"), list(expires_at = as.POSIXct(NA), notes = "b3"))
  expect_equal(stott$mget_keymeta(c("a", "b"), namespace = c("ns1", "ns2")), list(list(expires_at = as.POSIXct(t1), notes = "a2"),
                                                     list(expires_at = as.POSIXct(NA), notes = "b3")), ignore_attr = TRUE)

  expect_all_true(stott$exists(c("a", "b"), namespace = c("ns1", "ns2")))

  df <- data.frame(namespace = "ns1", key = "a", expires_at = t1)
  df_trg <- data.table::as.data.table(df)

  expect_equal(stott$keys_with_expiration(), df_trg)
  expect_equal(stott$expired_keys(), df_trg)
  expect_true(stott$has_expired_keys())
  expect_true(stott$is_key_expired("a", "ns1"))
})

test_that("'index_export' with time-travel", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, default_namespace = "ns1")

  t0 <- Sys.time()
  sto$set("a", 1)
  t1 <- Sys.time()
  sto$set("a", 2)
  sto$set("b", 3, namespace = "ns2")
  t2 <- Sys.time()

  hashes <- sto$list_hashes()

  # Open at t0 ---
  stott <- storr_timetravel(uri, timestamp = t0, default_namespace = "ns1")

  df_trg <- structure(list(namespace = character(0),
                           key = character(0),
                           hash = character(0),
                           expires_at = structure(numeric(0), class = c("POSIXct", "POSIXt"
                           )),
                           notes = character(0)), row.names = integer(0), class = c("data.table", "data.frame"
                           ))
  # Expect nothing at t0
  expect_equal(stott$index_export(), df_trg, ignore_attr = TRUE)

  # Open at t1
  stott$timestamp <- t1
  expect_equal(nrow(res1 <- stott$index_export()), 1)
  expect_equal(res1$hash, "38e42db36c4414f7bbc19d750f71a721")

  # Open at t2
  stott$timestamp <- t2
  expect_equal(nrow(res1 <- stott$index_export()), 2)
  expect_equal(res1$hash, c("87494137ffd66807c0c5c877856799cc", "02c87a685a6264c39c65c94a51de14b8"
  ))
  expect_equal(res1$key, c("a", "b"))

})


test_that("'export' with time-travel", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, default_namespace = "ns1")
  dr <- driver_tiledb(uri); sto1 <- storr::storr(dr)
  t0 <- Sys.time()
  sto$set("a", 1)
  t1 <- Sys.time()
  sto$set("a", 2)
  sto$set("b", 3, namespace = "ns2")
  t2 <- Sys.time()

  # Open at t0 ---
  stott <- storr_timetravel(uri, timestamp = t0, default_namespace = "ns1")

  # Expect nothing at t0
  expect_no_error(dest_t0 <- stott$export(list()))

  # Open at t1
  stott$timestamp <- t1
  expect_no_error(dest_t1 <- stott$export(list()))
  expect_named(dest_t1, "a")
  expect_equal(dest_t1$a, 1)

  # Open at t2
  stott$timestamp <- t2
  expect_no_error(dest_t2 <- stott$export(list()))
  expect_error(stott$export(list(),  namespace = NULL),
               "If exporting multiple namespaces, both dest and src must be storrs",
               class = "error", fixed = TRUE)

  expect_named(dest_t2, "a")
  expect_equal(dest_t2$a, 2)

  dest_t2 <- stott$export(list(), namespace = "ns2")
  expect_named(dest_t2, "b")
  expect_equal(dest_t2$b, 3)

})


test_that("'export_tdb' with time-travel", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, default_namespace = "ns1")

  uri2 <- file.path(withr::local_tempdir(), "test-storr2")
  sto2 <- storr_tiledb(uri2, init = TRUE, default_namespace = "ns1")

  t0 <- Sys.time()
  sto$set("a", 1)
  t1 <- Sys.time()
  sto$set("a", 2)
  sto$set("b", 3, namespace = "ns2")
  t2 <- Sys.time()

  # Open at t0 ---
  stott <- storr_timetravel(uri, timestamp = t0, default_namespace = "ns1")

  # Expect nothing at t0
  expect_warning(stott$export_tdb(uri_dest = uri2), class = "warning",
                 "Nothing to export for the selected key-namespace.")

  expect_error(stott$export_tdb(uri_dest = uri),
               "Destination URI can not be the same as source.",
               class = "error",
               fixed = TRUE)

  # Open at t1
  stott$timestamp <- t1
  expect_no_error(stott$export_tdb(uri_dest = uri2))
  expect_equal(sto2$get("a"), 1)
  expect_equal(sto2$list(namespace = "ns1"), "a")

  # Open at t2
  stott$timestamp <- t2
  expect_no_error(stott$export_tdb(uri_dest = uri2, namespace = NULL)) # all namespaces
  expect_equal(sto2$get("a"), 2)
  expect_equal(sto2$mget(c("a", "b"), namespace = c("ns1", "ns2")), list(2, 3))
  expect_equal(sto2$list_namespaces(), c("ns1", "ns2"))

})

test_that("'export_tdb' with time-travel (diff hash)", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, default_namespace = "ns1", hash_algorithm = "sha1")

  uri2 <- file.path(withr::local_tempdir(), "test-storr2")
  sto2 <- storr_tiledb(uri2, init = TRUE, default_namespace = "ns1")

  t0 <- Sys.time()
  sto$set("a", 1)
  t1 <- Sys.time()
  sto$set("a", 2)
  sto$set("b", 3, namespace = "ns2")
  t2 <- Sys.time()

  # Open at t0 ---
  stott <- storr_timetravel(uri, timestamp = t0, default_namespace = "ns1")

  # Expect nothing at t0
  expect_warning(stott$export_tdb(uri_dest = uri2), class = "warning",
                 "Nothing to export for the selected key-namespace.")

  # Open at t1
  stott$timestamp <- t1
  expect_no_error(stott$export_tdb(uri_dest = uri2))
  expect_equal(sto2$get("a"), 1)
  expect_equal(sto2$list(namespace = "ns1"), "a")

  # Open at t2
  stott$timestamp <- t2
  expect_no_error(stott$export_tdb(uri_dest = uri2, namespace = NULL)) # all namespaces
  expect_equal(sto2$get("a"), 2)
  expect_equal(sto2$mget(c("a", "b"), namespace = c("ns1", "ns2")), list(2, 3))
  expect_equal(sto2$list_namespaces(), c("ns1", "ns2"))

})


test_that("'export_tdb' with time-travel (diff serial_format)", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, default_namespace = "ns1", serial_format = "qs2")

  uri2 <- file.path(withr::local_tempdir(), "test-storr2")
  sto2 <- storr_tiledb(uri2, init = TRUE, default_namespace = "ns1")

  t0 <- Sys.time()
  sto$set("a", 1)
  t1 <- Sys.time()
  sto$set("a", 2)
  sto$set("b", 3, namespace = "ns2")
  t2 <- Sys.time()

  # Open at t0 ---
  stott <- storr_timetravel(uri, timestamp = t0, default_namespace = "ns1")

  # Expect nothing at t0
  expect_warning(stott$export_tdb(uri_dest = uri2), class = "warning",
                 "Nothing to export for the selected key-namespace.")

  # Open at t1
  stott$timestamp <- t1
  expect_no_error(stott$export_tdb(uri_dest = uri2))
  expect_equal(sto2$get("a"), 1)
  expect_equal(sto2$list(namespace = "ns1"), "a")

  # Open at t2
  stott$timestamp <- t2
  expect_no_error(stott$export_tdb(uri_dest = uri2, namespace = NULL)) # all namespaces
  expect_equal(sto2$get("a"), 2)
  expect_equal(sto2$mget(c("a", "b"), namespace = c("ns1", "ns2")), list(2, 3))
  expect_equal(sto2$list_namespaces(), c("ns1", "ns2"))

})


test_that("'get_all' and 'mget_all' with time-travel", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, default_namespace = "ns1")

  t0 <- Sys.time()
  sto$set("a", 1)
  t1 <- Sys.time()
  sto$set("a", 2, notes = "Good")
  sto$set("b", 3)
  t2 <- Sys.time()

  # Open at t1
  stott <- storr_timetravel(uri, timestamp = t1, default_namespace = "ns1")

  # Check 'get_all'
  trg1 <- list(keyval = 1, keymeta = list(expires_at = structure(NA_real_, class = c("POSIXct",
                                                                                     "POSIXt"), tzone = ""), notes = NA_character_))
  expect_equal(stott$get_all("a"), trg1)
  expect_error(stott$get_all("b"), class = "error", "key 'b' ('ns1') not found", fixed = TRUE)

  # Open at t2
  stott$timestamp <- t2
  trg2 <- list(list(keyval = 2, keymeta = list(expires_at = structure(NA_real_, class = c("POSIXct",
                                                                                          "POSIXt"), tzone = ""), notes = "Good")), list(keyval = 3, keymeta = list(
                                                                                            expires_at = structure(NA_real_, class = c("POSIXct", "POSIXt"
                                                                                            ), tzone = ""), notes = NA_character_)), NULL)
  expect_equal(stott$mget_all(c("a", "b", "c")), trg2, ignore_attr = TRUE)

  expect_equal(stott$mget_all("nope"), list(NULL))
  expect_equal(stott$mget_all("nope", missing = "noval"), list(list(keyval = "noval", keymeta = "noval")))

})


test_that("'get_keymeta_expires_at/notes' and 'mget_keymeta_expires_at/notes' with time-travel", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, default_namespace = "ns1")

  t0 <- Sys.time()
  sto$set("a", 1, notes = "a1")
  t1 <- Sys.time()
  sto$update_keymeta("a", notes = "a2", expires_at = as.POSIXct(t1))
  sto$set("b", 3, namespace = "ns2", notes = "b3")
  t2 <- Sys.time()

  # Open at t0 ---
  stott <- storr_timetravel(uri, timestamp = t0, default_namespace = "ns1")

  # Expect nothing at t0
  expect_error(stott$get_keymeta_notes("a"), class = "error", "key 'a' ('ns1') not found", fixed = TRUE)
  expect_equal(stott$mget_keymeta_notes(c("a", "b")), structure(list(NULL, NULL), missing = 1:2))

  expect_all_false(stott$exists(c("a", "b"), namespace = c("ns1", "ns2")))

  # Open at t1
  stott$timestamp <- t1

  expect_equal(stott$get_keymeta_expires_at("a"), as.POSIXct(NA))
  expect_equal(stott$mget_keymeta_expires_at(c("a", "b")), structure(list(as.POSIXct(NA), NULL), missing = 2L))

  expect_equal(stott$exists(c("a", "b"),  namespace = c("ns1", "ns2")), c(TRUE, FALSE))

  # Open at t2
  stott$timestamp <- t2
  expect_equal(stott$get_keymeta_notes("a"), "a2")
  expect_equal(stott$get_keymeta_notes("b", namespace = "ns2"), "b3")

  trg <- structure(list(as.POSIXct(t1), as.POSIXct(NA) , NULL), missing = 3L)
  expect_equal(stott$mget_keymeta_expires_at(c("a", "b", "c"), namespace = c("ns1", "ns2", "ns")), trg,ignore_attr = TRUE)

  expect_all_true(stott$exists(c("a", "b"), namespace = c("ns1", "ns2")))

})


test_that("list_notes", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, default_namespace = "ns1")


  t0 <- Sys.time()
  sto$set("a", 1, notes = "note-a")
  t1 <- Sys.time()

  sto$set("b", 2, notes = "note-b")
  t2 <- Sys.time()


  # Open at t0 ---
  stott <- storr_timetravel(uri, timestamp = t0, default_namespace = "ns1")

  # Expect nothing at t0
  expect_equal(stott$list_notes("ns1"), character(0))

  # Open at t1
  stott$timestamp <- t1

  expect_equal(stott$list_notes("ns1"), "note-a")
  expect_equal(stott$list_notes("ns1", named = TRUE), c(a = "note-a"))

  # Open at t2
  stott$timestamp <- t2
  expect_equal(stott$list_notes("ns1"), c("note-a", "note-b"))
  expect_equal(stott$list_notes("ns1", named = TRUE), c(a = "note-a", b = "note-b"))

  expect_error(stott$list_notes(c("ns0", "ns1")),
                "`namespace` should be a single character string.",
                class = "error", fixed = TRUE)


})



test_that("keys_with_notes", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, default_namespace = "ns1")


  t0 <- Sys.time()
  sto$set("a", 1, notes = "note-a")
  t1 <- Sys.time()

  sto$set("b", 2, notes = "note-b")
  sto$set("c", 2)
  t2 <- Sys.time()


  # Open at t0 ---
  stott <- storr_timetravel(uri, timestamp = t0, default_namespace = "ns1")

  # Expect nothing at t0
  df_trg <- structure(list(namespace = character(0), key = character(0),
                           notes = character(0)), row.names = integer(0),
                           class = c("data.table", "data.frame"
                           ))
  expect_equal(stott$keys_with_notes("ns1"), df_trg)

  # Open at t1
  stott$timestamp <- t1

  expect_no_error(dt1 <- sto$keys_with_notes(NULL, notes = TRUE))
  expect_s3_class(dt1, c("data.table"))

  expect_equal(dim(dt1), c(2, 3))
  expect_equal(dt1$key, c("a", "b"))
  expect_equal(colnames(dt1), c("namespace", "key", "notes"))
  expect_equal(stott$list_notes("ns1", named = TRUE), c(a = "note-a"))

  # Without 'notes' field
  expect_no_error(dt1 <- sto$keys_with_notes(NULL, notes = FALSE))
  expect_s3_class(dt1, c("data.table"))
  expect_equal(dim(dt1), c(2, 2))
  expect_equal(dt1$key, c("a", "b"))
  expect_equal(colnames(dt1), c("namespace", "key"))

  expect_error(stott$list_notes(c("ns0", "ns1")),
               "`namespace` should be a single character string.",
               class = "error", fixed = TRUE)


})
