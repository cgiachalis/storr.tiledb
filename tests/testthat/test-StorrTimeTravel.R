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
  expect_equal(stott$list("ns2"), "b")
  expect_equal(stott$list_hashes(), hashes)
  expect_equal(stott$list_namespaces(), c("ns1", "ns2"))

})

test_that("'get_keymeta'/'mget_keymeta' and friends with time-travel", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, default_namespace = "ns1")

  t0 <- Sys.time()
  sto$set("a", 1, notes = "a1")
  t1 <- Sys.time()
  sto$set_keymeta("a", notes = "a2", expires_at = as.POSIXct(t1))
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
