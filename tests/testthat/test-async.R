
test_that("set_async", {

  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE, async = TRUE)

  t0 <- Sys.time()
  expect_no_error(m1 <- sto$set_async("a", value = 1, expires_at = t0, notes = "async"))
  expect_no_error(m2 <- sto$set_async("b", value = 2, namespace = "ns2"))

  expect_named(m1, c("mirai", "hash"))
  expect_all_true(sapply(m1$mirai, mirai::is_mirai))

  # wait mirai elements to be resolved
  miall <- c(unclass(m1$mirai), unclass(m2$mirai))
  all_resolved <- all(!sapply(miall, mirai::unresolved))

  while (!all_resolved) {
    all_resolved <- all(!sapply(miall, mirai::unresolved))
  }

  # Sys.sleep(1)
  # expect_equal(sto$get("a"), 1)
  # expect_equal(sto$get("b", "ns2"), 2)

  # test cached values
  expect_equal(sto$mget(c("a", "b"), c("objects", "ns2")), list(1, 2))

  trg <- list(list(expires_at = t0, notes = "async"),
              list(expires_at = as.POSIXct(NA),
  notes = NA_character_))

  expect_equal(sto$mget_keymeta(c("a", "b"), c("objects", "ns2")), trg)

  # test key/keymeta are saved on disk
  expect_equal(sto$mget(c("a", "b"), c("objects", "ns2"), use_cache = FALSE), list(1, 2))
  expect_equal(sto$mget_keymeta(c("a", "b"), c("objects", "ns2"), use_cache = FALSE),
               trg, ignore_attr = TRUE)

  # test inputs
  expect_error(sto$set_async(c("a", "b"), 1),
              "'key' must have 1 elements (recieved 2)",
              fixed = TRUE,
              class = "error")

  expect_error(sto$set_async("a", 1, namespace = c("a", "b")),
               "'namespace' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  expect_error(sto$set_async("a", 1, expires_at = "a"),
               "'expires_at' should be a date-time object, not character",
               fixed = TRUE,
               class = "error")

  expect_error(sto$set_async("a", 1, expires_at = rep(as.POSIXct(NA), 2)),
               "'expires_at' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  expect_error(sto$set_async("a", 1, notes = 1),
               "'notes' should be a character string, not numeric",
               fixed = TRUE,
               class = "error")

  expect_error(sto$set_async("a", 1, notes = c("a", "b")),
               "'notes' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  })

test_that("mset_async", {

  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE, async = TRUE)

  t0 <- Sys.time()
  expect_no_error(m1 <- sto$mset_async(c("a", "b"), value = c(1, 2), namespace = c("ns1", "ns2"),
                                       expires_at = c(t0, t0), notes = c("async1", "async2")))

  expect_named(m1, c("mirai", "hash"))
  expect_all_true(sapply(m1$mirai, mirai::is_mirai))

  # wait mirai elements to be resolved
  miall <- unclass(m1$mirai)
  all_resolved <- all(!sapply(miall, mirai::unresolved))

  while (!all_resolved) {
    all_resolved <- all(!sapply(miall, mirai::unresolved))
  }


  # test cached values
  expect_equal(sto$mget(c("a", "b"), c("ns1", "ns2")), list(1, 2))

  trg <- list(list(expires_at = t0, notes = "async1"),
              list(expires_at = t0, notes = "async2"))

  expect_equal(sto$mget_keymeta(c("a", "b"), c("ns1", "ns2")), trg)

  # test key/keymeta are saved on disk
  expect_equal(sto$mget(c("a", "b"), c("ns1", "ns2"), use_cache = FALSE), list(1, 2))
  expect_equal(sto$mget_keymeta(c("a", "b"), c("ns1", "ns2"), use_cache = FALSE),
               trg, ignore_attr = TRUE)

  # test inputs
  expect_error(sto$mset_async(c("a", "b"), namespace = c("ns1", "ns2", "ns3")),
               "Incompatible lengths for key and namespace",
               fixed = TRUE,
               class = "error")

  expect_error(sto$mset_async("a", 1, notes = c("a", "b")),
               "'notes' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  expect_error(sto$mset_async("a", 1, expires_at = c("a", "b")),
               "'expires_at' should be a date-time object, not character",
               fixed = TRUE,
               class = "error")

  # expect_error(sto$set_async("a", 1, notes = 1),
  #              "'notes' should be a character string, not numeric",
  #              fixed = TRUE,
  #              class = "error")
  #
  # expect_error(sto$set_async("a", 1, notes = c("a", "b")),
  #              "'notes' must have 1 elements (recieved 2)",
  #              fixed = TRUE,
  #              class = "error")

})
