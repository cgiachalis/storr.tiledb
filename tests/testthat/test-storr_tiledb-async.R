
test_that("set_async", {

  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE, async = TRUE)

  t0 <- Sys.time()
  expect_no_error(m1 <- sto$set_async("a", value = 1, expires_at = t0, notes = "async"))
  expect_no_error(m2 <- sto$set_async("b", value = 2, namespace = "ns2"))

  expect_named(m1, c("mirai", "hash"))
  expect_all_true(sapply(m1$mirai, mirai::is_mirai))
  expect_equal(m1$hash, "632336c518ae1c89ecf26ae5fbec5860")

  # cached keymeta are available immediately
  trg <- list(list(expires_at = t0, notes = "async"),
              list(expires_at = as.POSIXct(NA),
                   notes = NA_character_))

  expect_equal(sto$mget_keymeta(c("a", "b"), c("objects", "ns2")), trg)

  # wait mirai elements to be resolved
  miall <- c(unclass(m1$mirai), unclass(m2$mirai))
  all_resolved <- all(!sapply(miall, mirai::unresolved))

  while (!all_resolved) {
    all_resolved <- all(!sapply(miall, mirai::unresolved))
  }

  # Sys.sleep(1)
  # expect_equal(sto$get("a"), 1)
  # expect_equal(sto$get("b", "ns2"), 2)

  # test cached values (note mget/get always goes on db to check if hash exists
  # so we wait async to complete)
  expect_equal(sto$mget(c("a", "b"), c("objects", "ns2")), list(1, 2))

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


  # test cached keymeta
  trg <- list(list(expires_at = t0, notes = "async1"),
              list(expires_at = t0, notes = "async2"))

  expect_equal(sto$mget_keymeta(c("a", "b"), c("ns1", "ns2")), trg)

  # wait mirai elements to be resolved
  miall <- unclass(m1$mirai)
  all_resolved <- all(!sapply(miall, mirai::unresolved))

  while (!all_resolved) {
    all_resolved <- all(!sapply(miall, mirai::unresolved))
  }


  # test cached values
  expect_equal(sto$mget(c("a", "b"), c("ns1", "ns2")), list(1, 2))

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


test_that("set_by_value_async", {

  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE, async = TRUE)

  t0 <- Sys.time()
  expect_no_error(m1 <- sto$set_by_value_async(value = 1, expires_at = t0, notes = "async"))
  expect_no_error(m2 <- sto$set_by_value_async(value = 2, namespace = "ns2"))

  expect_named(m1, c("mirai", "hash"))
  expect_all_true(sapply(m1$mirai, mirai::is_mirai))

  # cached keymeta are available immediately
  trg <- list(list(expires_at = t0, notes = "async"),
              list(expires_at = as.POSIXct(NA),
                   notes = NA_character_))

  h <- c(m1$hash, m2$hash)
  expect_equal(sto$mget_keymeta(h, c("objects", "ns2")), trg)

  # wait mirai elements to be resolved
  miall <- c(unclass(m1$mirai), unclass(m2$mirai))
  all_resolved <- all(!sapply(miall, mirai::unresolved))

  while (!all_resolved) {
    all_resolved <- all(!sapply(miall, mirai::unresolved))
  }

  # Sys.sleep(1)
  # expect_equal(sto$get("a"), 1)
  # expect_equal(sto$get("b", "ns2"), 2)

  # test cached values (note mget/get always goes on db to check if hash exists
  # so we wait async to complete)
  expect_equal(sto$mget(h, c("objects", "ns2")), list(1, 2))

  # test key/keymeta are saved on disk
  expect_equal(sto$mget(h, c("objects", "ns2"), use_cache = FALSE), list(1, 2))
  expect_equal(sto$mget_keymeta(h, c("objects", "ns2"), use_cache = FALSE),
               trg, ignore_attr = TRUE)

  # test inputs
  expect_error(sto$set_by_value_async(1, namespace = c("a", "b")),
               "'namespace' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  expect_error(sto$set_by_value_async(1, "a", expires_at = "a"),
               "'expires_at' should be a date-time object, not character",
               fixed = TRUE,
               class = "error")

  expect_error(sto$set_by_value_async(1, "a", expires_at = rep(as.POSIXct(NA), 2)),
               "'expires_at' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  expect_error(sto$set_by_value_async(1, "a", notes = 1),
               "'notes' should be a character string, not numeric",
               fixed = TRUE,
               class = "error")

  expect_error(sto$set_by_value_async(1, "a", notes = c("a", "b")),
               "'notes' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

})


test_that("mset_by_value_async", {

  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE, async = TRUE)

  t0 <- Sys.time()
  expect_no_error(m1 <- sto$mset_by_value_async(value = c(1, 2),
                                                namespace = c("ns1", "ns2"),
                                                expires_at = c(t0, t0),
                                                notes = c("async1", "async2")))

  h <- m1$hash
  expect_named(m1, c("mirai", "hash"))
  expect_all_true(sapply(m1$mirai, mirai::is_mirai))


  # test cached keymeta
  trg <- list(list(expires_at = t0, notes = "async1"),
              list(expires_at = t0, notes = "async2"))

  expect_equal(sto$mget_keymeta(h, c("ns1", "ns2")), trg)

  # wait mirai elements to be resolved
  miall <- unclass(m1$mirai)
  all_resolved <- all(!sapply(miall, mirai::unresolved))

  while (!all_resolved) {
    all_resolved <- all(!sapply(miall, mirai::unresolved))
  }

  # test cached values
  expect_equal(sto$mget(h, c("ns1", "ns2")), list(1, 2))

  # test key/keymeta are saved on disk
  expect_equal(sto$mget(h, c("ns1", "ns2"), use_cache = FALSE), list(1, 2))
  expect_equal(sto$mget_keymeta(h, c("ns1", "ns2"), use_cache = FALSE),
               trg, ignore_attr = TRUE)

  # test inputs
  # expect_error(sto$mset_by_value_async(c("a", "b"), namespace = c("ns1", "ns2", "ns3")),
  #              "Incompatible lengths for key and namespace",
  #              fixed = TRUE,
  #              class = "error")

  expect_error(sto$set_by_value_async("a", namespace = c("a", "b")),
               "'namespace' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  expect_error(sto$set_by_value_async("a", notes = 1),
               "'notes' should be a character string, not numeric",
               fixed = TRUE,
               class = "error")

  expect_error(sto$mset_by_value_async( 1, notes = c("a", "b")),
               "'notes' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  expect_error(sto$mset_by_value_async( 1, expires_at = c(as.POSIXct(1), as.POSIXct(2))),
               "'expires_at' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  expect_error(sto$mset_by_value_async( 1, expires_at = c("a", "b")),
               "'expires_at' should be a date-time object, not character",
               fixed = TRUE,
               class = "error")


})


test_that("set_keymeta_async", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, async = TRUE)

  # set a key with default metadata
  sto$set("x", 1)

  # set keymeta (update both expires_at and notes)
  trgval <- list(expires_at = as.POSIXct(1, tz = NULL), notes = "😀")
  expect_no_error(m1 <- sto$set_keymeta_async("x", expires_at = trgval$expires_at,
                                              notes = trgval$notes
  ))

  expect_named(m1, c("mirai", "keyns"))
  expect_true( mirai::is_mirai(m1$mirai))
  expect_equal(m1$keyns, "x:objects")

  expect_equal(sto$get_keymeta("x"), trgval)

  # wait mirai elements to be resolved
  miall <- m1$mirai
  all_resolved <- !mirai::unresolved(miall)

  while (!all_resolved) {
    all_resolved <- !mirai::unresolved(miall)
  }

  expect_equal(sto$get_keymeta("x", use_cache = FALSE), trgval)


  # check assertions
  expect_error(sto$set_keymeta_async("y", namespace = "ns2", notes = "nokey"),
               "key 'y' ('ns2') not found",
               fixed = TRUE,
               class = "KeyError")


  expect_error(sto$set_keymeta_async(c("x", "y")),
               "'key' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  expect_error(sto$set_keymeta_async("x", c("ns1", "ns2")),
               "'namespace' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")


  expect_error(sto$set_keymeta_async("x", expires_at = 1),
               "'expires_at' should be a date-time object, not numeric",
               fixed = TRUE,
               class = "error")

  expect_error(sto$set_keymeta_async("x", expires_at = c(as.POSIXct(1), as.POSIXct(2))),
               "'expires_at' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  expect_error(sto$set_keymeta_async("x", notes = 1),
               "'notes' should be a character string, not numeric",
               fixed = TRUE,
               class = "error")

  expect_error(sto$set_keymeta_async("x", notes = c("a", "v")),
               "'notes' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  })


test_that("mset_keymeta_async", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, async = TRUE)

  # set a key with default metadata
  sto$mset(c("x", "y"), c(1, 2))

  trg <- "x:objects"

  # set keymeta (update both expires_at and notes)
  trgval <- list(list(expires_at = as.POSIXct(1), notes = "😀"),
                 list(expires_at = as.POSIXct(NA), notes = NA_character_))

  expect_no_error(m1 <- sto$mset_keymeta_async(c("x", "y"),
                                              expires_at = c(as.POSIXct(1),
                                                             as.POSIXct(NA)),
                                              notes = c("😀", NA_character_)))

  expect_named(m1, c("mirai", "keyns"))
  expect_true( mirai::is_mirai(m1$mirai))
  expect_equal(m1$keyns, c("x:objects", "y:objects"))

  expect_equal(sto$mget_keymeta(c("x", "y")), trgval)

  # wait mirai elements to be resolved
  miall <- m1$mirai
  all_resolved <- !mirai::unresolved(miall)

  while (!all_resolved) {
    all_resolved <- !mirai::unresolved(miall)
  }

  expect_equal(sto$mget_keymeta(c("x", "y"), use_cache = FALSE), trgval,
               ignore_attr = TRUE)


  # check assertions
  expect_error(sto$mset_keymeta_async(c("x", "v"), notes = c(NA_character_, NA_character_)),
               "key 'v' ('objects') not found",
               fixed = TRUE,
               class = "KeyError")


  # test key-namespace not found
  expect_error(sto$mset_keymeta_async(c("x1", "v"), c("obj1", "obj2"), notes = rep(NA_character_, 2)),
               "key 'x1,v' ('obj1,obj2') not found",
               fixed = TRUE,
               class = "error")

  # check key-namespace for incompatibility
  expect_error(sto$mset_keymeta(c("x", "y", "z"), namespace = c("objects", "objects")),
               "Incompatible lengths for key and namespace",
               fixed = TRUE,
               class = "error")

  expect_error(sto$mset_keymeta_async("x", notes = 1),
               "'notes' should be a character string, not numeric",
               fixed = TRUE,
               class = "error")

  expect_error(sto$mset_keymeta_async("x", notes = c("a", "b")),
               "'notes' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  expect_error(sto$mset_keymeta_async("x", expires_at = "a"),
               "'expires_at' should be a date-time object, not character",
               fixed = TRUE,
               class = "error")

  expect_error(sto$mset_keymeta_async("x", expires_at = rep(as.POSIXct(NA), 3)),
               "'expires_at' must have 1 elements (recieved 3)",
               fixed = TRUE,
               class = "error")


})
