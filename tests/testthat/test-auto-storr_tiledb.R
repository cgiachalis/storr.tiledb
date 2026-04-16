oldsize <- tiledb::get_allocation_size_preference()
tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
on.exit(tiledb::set_allocation_size_preference(oldsize))

# IMPORTANT: Rerun spec's storr test suite from storr package.
#
# Because 'storr_tiledb()' is a subclass of 'storr' and it
# overrides key methods, we need to ensure that its methods
# give the expected results which are identical to
# storr, e.g. 'storr(driver_tiledb())'.

# NOTES: 'storr_tiledb' '$clear()' returns a logical vector
# whereas 'storr' returns number of deleted keys

test_that("basic", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  #driver_tiledb_create(uri)
  st <- storr_tiledb(uri, init = TRUE)

  expect_s3_class(st, "TileDBStorr")

  ## At this point no namespaces (this might be relaxed)
  expect_identical(st$list_namespaces(), character(0))

  expect_identical(st$list(), character(0))
  expect_identical(st$list_hashes(), character(0))
  ## The objects namespace is allowed here because it's the storr
  ## default namespace; simply querying it above may allow it to come
  ## into being.
  expect_identical(setdiff(st$list_namespaces(), "objects"),
                             character(0))

  key <- "aaa"

  expect_error(st$get(key),
                         sprintf("key '%s' ('objects') not found", key),
                         fixed = TRUE,
                         class = "KeyError")

  d <- runif(100)
  hash <- st$hash_object(d)

  res <- st$set(key, d)
  expect_true(st$exists(key))
  expect_true(st$exists_object(hash))

  expect_identical(res, hash)
  expect_identical(st$list(), key)
  expect_identical(st$list_hashes(), hash)
  expect_identical(st$get_hash(key), hash)
  expect_equal(st$get(key), d, tolerance = 1e-15)
  expect_equal(st$get(key, use_cache = FALSE), d,
                         tolerance = 1e-15)
  expect_equal(st$get_value(hash), d, tolerance = 1e-15)

  # NOTE: storr_tiledb uses hash table instead of environment
  expect_identical(hashkeys(st$envir), hash)

  expect_identical(st$list_namespaces(), "objects")

  ## Set a second key to to the same value:
  key2 <- "bbb"
  st$set(key2, d)
  expect_identical(sort(st$list()), c(key, key2))
  expect_identical(st$list_hashes(), hash)
  expect_identical(st$get_hash(key2), hash)
  expect_equal(st$get(key2), d, tolerance = 1e-15)
  expect_equal(st$get(key2, use_cache = FALSE), d,
                         tolerance = 1e-15)

  ## Drop key:
  expect_true(st$del(key))
  expect_identical(st$list(), key2)
  expect_identical(st$list_hashes(), hash)
  expect_equal(st$get(key2), d, tolerance = 1e-15)

  ## Drop the other key:
  expect_true(st$del(key2))
  expect_identical(st$list(), character(0))
  expect_identical(st$list_hashes(), hash)

  drop <- st$gc()
  expect_identical(drop, hash)
  expect_identical(st$list_hashes(), character(0))

  expect_identical(hashkeys(st$envir), character(0))

  ## Skip the cache on the way in:
  st$set(key2, d, use_cache = FALSE)
  expect_identical(hashkeys(st$envir), character(0))
  expect_equal(st$get(key2), d, tolerance = 1e-15)
  expect_equal(st$get(key2, use_cache = FALSE), d,
                         tolerance = 1e-15)
})

test_that("replace value", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  st <- storr_tiledb(uri, init = TRUE)

  x <- runif(5)
  y <- runif(10)

  st$set("key", x)
  expect_equal(st$get("key", use_cache = FALSE), x)

  st$set("key", y)
  expect_equal(st$get("key", use_cache = FALSE), y)
})


test_that("default namespace", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  driver_tiledb_create(uri)

  st0 <- storr_tiledb(uri)
  st <- storr_tiledb(uri,  default_namespace = "storr")

  expect_identical(st$default_namespace, "storr")

  st$set("foo", 1:10)
  expect_identical(st$list("objects"), character(0))
  expect_identical(st$list("storr"), "foo")

  st0$set("foo", letters)
  expect_identical(st0$get("foo"), letters)
  expect_identical(st$get("foo"), 1:10)
})


test_that("set_by_value", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  st <- storr_tiledb(uri, init = TRUE)

  x <- runif(10)
  h <- st$set_by_value(x)
  expect_identical(h, st$hash_object(x))
  expect_identical(st$list_hashes(), h)
  expect_identical(st$list(), h)
  expect_equal(st$get(h), x)
})


test_that("clear", {
  uri <- file.path(withr::local_tempdir(), "test-storr")
  st <- storr_tiledb(uri, init = TRUE)

  st$set("a1", 1, namespace = "a")
  st$set("a2", 2, namespace = "a")
  st$set("b1", 1, namespace = "b")

  expect_equal(sort(st$list("a")), sort(c("a1", "a2")))
  expect_equal(st$clear("a"), TRUE)
  expect_equal(st$list("a"), character(0))

  expect_equal(st$list("b"), "b1")
  expect_equal(st$clear(NULL), TRUE)
  expect_equal(st$list("b"), character(0))

  st$set("a1", 1, namespace = "a")
  st$set("a2", 2, namespace = "a")
  st$set("b1", 1, namespace = "b")
  expect_equal(st$clear(NULL), c(TRUE, TRUE))
  expect_equal(st$clear(NULL), NULL)
  expect_equal(st$clear("no_such_namespace"), NULL)
})



test_that("reconnect", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  st <- storr_tiledb(uri, init = TRUE)

  st$set("a1", 1, namespace = "a")
  st$set("a2", 2, namespace = "a")
  st$set("b1", 1, namespace = "b")

  st2 <- storr_tiledb(uri)

  expect_equal(st2$list_namespaces(), st$list_namespaces())
  expect_equal(st2$list("a"), st$list("a"))
  expect_equal(st2$get("a1", "a"), st$get("a1", "a"))
})



test_that("hash_algorithm", {

  uri <- file.path(withr::local_tempdir(), "test-driver")

  .driver_create <- function(dr = NULL, ...) {

    arg <- list(...)

    if (is.null(dr)) {

      driver_tiledb_create(uri, hash_algorithm = arg$hash_algorithm)
      driver_tiledb(uri)

    } else {

      if (!is.null(arg$hash_algorithm)) {
        if (!identical(dr$hash_algorithm, arg$hash_algorithm)) {
          stop(ConfigError("hash_algorithm", dr$hash_algorithm, arg$hash_algorithm))
        }

      }
      dr
    }
  }

  hash_algos <- c("md5", "sha1")
  x <- runif(10)
  key <- "foo"

  uri <- file.path(withr::local_tempdir(), "test-storr")
  dr <- .driver_create()
  traits <- storr_traits(dr$traits)
  dr$destroy()
  if (!traits$hash_algorithm) {
    skip("hash_algorithm not supported")
  }
## TODO: This is very ugly and mimics the behaviour in storr.
## However, it's required so that we don't rely on md5 being the
## default hash algorithm!
hmd5 <- make_hash_serialized_object("md5", !traits$drop_r_version)(
    make_serialize_object(traits$drop_r_version, traits$accept == "string")(
      x))

for (h in hash_algos) {

  dr <- .driver_create(hash_algorithm = h)
  on.exit(dr$destroy())

  expect_equal(dr$hash_algorithm, h)

  st <- storr_tiledb(uri)
  st$set(key, x)

  expect_equal(st$get(key), x)
  hash <- st$hash_object(x)
  expect_equal(st$list_hashes(), hash)
  ## Sanity check
  expect_equal(hash == hmd5, h == "md5")

  h_other <- setdiff(hash_algos, h)[[1L]]

  ## TODO: doing this with
  ##
  ##   expect_error(.driver_create(dr, hash_algorithm = h_other),
  ##                           "Incompatible value for hash_algorithm")
  ##
  ## would be preferable, but testthat gives a warning about this up
  ## to version 3.0.0 and thor does not include an appropriate error
  ## class.
  e <- expect_error(.driver_create(dr, hash_algorithm = h_other))
  expect_match(e$message, "Incompatible value for hash_algorithm")

  expect_equal(.driver_create(dr)$hash_algorithm, h)

  dr$destroy()
}
on.exit()
})


test_that("get_value", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  st <- storr_tiledb(uri, init = TRUE)

  x <- runif(10)
  st$set("a", x)

  h <- st$list_hashes()
  expect_equal(st$get_value(h), x)

  expect_error(st$get_value("nosuchhash"),
                         "hash 'nosuchhash' not found",
                         class = "HashError")
})


test_that("mget_value", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  st <- storr_tiledb(uri, init = TRUE)

  st$set("a", 1, use_cache = FALSE)
  st$set("a", 1, namespace = "ns1", use_cache = F)
  st$set("b", 3, namespace = "ns1", use_cache = F)
  st$set("d", 4, namespace = "ns1", use_cache = F)

  hashes <- st$index_export()[, 1:3][["hash"]]

  expect_equal(st$mget_value(hashes, use_cache = FALSE), list(1, 1, 3, 4))

  expect_equal(st$mget_value(hashes[c(4,3,2)], use_cache = FALSE), list(4, 3, 1))

  expect_equal(st$mget_value(hashes), list(1, 1, 3, 4))

  expect_equal(st$mget_value(hashes[c(4,3,2)]), list(4, 3, 1))

})

## Really simple test to make sure that mget works correctly.  This is
## primarily up to storr, rather than the driver, because we'll test
## mget at the driver level separately.
test_that("mget", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  st <- storr_tiledb(uri, init = TRUE)

  h1 <- st$set("foo", 1)
  h2 <- st$set("bar", 2)
  h3 <- st$set("baz", 3, "other")

  expect_equal(st$mget(character(0)), list())
  expect_equal(st$mget("foo"), list(1))
  expect_equal(st$mget(c("foo", "bar")), list(1, 2))
  expect_equal(st$mget(c("foo", "baz", "bar")),
                         structure(list(1, NULL, 2), missing = 2))

  expect_equal(st$mget_hash(c("foo", "bar")), c(h1, h2))
  expect_equal(st$mget_hash(character(0)), character(0))
  expect_equal(st$mget_hash("baz"), NA_character_)
  expect_equal(st$mget_hash(c("foo", "baz", "bar")),
                         c(h1, NA, h2))

  expect_equal(st$mget_hash(c("foo", "baz", "bar"),
                                      c("objects", "other", "objects")),
                         c(h1, h3, h2))
})


test_that("mset", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  st <- storr_tiledb(uri, init = TRUE)

  h <- st$mset(c("foo", "bar"), c(1, 2))
  expect_equal(st$mget_hash(c("foo", "bar")), h)

  ## Multiple namespaces at once:
  h <- st$mset(c("a", "b", "c"), 1:3, c("x", "y", "z"))
  expect_equal(st$get("a", "x"), 1)
  expect_equal(st$get("b", "y"), 2)
  expect_equal(st$get("c", "z"), 3)

  ## TODO: test that when value is the wrong length for the hashes we
  ## throw an error.  The drivers are allowed to assume this.
})


## This is really a test of storr, and if the tests above pass these
## should all pass easily.  Putting them here means that they test
## both the with-mget and without-mget branches though.
test_that("avoiding caching", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  st <- storr_tiledb(uri, init = TRUE)

  st$mset(c("a", "b"), 1:2, use_cache = FALSE)
  expect_equal(hashkeys(st$envir), character(0))
  expect_equal(st$mget(c("a", "b")), list(1, 2))
})


test_that("gc", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  st <- storr_tiledb(uri, init = TRUE)

  x <- runif(10)
  y <- letters
  z <- cos

  hx <- st$set("a", x)
  hy <- st$set("b", y)
  hz <- st$set("c", z)
  st$set("x", x)
  st$set("y", y, "other")

  expect_equal(st$gc(), character(0))

  st$del("b")
  expect_equal(st$gc(), character(0))
  st$del("y", "other")
  expect_equal(st$gc(), hy)

  st$del(c("a", "c", "x"))
  expect_equal(st$list(), character(0))
  expect_equal(sort(st$gc()), sort(c(hx, hz)))
})


test_that("destroy", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  st <- storr_tiledb(uri, init = TRUE)

  expect_equal(tiledb::tiledb_object_type(uri), "GROUP")
  expect_null(st$destroy())
  expect_equal(tiledb::tiledb_object_type(uri), "INVALID")

})

test_that("fill", {
  uri <- file.path(withr::local_tempdir(), "test-storr")
  st <- storr_tiledb(uri, init = TRUE)

  v <- runif(10)
  keys <- letters[1:3]
  h <- st$fill(keys, v)
  expect_equal(h, st$hash_object(v))
  expect_equal(st$mget(keys),
               rep(list(v), length(keys)))
})


test_that("duplicate", {
  uri <- file.path(withr::local_tempdir(), "test-storr")
  st <- storr_tiledb(uri, init = TRUE)

  h1 <- st$set("a", runif(10))
  expect_null(st$duplicate("a", "b"))

  expect_identical(st$get("b"), st$get("a"))
  expect_identical(st$get_hash("b"), st$get_hash("a"))
})


test_that("index - empty", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  st <- storr_tiledb(uri, init = TRUE)

  d <- data.frame(
    namespace = character(0),
    key = character(0),
    hash = character(0),
    expires_at = as.POSIXct(double()),
    notes = character(0)
  )
  trg <- data.table::as.data.table(d)
  expect_identical(st$index_export(), trg)

  expect_silent(st$index_import(st$index_export()))
  expect_equal(st$list_hashes(), character(0))
})


test_that("index one namespace", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  st <- storr_tiledb(uri, init = TRUE)
  letters4 <- letters[1:4]
  st$mset(letters4, LETTERS[1:4],
          expires_at = c(rep(as.POSIXct(NA),3), as.POSIXct(1)),
          notes = c("hi", rep(NA_character_, 3)))

  d <- st$index_export()
  cmp <- data.frame(
    namespace = "objects",
    key = sort( letters4),
    hash = vcapply(toupper(sort( letters4)), st$hash_object, USE.NAMES = FALSE),
    expires_at = c(rep(as.POSIXct(NA),3), as.POSIXct(1)),
    notes = c("hi", rep(NA_character_, 3)))

  expect_equal(d, cmp, ignore_attr = TRUE)

  st$del(letters4)
  expect_equal(nrow(st$index_export()), 0L)

  st$index_import(d)
  expect_identical(st$index_export(), d)
})


test_that("index multiple namespaces", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  st <- storr_tiledb(uri, init = TRUE)

  k1 <- letters[1:10]
  k2 <- letters[7:13]
  v1 <- runif(length(k1))
  v2 <- runif(length(k2))

  st$mset(k1, v1, "n1")
  st$mset(k2, v2, "n2")

  d <- st$index_export()

  trg <- data.frame(namespace =  rep(c("n1", "n2"), c(length(k1), length(k2))),
                    key = c(k1, k2),
                    hash = vcapply(c(v1, v2), st$hash_object, USE.NAMES = FALSE),
                    expires_at = as.POSIXct(NA),
                    notes = NA_character_)

  trg <- sort_by(trg, ~key)
  expect_equal(d, trg, ignore_attr = TRUE)


  d1 <- st$index_export("n1")
  d2 <- st$index_export("n2")
  expect_equal(d1,  subset(trg, namespace == "n1"), ignore_attr = TRUE)
  expect_equal(d2,  subset(trg, namespace == "n2"), ignore_attr = TRUE)
})


test_that("invalid import", {
  uri <- file.path(withr::local_tempdir(), "test-storr")
  st <- storr_tiledb(uri, init = TRUE)

  d <- data.frame(namespace = "objects",
                  key = "foo",
                  hash = st$hash_object(1))

  expect_error(
    st$index_import(mtcars),
    "Missing required columns for index: 'namespace', 'key', 'hash'",
    fixed = TRUE)
  expect_error(st$index_import(d),
               "Missing 1 / 1 hashes - can't import")
  d$key <- factor(d$key)
  expect_error(st$index_import(d),
               "Column not character: 'key'")
})
