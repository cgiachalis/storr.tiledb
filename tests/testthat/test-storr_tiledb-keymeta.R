oldsize <- tiledb::get_allocation_size_preference()
tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
on.exit(tiledb::set_allocation_size_preference(oldsize))


test_that("set_keymeta", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  driver_tiledb_create(uri)
  sto <- storr_tiledb(uri)

  # set a key with default metadata
  sto$set("x", 1)

  trg <- "x:objects"

  # set keymeta (update both expires_at and notes)
  trgval <- list(expires_at = as.POSIXct(1, tz = NULL), notes = "😀")
  expect_equal(sto$set_keymeta("x",
                               expires_at = trgval$expires_at, notes = trgval$notes), trg)

  # test that were saved
  expect_equal(sto$get_keymeta("x"), trgval)
  expect_equal(sto$get_keymeta("x", use_cache = FALSE), trgval)

  # update note only
  expect_equal(sto$set_keymeta("x", notes = intToUtf8("0x1f608")), trg)
  # test note update
  trgval <- list(expires_at = as.POSIXct(1, tz = NULL), notes = "😈")
  expect_equal(sto$get_keymeta("x"), trgval)

  # update datetime only
  expect_equal(sto$set_keymeta("x", expires_at = as.POSIXct(NA)), trg)

  trgval <- list(expires_at = as.POSIXct(NA), notes = "😈")

  # test datetime update
  expect_equal(sto$get_keymeta("x"), trgval)
  expect_equal(sto$get_keymeta("x", use_cache = FALSE), trgval)

  # nothing to update, return empty character()
  expect_equal(sto$set_keymeta("x"), character())

  # test we don't copy to cache
  expect_equal(sto$set_keymeta("x", expires_at = as.POSIXct(100), use_cache = FALSE), trg)
  trgval_new <- list(expires_at = as.POSIXct(100, tz = NULL), notes = "😈")

  # test cache is empty for this pair (use_cache = FASLE always removes key)
  expect_null(sto$envir_metadata[["x:objects"]])

  # now with use_cache = TRUE, it reaches database and then fills cache
  expect_equal(sto$get_keymeta("x", use_cache = TRUE), trgval_new)

  # cache for this key is filled
  expect_equal(sto$envir_metadata[["x:objects"]], trgval_new)

  # test again the datetime update but dont use cache
  expect_equal(sto$get_keymeta("x", use_cache = FALSE), trgval_new)

  # check assertions
  expect_error(sto$set_keymeta("y",namespace = "ns2", notes = "nokey"),
               "key 'y' ('ns2') not found",
               fixed = TRUE,
               class = "KeyError")


  expect_error(sto$set_keymeta(c("x", "y")),
               "'key' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  expect_error(sto$set_keymeta("x", c("ns1", "ns2")),
               "'namespace' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")


  expect_error(sto$set_keymeta("x", expires_at = 1),
               "'expires_at' should be a date-time object, not numeric",
               fixed = TRUE,
               class = "error")

  expect_error(sto$set_keymeta("x", expires_at = c(as.POSIXct(1), as.POSIXct(2))),
               "'expires_at' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  expect_error(sto$set_keymeta("x", notes = 1),
               "'notes' should be a character string, not numeric",
               fixed = TRUE,
               class = "error")

  expect_error(sto$set_keymeta("x", notes = c("a", "v")),
               "'notes' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

})


test_that("get_keymeta", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  driver_tiledb_create(uri)
  sto <- storr_tiledb(uri)

  # set a key with default metadata
  sto$set("x", 1)

  # get defualt keymeta
  trgval <- list(expires_at = as.POSIXct(NA), notes = NA_character_)

  expect_equal(sto$get_keymeta("x"), trgval)
  expect_equal(sto$get_keymeta("x", use_cache = FALSE), trgval)

  # check keymeta cache
  sto$flush_cache()
  expect_equal(numhash(sto$envir_metadata), 0)

  # this will retrieve from database not from cache, but saves to cache afterwards
  expect_equal(sto$get_keymeta("x", use_cache = TRUE), trgval)
  # keymeta cache must be filled up now
  expect_equal(numhash(sto$envir_metadata), 1)
  expect_equal(sto$envir_metadata[["x:objects"]], trgval)

  # now test gettimg keymeta from disk but dont copy to cache
  sto$flush_cache()
  expect_equal(sto$get_keymeta("x", use_cache = FALSE), trgval)

  # test we didn't copy to cache
  expect_equal(numhash(sto$envir_metadata), 0)

  # test assertions etc..
  expect_error(sto$get_keymeta("y",namespace = "ns2"),
               "key 'y' ('ns2') not found",
               fixed = TRUE,
               class = "KeyError")


  expect_error(sto$get_keymeta(c("x", "y")),
               "'key' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  expect_error(sto$get_keymeta("x", c("ns1", "ns2")),
               "'namespace' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  })


test_that("mget_keymeta", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  driver_tiledb_create(uri)
  sto <- storr_tiledb(uri)

  # set some keys with key metadata
  sto$mset(c("x", "y", "z"), c(1, 2, 3),
           expires_at = as.POSIXct(1:3, tz = NULL),
           notes = c("xnote", "ynote", "znote"))

  # expected target list
  expires_at <-  as.POSIXct(1:3, tz = NULL)
  notes <- c("xnote", "ynote", "znote")
  trg <- vector("list", 3)
  for(i in seq_along(notes)) {
    trg[[i]] <- list(expires_at = expires_at[i], notes = notes[i])
  }

  # check keymeta were stored correctly
  expect_equal(sto$mget_keymeta(c("x", "y", "z")), trg)
  expect_equal(sto$mget_keymeta(c("x", "y", "z"), use_cache = FALSE), trg)

  # check keymeta cache
  sto$flush_cache()
  expect_equal(numhash(sto$envir_metadata), 0)

  # check we fill up cache
  expect_equal(sto$mget_keymeta(c("x", "y", "z")), trg)
  expect_equal(numhash(sto$envir_metadata), 3)
  expect_equal(sto$envir_metadata[["x:objects"]], trg[[1]])

  # check we're not copying into cache
  sto$flush_cache()
  expect_equal(sto$mget_keymeta(c("x", "y"), use_cache = FALSE), trg[-3])
  expect_equal(numhash(sto$envir_metadata), 0)

  # fetch a not found single key
  expect_equal(sto$mget_keymeta("k"),structure(list(list(NULL)), missing = 1L))
  expect_equal(sto$mget_keymeta("k", use_cache = FALSE),structure(list(list(NULL)), missing = 1L))

  # k, v are missing, set missing val
  trg <- structure(list(
    "nometa",
    list(expires_at = structure(1L, class = c("POSIXct", "POSIXt")), notes = "xnote"),
    "nometa"
  ), missing = c(1L, 3L))

  expect_equal(sto$mget_keymeta(c("k", "x", "v"), missing = "nometa"), trg)

  # x, y but from not found namespace
  expect_equal(sto$mget_keymeta(c("x", "y"), namespace = "not_objects"), structure(list(list(NULL), list(NULL)), missing = 1:2))


  # check key-namespace for incompatibility
  expect_error(sto$mget_keymeta(c("x", "y", "z"), namespace = c("objects", "objects")),
               "Incompatible lengths for key and namespace",
               fixed = TRUE,
               class = "error")

})


test_that("mset_keymeta", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  driver_tiledb_create(uri)
  sto <- storr_tiledb(uri)
  keys <- c("x", "y", "z")
  km <- paste(keys, "objects", sep = ":")

  # set some keys with no metadata (default)
  sto$mset(c("x", "y", "z"), c(1, 2, 3))

  # now set only notes
  expect_equal(sto$mset_keymeta(c("x", "y", "z"),
                                notes =  c("xnote", "ynote", "znote")),
                                km)


  # test that notes were stored
  notes <- c("xnote", "ynote", "znote")
  trg <- vector("list", 3)
  for(i in seq_along(notes)) {
    trg[[i]] <- list(expires_at = as.POSIXct(NA), notes = notes[i])
  }

  expect_equal(sto$mget_keymeta(keys), trg)

  # note: use ignore_attr return val includes tzone attr with value ''
  expect_equal(sto$mget_keymeta(keys, use_cache = FALSE), trg, ignore_attr = TRUE)

  # continue with settling expiries only
  expires_at <-  as.POSIXct(1:3, tz = NULL)
  expect_equal(sto$mset_keymeta(keys, expires_at = expires_at), km)

  # test datetimes were stored
  for(i in seq_along(expires_at)) {
    trg[[i]]$expires_at <- expires_at[i]
  }
  expect_equal(sto$mget_keymeta(keys), trg)
  expect_equal(sto$mget_keymeta(keys, use_cache = FALSE), trg)

  # check cache --

  # test we don't copy to cache
  expect_equal(sto$mset_keymeta("x", expires_at = as.POSIXct(100), use_cache = FALSE), km[1])
  trgval_new <- list(expires_at = as.POSIXct(100, tz = NULL), notes = "xnote")

  # test cache is empty for this pair (use_cache = FASLE always removes keys)
  expect_null(sto$envir_metadata[["x:objects"]])

  # now with use_cache = TRUE, it reaches database and then fills cache
  expect_equal(sto$get_keymeta("x", use_cache = TRUE), trgval_new)

  # cache for this key is filled
  expect_equal(sto$envir_metadata[["x:objects"]], trgval_new)

  # test again the datetime update but don't use cache
  expect_equal(sto$get_keymeta("x", use_cache = FALSE), trgval_new)

  # --

  # reset x, y to default keymeta
  expect_equal(sto$mset_keymeta(c("x", "z"), expires_at = rep(as.POSIXct(NA, tz = NULL), 2),
                                notes = c(NA_character_, NA_character_)),
               c("x:objects", "z:objects"))



  trg <- list(
    list(expires_at = structure(NA_real_, class = c("POSIXct", "POSIXt")), notes = NA_character_),
    list(expires_at = structure(NA_real_, class = c("POSIXct", "POSIXt")), notes = NA_character_)
  )
  expect_equal(sto$mget_keymeta(c("x", "z")), trg)

  expect_equal(sto$mget_keymeta(c("x", "z"), use_cache = FALSE), trg)


  # nothing to set
  expect_equal(sto$mset_keymeta(c("x", "y")), character())

  # test key-namespace not found
  expect_error(sto$mset_keymeta(c("x", "v"), notes = c(NA_character_, NA_character_)),
               "key 'v' ('objects') not found",
               fixed = TRUE,
               class = "error")

  # test key-namespace not found
  expect_error(sto$mset_keymeta(c("x1", "v"), c("obj1", "obj2"), notes = rep(NA_character_, 2)),
               "key 'x1,v' ('obj1,obj2') not found",
               fixed = TRUE,
               class = "error")

  # check key-namespace for incompatibility
  expect_error(sto$mset_keymeta(c("x", "y", "z"), namespace = c("objects", "objects")),
               "Incompatible lengths for key and namespace",
               fixed = TRUE,
               class = "error")

  })
