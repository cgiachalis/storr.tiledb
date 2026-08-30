oldsize <- tiledb::get_allocation_size_preference()
tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
on.exit(tiledb::set_allocation_size_preference(oldsize))


test_that("update_keymeta", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE)

  # set a key with default metadata
  sto$set("x", 1)

  trg <- "x:objects"

  # set keymeta (update both expires_at and notes)
  trgval <- list(expires_at = as.POSIXct(1, tz = NULL), notes = "😀")
  expect_equal(sto$update_keymeta("x",
                               expires_at = trgval$expires_at,
                               notes = trgval$notes), trg)

  # test that were saved
  expect_equal(sto$get_keymeta("x"), trgval)
  expect_equal(sto$get_keymeta("x", use_cache = FALSE), trgval)

  # update note only
  expect_equal(sto$update_keymeta("x", notes = intToUtf8("0x1f608")), trg)

  # test note update
  trgval <- list(expires_at = as.POSIXct(1, tz = NULL), notes = "😈")
  expect_equal(sto$get_keymeta("x"), trgval)

  # update datetime only
  expect_equal(sto$update_keymeta("x", expires_at = as.POSIXct(NA)), trg)

  trgval <- list(expires_at = as.POSIXct(NA), notes = "😈")

  # test datetime update
  expect_equal(sto$get_keymeta("x"), trgval)
  expect_equal(sto$get_keymeta("x", use_cache = FALSE), trgval)

  # nothing to update, return empty character()
  expect_equal(sto$update_keymeta("x"), character())

  # test we don't copy to cache
  expect_equal(sto$update_keymeta("x", expires_at = as.POSIXct(100), use_cache = FALSE), trg)
  trgval_new <- list(expires_at = as.POSIXct(100, tz = NULL), notes = "😈")

  # test cache is empty for this pair (use_cache = FASLE always removes key)
  expect_null(sto$envir_metadata[["x:objects"]])

  # now with use_cache = TRUE, it reaches database and then fills cache
  expect_equal(sto$get_keymeta("x", use_cache = TRUE), trgval_new)

  # cache for this key is filled
  expect_equal(sto$envir_metadata[["x:objects"]], trgval_new)

  # test again the datetime update but dont use cache
  expect_equal(sto$get_keymeta("x", use_cache = FALSE), trgval_new)

  # With use_cache = TRUE, on clean cache
  sto$flush_cache()
  expect_equal(numhash(sto$envir_metadata), 0)
  expect_equal(sto$update_keymeta("x", expires_at = as.POSIXct(NA), use_cache = TRUE), trg)
  # 'notes' are not overridden
  expect_equal(sto$get_keymeta("x", use_cache = TRUE), trgval)
  expect_equal(sto$get_keymeta("x", use_cache = FALSE), trgval)


  # check assertions
  expect_error(sto$update_keymeta("y",namespace = "ns2", notes = "nokey"),
               "key 'y' ('ns2') not found",
               fixed = TRUE,
               class = "KeyError")


  expect_error(sto$update_keymeta(c("x", "y")),
               "'key' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  expect_error(sto$update_keymeta("x", c("ns1", "ns2")),
               "'namespace' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")


  expect_error(sto$update_keymeta("x", expires_at = 1),
               "'expires_at' should be a date-time object, not numeric",
               fixed = TRUE,
               class = "error")

  expect_error(sto$update_keymeta("x", expires_at = c(as.POSIXct(1), as.POSIXct(2))),
               "'expires_at' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  expect_error(sto$update_keymeta("x", notes = 1),
               "'notes' should be a character string, not numeric",
               fixed = TRUE,
               class = "error")

  expect_error(sto$update_keymeta("x", notes = c("a", "v")),
               "'notes' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

})


test_that("get_keymeta", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE)

  # set a key with default metadata
  sto$set("x", 1)

  # get default keymeta
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

  # now test getting keymeta from disk but don't copy to cache
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
  sto <- storr_tiledb(uri, init = TRUE)

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

test_that("get_keymeta_expires_at and get_keymeta_notes", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE)

  # add some keys
  sto$set("x", 1)
  t0 <- Sys.time()+100
  sto$set("y", 1, expires_at = t0, notes = "name:Bob")
  expect_equal(numhash(sto$envir_metadata), 2)
  expect_equal(sto$get_keymeta_notes("x", use_cache = TRUE), NA_character_)
  expect_equal(sto$get_keymeta_notes("y", use_cache = TRUE), "name:Bob")
  expect_equal(sto$get_keymeta_expires_at("x", use_cache = TRUE), as.POSIXct(NA))
  expect_equal(sto$get_keymeta_expires_at("y", use_cache = TRUE), t0)

  # use_cache = FALSE with empty cache (no cache writes)
  sto$flush_cache()
  expect_equal(numhash(sto$envir_metadata), 0)
  expect_equal(sto$get_keymeta_notes("x", use_cache = FALSE), NA_character_)
  expect_equal(sto$get_keymeta_notes("y", use_cache = FALSE), "name:Bob")
  expect_equal(sto$get_keymeta_expires_at("x", use_cache = FALSE), as.POSIXct(NA))
  expect_equal(sto$get_keymeta_expires_at("y", use_cache = FALSE), t0)
  expect_equal(numhash(sto$envir_metadata), 0)

  # test assertions etc..
  expect_error(sto$get_keymeta_notes("y",namespace = "ns2"),
               "key 'y' ('ns2') not found",
               fixed = TRUE,
               class = "KeyError")


  expect_error(sto$get_keymeta_notes(c("x", "y")),
               "'key' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

  expect_error(sto$get_keymeta_notes("x", c("ns1", "ns2")),
               "'namespace' must have 1 elements (recieved 2)",
               fixed = TRUE,
               class = "error")

})


test_that("mget_keymeta_expires_at and mget_keymeta_notes", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE)

  # add some keys
  sto$set("x", 1)
  t0 <- Sys.time()+100
  sto$set("y", 1, expires_at = t0, notes = "name:Bob")
  expect_equal(numhash(sto$envir_metadata), 2)

  expect_equal(sto$mget_keymeta_notes("x", use_cache = TRUE), list(NA_character_))
  expect_equal(sto$mget_keymeta_notes(c("x","y"), use_cache = TRUE), list(NA_character_, "name:Bob"))
  expect_equal(sto$mget_keymeta_expires_at("x", use_cache = TRUE), list(as.POSIXct(NA)))
  expect_equal(sto$mget_keymeta_expires_at(c("x","y"), use_cache = TRUE), list(as.POSIXct(NA), t0))

  # with missing (nomatch)
  expect_equal(sto$mget_keymeta_notes("d", use_cache = TRUE), structure(list(NULL), missing = 1L))
  expect_equal(sto$mget_keymeta_notes(c("y", "d"), use_cache = TRUE), structure(list("name:Bob", NULL), missing = 2L))

  # use_cache = FALSE with empty cache (no cache writes)
  sto$flush_cache()
  expect_equal(numhash(sto$envir_metadata), 0)
  expect_equal(sto$mget_keymeta_notes("x", use_cache = FALSE), list(NA_character_))
  expect_equal(sto$mget_keymeta_notes(c("x","y"), use_cache = FALSE), list(NA_character_, "name:Bob"))
  expect_equal(sto$mget_keymeta_expires_at("x", use_cache = FALSE), list(as.POSIXct(NA)))
  expect_equal(sto$mget_keymeta_expires_at(c("x","y"), use_cache = FALSE), list(as.POSIXct(NA, tz = NULL), t0))
  expect_equal(numhash(sto$envir_metadata), 0)

  # with missing (nomatch)
  expect_equal(sto$mget_keymeta_notes("d", use_cache = FALSE, missing = "NO-VAL"), structure(list("NO-VAL"), missing = 1L))
  expect_equal(sto$mget_keymeta_notes(c("y", "d"), use_cache = FALSE, missing = "NO-VAL"), structure(list("name:Bob", "NO-VAL"), missing = 2L))

  # check key-namespace for incompatibility
  expect_error(sto$mget_keymeta_notes(c("x", "y", "z"), namespace = c("objects", "objects")),
               "Incompatible lengths for key and namespace",
               fixed = TRUE,
               class = "error")

})

test_that("mupdate_keymeta", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE)

  keys <- c("x", "y", "z")
  km <- paste(keys, "objects", sep = ":")

  # set some keys with no metadata (default)
  sto$mset(c("x", "y", "z"), c(1, 2, 3))

  # now set only notes
  expect_equal(sto$mupdate_keymeta(c("x", "y", "z"), notes =  c("xnote", "ynote", "znote")), km)
  # check metadata cache is filled up
  expect_equal(numhash(sto$envir_metadata), 3)
  expect_setequal(hashkeys(sto$envir_metadata), km)

  # test that notes were stored
  notes <- c("xnote", "ynote", "znote")
  trg <- vector("list", 3)
  for(i in seq_along(notes)) {
    trg[[i]] <- list(expires_at = as.POSIXct(NA), notes = notes[i])
  }

  expect_equal(sto$mget_keymeta(keys), trg, ignore_attr = TRUE)

  # note: use ignore_attr return val includes tzone attr with value ''
  expect_equal(sto$mget_keymeta(keys, use_cache = FALSE), trg, ignore_attr = TRUE)

  # continue with setting expires_at only
  expires_at <-  as.POSIXct(1:3, tz = NULL)
  expect_equal(sto$mupdate_keymeta(keys, expires_at = expires_at), km)

  # test datetimes were stored
  for(i in seq_along(expires_at)) {
    trg[[i]]$expires_at <- expires_at[i]
  }
  expect_equal(sto$mget_keymeta(keys), trg)
  expect_equal(sto$mget_keymeta(keys, use_cache = FALSE), trg)

  # check cache --

  # test we don't copy to cache
  expect_equal(sto$mupdate_keymeta("x", expires_at = as.POSIXct(100), use_cache = FALSE), km[1])
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
  expect_equal(sto$mupdate_keymeta(c("x", "z"), expires_at = rep(as.POSIXct(NA, tz = NULL), 2),
                                notes = c(NA_character_, NA_character_)),
               c("x:objects", "z:objects"))



  trg <- list(
    list(expires_at = structure(NA_real_, class = c("POSIXct", "POSIXt")), notes = NA_character_),
    list(expires_at = structure(NA_real_, class = c("POSIXct", "POSIXt")), notes = NA_character_)
  )
  expect_equal(sto$mget_keymeta(c("x", "z")), trg)

  expect_equal(sto$mget_keymeta(c("x", "z"), use_cache = FALSE), trg)


  # nothing to set
  expect_equal(sto$mupdate_keymeta(c("x", "y")), character())

  # test key-namespace not found
  expect_error(sto$mupdate_keymeta(c("x", "v"), notes = c(NA_character_, NA_character_)),
               "key 'v' ('objects') not found",
               fixed = TRUE,
               class = "error")

  # test key-namespace not found
  expect_error(sto$mupdate_keymeta(c("x1", "v"), c("obj1", "obj2"), notes = rep(NA_character_, 2)),
               "key 'x1,v' ('obj1,obj2') not found",
               fixed = TRUE,
               class = "error")

  # check key-namespace for incompatibility
  expect_error(sto$mupdate_keymeta(c("x", "y", "z"), namespace = c("objects", "objects")),
               "Incompatible lengths for key and namespace",
               fixed = TRUE,
               class = "error")

  expect_error(sto$mupdate_keymeta("x", notes = 1),
               "'notes' should be a character string, not numeric",
               fixed = TRUE,
               class = "error")


  expect_error(sto$mupdate_keymeta("x", expires_at = "a"),
               "'expires_at' should be a date-time object, not character",
               fixed = TRUE,
               class = "error")

  })


test_that("clear_keymeta", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE)

  sto$mset(key = c("x", "y", "z"),
           list(1, 2, 3),
           namespace = c("ns1", "ns2", "ns2"),
    notes =  c("xnote", "ynote", "znote"),
    expires_at = rep(Sys.time(), 3))


  # Clear single value ---
  expect_no_error(xres <-sto$clear_keymeta("x", "ns1"))
  expect_equal(xres, "x:ns1")

  trg <- list(expires_at = structure(NA_real_, class = c("POSIXct", "POSIXt"
  ), tzone = ""), notes = NA_character_)

  expect_equal(sto$get_keymeta("x", "ns1", use_cache = FALSE), trg)

  # Clear in bulk ---
  expect_no_error(xres <- sto$clear_keymeta(c("y", "z"), c("ns2", "ns2")))
  expect_equal(xres, c("y:ns2", "z:ns2"))


  expect_equal(sto$mget_keymeta(c("y", "z"), c("ns2", "ns2"), use_cache = FALSE),
  list(trg, trg), ignore_attr = TRUE)

})


test_that("clear_keymeta_async", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE, async = TRUE)

  sto$mset(key = c("x", "y", "z"),
           list(1, 2, 3),
           namespace = c("ns1", "ns2", "ns2"),
           notes =  c("xnote", "ynote", "znote"),
           expires_at = rep(Sys.time(), 3))


  # Clear single value ---
  expect_no_error(xres <-sto$clear_keymeta_async("x", "ns1"))
  expect_equal(xres$keyns, "x:ns1")

  trg <- list(expires_at = structure(NA_real_, class = c("POSIXct", "POSIXt"
  ), tzone = ""), notes = NA_character_)

  xres$mirai[] # block until resolves
  expect_equal(sto$get_keymeta("x", "ns1", use_cache = FALSE), trg)

  # Clear in bulk ---
  expect_no_error(xres <-sto$clear_keymeta_async(c("y", "z"), c("ns2", "ns2")))
  expect_equal(xres$keyns, c("y:ns2", "z:ns2"))

  xres$mirai[] # block until resolves
  expect_equal(sto$mget_keymeta(c("y", "z"), c("ns2", "ns2"), use_cache = FALSE),
               list(trg, trg), ignore_attr = TRUE)

})
