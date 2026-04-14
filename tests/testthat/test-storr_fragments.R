
test_that("'storr_fragments'", {

  uri <- file.path(withr::local_tempdir(), "sto1")
  sto <- storr_tiledb(uri, init = TRUE)

  expect_s3_class(storr_fragments(uri), "StorrFragments")

  # With config, e.g., encryption
  uri <- file.path(withr::local_tempdir(), "sto2")
  key <- "5b643a5e173c27d76b3f2af01fcb327b"
  config <- tiledb::tiledb_config()
  config["sm.encryption_type"] <- "AES_256_GCM";
  config["sm.encryption_key"] <- key
  ctx <- R6.tiledb::new_context(config) # not cached, only within driver
  sto <- storr_tiledb(uri, init = TRUE, context = ctx)
  expect_error(storr_fragments(uri))
})

test_that("'storr_consolidate'", {

  uri <- file.path(withr::local_tempdir(), "sto1")
  sto <- storr_tiledb(uri, init = TRUE)

  sto$set("a", 1)
  sto$set("b", 2)

  expect_true(storr_consolidate(uri, what = "all", vacuum = FALSE, async = FALSE))
  expect_true(storr_consolidate(uri, what = "all", vacuum = TRUE, async = FALSE))
  expect_true(storr_consolidate(uri, what = "keys", vacuum = TRUE, async = TRUE)[])
  storr_fragments(uri)->sfo
  expect_equal(sfo$frag_num(), 2)
  expect_equal(sfo$to_vacuum_num(), 0)
})


test_that("'storr_vacuum'", {

  uri <- file.path(withr::local_tempdir(), "sto1")
  sto <- storr_tiledb(uri, init = TRUE)

  sto$set("a", 1)
  sto$set("b", 2)
  sfo <- storr_fragments(uri)
  sfo$consolidate()

  expect_true(storr_vacuum(uri, what = "all", async = FALSE))

  sfo$reload_finfo()
  expect_equal(sfo$frag_num(), 2)
  expect_equal(sfo$to_vacuum_num(), 0)

  sto$set("c", 3)
  sfo$consolidate()

  expect_true(storr_vacuum(uri, what = "all", async = TRUE)[])

  sfo$reload_finfo()
  expect_equal(sfo$frag_num(), 2)
  expect_equal(sfo$to_vacuum_num(), 0)
})
