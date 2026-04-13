
test_that("'StorrFragments' consolidate method with async", {

  uri <- file.path(withr::local_tempdir(), "sto1")
  sto <- storr_tiledb(uri, init = TRUE)

  sto$set("a", 1)
  sto$set("b", 2)
  sto$set("c", 3)

  fosto <- StorrFragments$new(uri)

  # 'all' consolidation  ---
  expect_no_error(m <- fosto$consolidate(what = "all", async = TRUE))
  expect_true(mirai::is_mirai(m))
  # wait to resolve
  expect_true(m[])
  expect_equal(fosto$frag_num(), 2)
  expect_equal(fosto$to_vacuum_num(), 6)

  # New commit
  sto$set("d", 4)
  fosto$reload_finfo()
  expect_equal(fosto$frag_num(), 4)
  expect_equal(fosto$to_vacuum_num(), 6)

  # 'keys' consolidation ---
  expect_no_error(m <- fosto$consolidate(what = "keys", async = TRUE))
  expect_true(mirai::is_mirai(m))
  # wait to resolve
  expect_true(m[])
  expect_equal(fosto$frag_num(), 3)
  # 2 frag merged, so vacuum num goes by two
  expect_equal(fosto$to_vacuum_num(), 8)

  # 'data' consolidation  ---
  expect_no_error(m <- fosto$consolidate(what = "data", async = TRUE))
  expect_true(mirai::is_mirai(m))
  # wait to resolve
  expect_true(m[])
  expect_equal(fosto$frag_num(), 2)

  # 2 frag merged, so vacuum num goes by two
  expect_equal(fosto$to_vacuum_num(), 10)
})


test_that("'StorrFragments' consolidate method with vacuum operation and async", {

  uri <- file.path(withr::local_tempdir(), "sto1")
  sto <- storr_tiledb(uri, init = TRUE)

  sto$set("a", 1)
  sto$set("b", 2)
  sto$set("c", 3)

  fosto <- StorrFragments$new(uri)

  # 'all' consolidation  ---
  expect_no_error(m <- fosto$consolidate(what = "all", vacuum = TRUE, async = TRUE))
  expect_true(mirai::is_mirai(m))
  # wait to resolve
  expect_true(m[])
  expect_equal(fosto$frag_num(), 2)
  expect_equal(fosto$to_vacuum_num(), 0)

  # New commit
  sto$set("d", 4)
  fosto$reload_finfo()
  expect_equal(fosto$frag_num(), 4)
  expect_equal(fosto$to_vacuum_num(), 0)

  # 'keys' consolidation ---
  expect_no_error(m <- fosto$consolidate(what = "keys", vacuum = TRUE, async = TRUE))
  expect_true(mirai::is_mirai(m))
  # wait to resolve
  expect_true(m[])
  expect_equal(fosto$frag_num(), 3)
  expect_equal(fosto$to_vacuum_num(), 0)

  # 'data' consolidation  ---
  expect_no_error(m <- fosto$consolidate(what = "data", vacuum = TRUE, async = TRUE))
  expect_true(mirai::is_mirai(m))
  # wait to resolve
  expect_true(m[])
  expect_equal(fosto$frag_num(), 2)
  expect_equal(fosto$to_vacuum_num(), 0)
})

test_that("'StorrFragments' vacuum method and async", {

  uri <- file.path(withr::local_tempdir(), "sto1")
  sto <- storr_tiledb(uri, init = TRUE)

  sto$set("a", 1)
  sto$set("b", 2)
  sto$set("c", 3)

  fosto <- StorrFragments$new(uri)

  # 'all' consolidation  ---
  expect_true(fosto$consolidate(what = "all"))
  expect_no_error(m <- fosto$vacuum(what = "all", async = TRUE))
  expect_true(mirai::is_mirai(m))
  # wait to resolve
  expect_true(m[])
  expect_equal(fosto$to_vacuum_num(), 0)

  # New commit
  sto$set("d", 4)
  fosto$reload_finfo()

  expect_true(fosto$consolidate(what = "all"))
  expect_equal(fosto$frag_num(), 2)
  expect_equal(fosto$to_vacuum_num(), 4)

  expect_no_error(m <- fosto$vacuum(what = "keys", async = TRUE))
  expect_true(mirai::is_mirai(m))
  # wait to resolve
  expect_true(m[])
  expect_equal(fosto$to_vacuum_num(), 2)

  expect_no_error(m <- fosto$vacuum(what = "data", async = TRUE))
  expect_true(mirai::is_mirai(m))
  # wait to resolve
  expect_true(m[])
  expect_equal(fosto$to_vacuum_num(), 0)

})

