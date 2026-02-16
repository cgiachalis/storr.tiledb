test_that("Test 'CAS' object", {

  uri <- file.path(withr::local_tempdir(), "test-cas")
  expect_no_error(cas <- CAS$new(uri))

  expect_s3_class(cas, "CAS")
  expect_false(cas$exists())
  expect_equal(cas$mode, "CLOSED")
  expect_error(cas$hash_algorithm, label = "CAS object does not exist")
  expect_error(cas$size, label = "CAS object does not exist")
  expect_error(cas$members_instantiated, label = "CAS object does not exist")

})


test_that("Test 'CAS' basic methods", {

  # NOTE: The following methods:
  # 'query_keys', 'filter_keys', 'update_keys'
  #  are tested in "TileDBDriver" unit tests

  uri <- file.path(withr::local_tempdir(), "test-cas")
  cas <- CAS$new(uri)

  # Invalid hash algo
  expect_error(cas$create(algo = "nope"))

  # Create CAS
  expect_invisible(cas$create())
  expect_true(cas$exists())
  expect_equal(cas$mode, "WRITE")

  # Check members
  expect_equal(cas$count_members(), 2)

  mdf <- cas$get_members_df()[, 1:2]
  expected <- as.data.frame(list(
    name = c("tbl_keys", "tbl_data"),
    type = c("ARRAY", "ARRAY")
  ))

  expect_equal(mdf, expected)

  # Check active fields
  expect_error(cas$hash_algorithm <- "invalid")
  expect_equal(cas$hash_algorithm, "md5")

  # Set new hash algo
  expect_no_error(cas$hash_algorithm <- "blake3")
  expect_equal(cas$hash_algorithm, "blake3")

  expect_error(cas$size <- "immutable")
  expect_s3_class(cas$size, "vfs_size")

  expect_error(cas$members_instantiated <- "immutable")

  # Open with member instantiation
  cas$close()

  expect_false(cas$members_instantiated)

  # A bit defensive (but checking nevertheless)
  expect_null(cas$members$tbl_keys$object)
  expect_null(cas$members$tbl_data$object)

  expect_no_error(cas$open("READ", instantiate = TRUE))

  expect_true(cas$members_instantiated)

  # A bit defensive (but checking nevertheless)
  expect_true(!is.null(cas$members$tbl_keys$object))
  expect_true(!is.null(cas$members$tbl_data$object))

  # Destroy CAS
  expect_no_error(cas$destroy())
  expect_false(cas$exists())

  })

