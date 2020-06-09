
test_langs <- c(
  "de" # German
  , "en" # English
  , "csb" # Kashubian was used to train LASER, but is unknown to ISO 639
  , "unk" # 'unknown' language
)

test_that("LASER language check works with default langugage support table", {

  expect_warning(check_laser_language(test_langs))
  res <- check_laser_language(test_langs,.verbose = FALSE)
  # type
  expect_type(res, "list")
  # length
  expect_length(res, 4)
  # lengths
  expect_equivalent(lengths(res), rep(1L, 4))
  # names
  expect_equal(names(res), test_langs)
  # values
  expect_true(all(unlist(res)[1:2]))
  expect_true(all(!unlist(res)[3:4]))

  expect_false(any(vapply(lapply(res[3:4], attr, "laser.lang"), is.null, TRUE)))
  expect_true(all(vapply(lapply(res[3:4], attr, "laser.lang"), is.logical, FALSE)))
  tmp_ <- vapply(res[3:4], attr, FALSE, which = "laser.lang")
  expect_equivalent(tmp_, c(T, F))

  expect_false(any(vapply(lapply(res[3:4], attr, "problem"), is.null, TRUE)))
  expect_true(all(vapply(lapply(res[3:4], attr, "problem"), is.character, FALSE)))
  tmp_ <- vapply(res[3:4], attr, NA_character_, which = "problem")
  lapply(tmp_, expect_match, regexp = "is not a valid ISO 639-1")
})

test_that("LASER language check works with default table", {

  expect_error(check_laser_language(test_langs, .supported.langs = NULL))
  expect_error(check_laser_language(test_langs, .supported.langs = list()))
  expect_error(check_laser_language(test_langs, .supported.langs = data.frame()))
  expect_error(check_laser_language(test_langs, .supported.langs = data.frame(code = 1, name = 2, iso2c = 3, iso3c = 4)))

})
