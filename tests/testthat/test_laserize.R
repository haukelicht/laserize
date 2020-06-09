test_that("laserize() fails with erroneous input", {
    expect_error(laserize(x = character()), "must inherit from/be a data frame")
    expect_error(laserize(x = list()), "must inherit from/be a data frame")
    expect_error(laserize(x = data.frame(id = 1, text = "foo")), "must inherit from/be a data frame")
    expect_error(laserize(x = data.frame(id = 1, text = 2, lang = 3)), "Columns 'text' and 'lang' of `x` must be character or factor vectors.")
})

test_that("laserize() fails when Python not available", {

  local_mock(py_available = function(...) FALSE, .env = "reticulate")
  expect_error(laserize(x = data.frame(id = 1, text = "foo", lang = "en")), "Python is not available on your system.")
})

test_that("mocked laserize() fails when venv erroneous", {

  local_mock(dir.exists = function(...) FALSE, .env = "base")
  expect_error(laserize(x = data.frame(id = 1, text = "foo", lang = "en"), .py.venv = "foo"), "Path .+ does not exist")


  local_mock(dir.exists = function(...) TRUE, .env = "base")
  with_mock(
    use_virtualenv = function(...) stop()
    , .env = "reticulate"
    , expect_error(laserize(x = data.frame(id = 1, text = "foo", lang = "en"), .py.venv = "foo"), "not a valid Python virtual environment")
  )
})

test_that("mocked laserize() fails when module cannot be loaded", {

  local_mock(py_available = function(...) TRUE, .env = "reticulate")
  with_mock(
    import = function(...) stop()
    , .env = "reticulate"
    , expect_error(laserize(x = data.frame(id = 1, text = "foo", lang = "en"), .py.venv = NULL), "Loading .+ module failed")
  )

  local_mock(grepl = function(...) TRUE, .env = "reticulate")
  with_mock(
    import = function(...) stop("ModuleNotFoundError: No module named 'laserembeddings'")
    , .env = "reticulate"
    , wrn <- capture_warning(laserize(x = data.frame(id = 1, text = "foo", lang = "en"), .py.venv = NULL))
    , expect_match(wrn$message, "module is not installed.")
  )
})

test_that("mocked laserize() fails when intantiatiting Laser() fails", {

  local_mock(py_available = function(...) TRUE, .env = "reticulate")
  local_mock(import = function(...) list(Laser = function(...) stop()), .env = "reticulate")
  expect_error(laserize(x = data.frame(id = 1, text = "foo", lang = "en"), .py.venv = NULL), "Could not intantiate embedder.")
})

test_that("mocked laserize() fails when embed_sentences() fails", {

  local_mock(py_available = function(...) TRUE, .env = "reticulate")
  local_mock(import = function(...) list(Laser = function() list(embed_sentences = function(...) stop())), .env = "reticulate")
  expect_error(
    laserize(x = data.frame(id = 1, text = "foo", lang = "en"), .py.venv = NULL, check.languages = FALSE)
    , "sentences failed."
  )
})

test_that("mocked laserize() return", {

  local_mock(py_available = function(...) TRUE, .env = "reticulate")
  local_mock(import = function(...) list(Laser = function() list(embed_sentences = function(...) matrix(rnorm(20), nrow = 1) ) ), .env = "reticulate")
  res <- laserize(x = data.frame(id = 1, text = "foo", lang = "en"), .py.venv = NULL, check.languages = FALSE)
  expect_true(inherits(res, "list"))
  expect_length(res, 1)
  expect_named(res, "1")
  expect_length(res[[1]], 4)
  expect_named(res[[1]], c("id", "text", "lang", "e"))
  expect_length(res[[1]]$e, 20)
})

test_that("mocked laserize() simplifies return", {

  local_mock(py_available = function(...) TRUE, .env = "reticulate")
  local_mock(import = function(...) list(Laser = function() list(embed_sentences = function(...) matrix(rnorm(20), nrow = 1) ) ), .env = "reticulate")
  res <- laserize(x = data.frame(id = 1, text = "foo", lang = "en"), .py.venv = NULL, check.languages = FALSE, simplify = TRUE)
  expect_true(inherits(res, "matrix"))
  expect_equal(dim(res), c(1, 20))
  expect_true(rownames(res) == "1")
})

test_that("laserize() return", {

  if (!reticulate::py_module_available("laserembeddings")) {
    skip(message = "laserembeddings not available")
  } else {
    local_mock(py_available = function(...) TRUE, .env = "reticulate")
    res <- laserize(x = data.frame(id = 1, text = "hello world", lang = "en"), .py.venv = NULL, check.languages = FALSE)
    expect_true(inherits(res, "list"))
    expect_length(res, 1)
    expect_named(res, "1")
    expect_length(res[[1]], 4)
    expect_named(res[[1]], c("id", "text", "lang", "e"))
    expect_length(res[[1]]$e, 1024)
  }
})

