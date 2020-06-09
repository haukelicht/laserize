# test_check_and_install_module.R

test_that("check_and_install_module() works as expected", {

  # if .py.venv is NULL, but module already available
  local_mock(py_module_available = function(...) TRUE, .env	= "reticulate")
  expect_true(laserize:::check_and_install_module("_test", .py.venv = NULL))
  expect_message(laserize:::check_and_install_module("_test", .py.venv = NULL), regexp = "^Checking if")
  expect_message(laserize:::check_and_install_module("_test", .py.venv = NULL), regexp = "module is already avaialable")

  # if .py.venv is NULL
  local_mock(py_module_available = function(...) FALSE, .env	= "reticulate")
  local_mock(py_install = function(module, ...) module, .env	= "reticulate")
  expect_true(laserize:::check_and_install_module("_test", .py.venv = NULL))
  expect_message(laserize:::check_and_install_module("_test", .py.venv = NULL), regexp = "^Checking if")

  # if .py.venv specified
  local_mock(virtualenv_install = function(venv, ...) venv, .env	= "reticulate")
  expect_true(laserize:::check_and_install_module("_test", .py.venv = "my-venv"))
  expect_message(laserize:::check_and_install_module("_test", .py.venv = "my-venv"), regexp = "^Checking if")

})

test_that("check_and_install_module() raises warning when installation fails", {


  local_mock(py_module_available = function(...) FALSE, .env	= "reticulate")
  local_mock(py_install = function(module, ...) stop(), .env	= "reticulate")
  local_mock(virtualenv_install = function(venv, ...) stop(), .env	= "reticulate")

  expect_warning(laserize:::check_and_install_module("_test", .py.venv = NULL), regexp = "^Error when trying to install")
  expect_warning(laserize:::check_and_install_module("_test", .py.venv = NULL), regexp = "^Error when trying to install")

  w_ <- getOption("warn")
  on.exit(options(warn = w_))
  options(warn = -1)
  expect_message(laserize:::check_and_install_module("_test", .py.venv = NULL), regexp = "^Checking if")
  expect_message(laserize:::check_and_install_module("_test", .py.venv = NULL), regexp = "^Checking if")

  expect_false(laserize:::check_and_install_module("_test", .py.venv = NULL))
  expect_false(laserize:::check_and_install_module("_test", .py.venv = NULL))

})
