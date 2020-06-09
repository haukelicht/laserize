test_that("setup_laser() fails with erroneous .py.venv input", {
    expect_error(setup_laser(.py.venv = TRUE))
    expect_error(setup_laser(.py.venv = integer()))
    expect_error(setup_laser(.py.venv = double()))
    expect_error(setup_laser(.py.venv = character(2L)))
})

test_that("mocked setup_laser() fails when .py.venv is existing dir but not a Python venv", {
    local_mock(dir.exists = function(...) TRUE, .env = "base")
    local_mock(file.exists = function(...) FALSE, .env = "base")
    expect_error(setup_laser(.py.venv = "path/to/venv"), regexp = "path/to/venv is an existing directory but not a Python virtual environment")
})

test_that("mocked setup_laser() fails when .py.venv is existing file and not a Python venv", {
    local_mock(dir.exists = function(...) FALSE, .env = "base")
    local_mock(file.exists = function(...) TRUE, .env = "base")
    expect_error(setup_laser(.py.venv = "path/to/venv"), regexp = "path/to/venv is an existing file and not a Python virtual environment")
})

test_that("mocked setup_laser() fails when .py.venv is existing dir but not a Python venv", {
    local_mock(dir.exists = function(...) FALSE, .env = "base")
    local_mock(file.exists = function(...) FALSE, .env = "base")
    expect_error(setup_laser(.py.venv = "path/to/venv"), regexp = "Path path/to/venv unknown.")
})

test_that("mocked setup_laser() returns FALSE and raises warning when Python not available", {
    local_mock(py_available = function(...) FALSE, .env = "reticulate")

    expect_warning(res <- setup_laser(.py.venv = NULL), "Python is not available on your system")
    expect_false(res)
})

test_that("mocked setup_laser() works if .py.venv is (fake) Python venv", {

    local_mock(dir.exists = function(...) TRUE, .env = "base")
    local_mock(file.exists = function(...) TRUE, .env = "base")
    local_mock(use_virtualenv = function(...) TRUE, .env = "reticulate")
    local_mock(askYesNo = function(...) TRUE, .env = "utils")
    local_mock(check_and_install_module = function(...) TRUE, .env = "laserize")
    local_mock(dir.exists = function(...) TRUE, .env = "base")
    local_mock(list.files = function(...) unlist(as.list(formals(setup_laser)$.required.models)[-1]), .env = "base")
    local_mock(system = function(...) TRUE, .env = "base")

    msgs <- capture_messages(setup_laser(.py.venv = "path/to/venv"))
    # lapply(msgs, cat)
    expect_match(msgs[1], "Using Python virtual environment")
    expect_match(msgs[2], "Setting virtual environment")
    expect_equal(Sys.getenv("RETICULATE_PYTHON_ENV"), "path/to/venv")
    expect_match(msgs[3], "^Installing")
    expect_match(msgs[4], "^Downloading")
    expect_match(msgs[5], "^Setting up .+ success")
    #
    # expect_true(setup_laser(.py.venv = "path/to/venv"))
})

test_that("mocked setup_laser() works if .py.venv needs to be created", {

    testthat::skip_if_not_installed("mockery")

    local_mock(file.exists = function(...) FALSE, .env = "base")
    local_mock(askYesNo = function(...) TRUE, .env = "utils")
    local_mock(virtualenv_create = function(venv, ...) venv, .env = "reticulate")
    local_mock(use_virtualenv = function(...) TRUE, .env = "reticulate")
    local_mock(check_and_install_module = function(...) TRUE, .env = "laserize")
    local_mock(system = function(...) TRUE, .env = "base")

    m <- mockery::mock(FALSE, FALSE, TRUE, cycle = TRUE) # from {mockery}
    with_mock(
        dir.exists = m
        , .env = "base"
        # test expectations
        , expect_warning(res <- setup_laser(.py.venv = "path/to/venv"), "not a valid Python virtual environment")
        , expect_true(res)
        , msgs <- capture_messages(suppressWarnings(setup_laser(.py.venv = "path/to/venv")))
        , expect_match(msgs[1], "Creating virtual environment")
        , expect_match(msgs[2], "Setting virtual environment")
        , expect_equal(Sys.getenv("RETICULATE_PYTHON_ENV"), "path/to/venv")
        , expect_match(msgs[3], "^Installing")
    )
})

test_that("setup_laser() works if .py.venv is (real) Python venv", {

    local_mock(check_and_install_module = function(...) TRUE, .env = "laserize")
    local_mock(askYesNo = function(...) FALSE, .env = "utils")
    local_mock(system = function(...) TRUE, .env = "base")

    venv <- file.path("~", ".virtualenvs", "laserize")
    if (!dir.exists(venv)) {
        skip()
    } else {
        msgs <- capture_messages(setup_laser(.py.venv = venv))
        # lapply(msgs, cat)
        expect_match(msgs[1], "Using Python virtual environment")
        expect_match(msgs[2], "Setting virtual environment")
        expect_equal(Sys.getenv("RETICULATE_PYTHON_ENV"), venv)
        expect_match(msgs[3], "^Installing")
        expect_match(msgs[4], "^Setting up .+ success")

        expect_true(setup_laser(.py.venv = file.path("~", ".virtualenvs", "laserize")))
    }
})

