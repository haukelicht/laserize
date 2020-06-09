#' Setup LASER
#'
#' @description LASER setup. Interactive function to create a
#'     Python virtual environment (optional) ,
#'     installs \bold{laserembeddings} (if not already avaialable),
#'     and downloads all models.
#'
#' @param .py.venv path to a Python virtual environment (see \code{\link[reticulate]{virtualenv_create}})
#' @param ... additional arguments passed to \code{\link[reticulate]{py_install}}
#'     or \code{\link[reticulate]{virtualenv_install}} (if \code{.py.venv} is not NULL)
#' @param .required.models a character vector specifying the model data required to use
#'     the \bold{laserembeddings} Python module.
#'     DO NOT change this argument, unless you know what you're doing.
#'
#' @return logical, indicating whether or not setting up the \bold{laserembeddings} Python module was successful
#'
#' @import reticulate
#' @export
setup_laser <- function(
  .py.venv = Sys.getenv("RETICULATE_PYTHON_ENV", NA_character_)
  , ...
  , .required.models = c("93langs.fcodes", "93langs.fvocab", "bilstm.93langs.2018-12-26.pt")
) {

  tmp_ <- is.null(.py.venv) || is.na(.py.venv) | is.logical(.py.venv) && !.py.venv
  stopifnot("`.py.venv` must be unit-length character string, NA, NULL or FALSE." = is.character(.py.venv) && length(.py.venv) == 1 | tmp_)

  if (!tmp_) {
    if (dir.exists(.py.venv) && file.exists(file.path(.py.venv, "bin", "activate"))) {
      message("Using Python virtual environment on path ", .py.venv)
    } else if (dir.exists(.py.venv)) {
      stop(.py.venv, " is an existing directory but not a Python virtual environment.", call. = FALSE)
    } else if (file.exists(.py.venv)) {
      stop(.py.venv, " is an existing file and not a Python virtual environment.", call. = FALSE)
    } else if (dir.exists(dirname(.py.venv))) {
      warning(.py.venv, " (`.py.venv`) is not a valid Python virtual environment.", call. = FALSE, immediate. = TRUE)
      crt_venv <- askYesNo(
        msg = paste("Do you want to create a new Python virtual environment named", sQuote(basename(.py.venv)), "on path", dirname(.py.venv), "?")
        , default = FALSE
      )

      if (!crt_venv)
        return(FALSE)

      # creating virtual environment
      message("Creating virtual environment ", sQuote(basename(.py.venv)), " ...")
      virtualenv_create(.py.venv)

    } else {
      stop("Path ", .py.venv, " unknown.", call. = FALSE)
    }

    # setting Python virtual environment
    message("Setting virtual environment ", sQuote(basename(.py.venv)), " ...")
    use_virtualenv(.py.venv)

    Sys.setenv("RETICULATE_PYTHON_ENV" = .py.venv)
    Sys.setenv("RETICULATE_PYTHON" = virtualenv_python(.py.venv))
  } else {
    if (!py_available()) {
      warning("Python is not available on your system. Install Pythonor run `setup_laser()` again with a valid path passed to argument .py.venv.", call. = FALSE)
      return(FALSE)
    }
    capture.output(py_config())
  }

  if (is.na(.py.venv))
    .py.venv <- NULL

  meth <- ifelse(is.null(.py.venv), "auto", "virtualenv")
  message("Installing \033[1mlaserembeddings\033[22m module ...")

  # Check if laserembeddings already available
  if(!check_and_install_module("laserembeddings", .py.venv, method = meth)) return(FALSE)
  if (askYesNo("Do you also want to ensure Japanese language support?", default = FALSE))
    if(!check_and_install_module("laserembeddings[ja]", .py.venv, method = meth)) return(FALSE)
  if (askYesNo("Do you also want to ensure Chinese language support?", default = FALSE))
    if(!check_and_install_module("laserembeddings[zh]", .py.venv, method = meth)) return(FALSE)

  # check if data is installed
  site_package <- list.files(file.path(.py.venv, "lib"), pattern = "laserembeddings", recursive = TRUE, include.dirs = TRUE, full.names = TRUE)
  site_package <- site_package[basename(site_package) == "laserembeddings"]
  if (
    length(site_package) == 0
    || !dir.exists(file.path(site_package, "data"))
    || !all(.required.models %in% list.files(file.path(site_package, "data")))
  ) {
    message("Downloading \033[1mlaserembeddings\033[22m models ...")

    data_dir <- ifelse(length(site_package) == 0, "", file.path(site_package, "data"))
    if (length(site_package) != 0 && !dir.exists(data_dir))
      dir.create(data_dir)

    dflt_python <- reticulate:::.globals$py_config$python
    if (is.null(dflt_python)) dflt_python <- "python"
    cmd <- sprintf(
      "%s -m laserembeddings download-models %s"
      , Sys.getenv("RETICULATE_PYTHON", dflt_python)
      , data_dir
    )

    downloaded <- tryCatch(system(cmd), error = function(err) err)

    if (inherits(downloaded, "error")) {
      warning("Downloading \033[1mlaserembeddings\033[22m models failed. Error message reads ", downloaded$message, call. = FALSE)
      return(FALSE)
    }

  }

  message("Setting up \033[1mLASER\033[22m was successful!")
  return(TRUE)
}
