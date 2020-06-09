#' Check and install Python module
#'
#' @param module a unit-length character string
#' @param .py.venv character (optional), specifying path to a Python virtual environment to be used.
#'      Defaults to \code{NULL} in which case the user's/systems default Python environment is used
#'      (see \code{py_config()}).
#'
#' @param ... Additional parameters passed to \code{\link[reticulate]{py_install}}
#'      or \code{\link[reticulate]{virtualenv_install}} (if \code{.py.venv} is not NULL)
#'
#' @return a logical indicating whether module exists (was or has been installed) after check.
check_and_install_module <- function(module, .py.venv, ...) {

  message("Checking if \033[1m", module, "\033[22m module already available ...")
  if (is.null(.py.venv) && py_module_available(module)) {
    message("\033[1m", module, "\033[22m module is already avaialable ...")
    return(TRUE)
  } else if (is.null(.py.venv)) {
    installed <- tryCatch(
      py_install(module, venv = .py.venv, ...)
      , error = function(err) err
    )
  } else {
    site_package <- list.files(file.path(.py.venv, "lib"), pattern = module, recursive = TRUE, include.dirs = TRUE, full.names = FALSE)
    module_installed <- module %in% basename(site_package)
    installed <- tryCatch(
      virtualenv_install(.py.venv, packages = module, ignore_installed = !module_installed, ...)
      , error = function(err) err
    )
  }

  if (inherits(installed, "error")) {
    warning("Error when trying to install \033[1m", module, "\033[22m module. Error message reads ", sQuote(installed$message), call. = FALSE)
    return(FALSE)
  }

  return(TRUE)

}


#' Check if input language code(s) is/are supported by LASER
#'
#' @param lang a character vector specifying language codes (ideally ISO 639-1).
#' @param .supported.langs a \code{data.frame} or \code{\link[tibble]{tibble}} with
#'      columns \describe{
#'          \item{code}{language code used by LASER}
#'          \item{name}{language name}
#'          \item{iso2c}{ISO 639-1 language code}
#'          \item{iso3c}{ISO 639-3 language code}
#'      }
#'
#' @param .verbose logical indicating whether or not to raise a warning for unknown/not supported languages.
#'
#' @return a named list of logical. If an element is \code{FALSE}, it has additional attributes
#'     \describe{
#'          \item{lang}{inputed language code}
#'          \item{laser.lang}{logical indicating whether \code{lang} is known to LASER}
#'          \item{problem}{The warning message raised if executed with \code{.verbose = TRUE}}
#'     }
#'
#' @note \code{check_laser_language} is vectorized over \code{lang}.
#'
#' @export
#'
#' @examples \dontrun{
#'   test_langs <- c(
#'     "de" # German
#'     , "en" # English
#'     , "csb" # Kashubian was used to train LASER, but is unknown to ISO 639
#'     , "unk" # 'unknown' language
#'   )
#'   res <- check_laser_language(test_langs)
#'   unlist(res) # as (named) logical vector
#' }
check_laser_language <- Vectorize(function(
  lang
  , .supported.langs = laser.languages
  , .verbose = TRUE
) {

  stopifnot(
    "`lang` must be a character vector" = length(lang) == 1
    , "`lang` must be a character vector" = is.character(lang)
    , "object passed to argument `.suppoted.lang` must be a data frame or tibble" = inherits(.supported.langs, "data.frame")
    , "`.suppoted.lang` must be a data frame or tibble with columns ‘code’, ‘name’, ‘iso2c’ and ‘iso3c’" = all(c("code", "name", "iso2c", "iso3c") %in% names(.supported.langs))
    , "`.suppoted.lang` must be a data frame or tibble with character columns only" = all("character" == vapply(.supported.langs, typeof, ""))
  )

  if (lang %in% na.omit(.supported.langs$iso2c))
    return(TRUE)

  out <- FALSE
  attr(out, "lang") <- lang

  wrn_msg <- paste(sQuote(lang), "is not a valid ISO 639-1 (two-character) code.")

  if (lang %in% na.omit(.supported.langs$code)){
    wrn_msg <- paste(wrn_msg, "LASER has been trained on it, however.")
    attr(out, "laser.lang") <- TRUE
  } else {
    attr(out, "laser.lang") <- FALSE
  }

  attr(out, "problem") <- wrn_msg

  if (.verbose) warning(wrn_msg, call. = FALSE, immediate. = FALSE)

  return(out)
}, USE.NAMES = TRUE, vectorize.args = "lang", SIMPLIFY = FALSE)

