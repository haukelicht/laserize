#' Obtain LASER embedding vectors for sentences/texts
#'
#' @description Function provides an interface to \code{embed_sentences} provided
#'      through the Python \bold{laserembeddings} module (\url{https://pypi.org/project/laserembeddings/})
#'
#' @param x a \code{data.frame}, \code{\link[data.table]{data.table}}, or \code{\link[tibble]{tibble}} with columns
#'      \describe{
#'          \item{id}{character, factor, or numeric; (unique) identifier of sentences/texts}
#'          \item{text}{character; the actual text to be embeded}
#'          \item{lang}{character; ISO 639-1 (two-character) code of source language of \code{text}}
#'      }
#'
#' @param check.languages logical specifying whether or not to check if all languages in \code{x}
#'      (i.e., \code{unique(x$lang)}) are available through LASER.
#'      Defaults to \code{FALSE}.
#'
#' @param .py.venv character (optional), specifying path to a Python virtual environment to be used.
#'      Defaults to \code{NULL} in which case the user's/systems default Python environment is used
#'      (see \code{py_config()}).
#'
#' @param simplify logical determining the return value (see below).
#'      \itemize{
#'        \item{
#'          If \code{FALSE} (the default), sentence embeddings are returned as a
#'          named (nested) \code{list} object.
#'          List element names correspond to \code{x$id}.
#'          Each list element, in turn, is a \code{list} object with the following elements:
#'          \describe{
#'            \item{id}{The sentence/text id (see \code{x})}
#'            \item{text}{The sentence/text (see \code{x})}
#'            \item{lang}{The sentence/text language code (see \code{x})}
#'            \item{e}{The sentence/text LASER embedding vector (type \code{double} with 1024 elements, one for each dimension of the embedding space)}
#'          }
#'        }
#'        \item{
#'          If \code{TRUE}, sentence/text embeddings are returned as a
#'          \code{matrix} with \emph{n} rows (i.e., number of sentences in \code{x}, \code{nrow(x)})
#'          and 1024 columns (the dimensionality of the embedding space), and matrix rows
#'          named like \code{x$id}.
#'        }
#'      }
#' @param .init.python logical. Passed to \code{\link[reticulate]{py_available}}'s \code{initialize}
#'
#' @section Checking language codes:
#'     Note that setting \code{check.languages = TRUE} triggers an interactive prompt message
#'     when there is a text in the input data frame with a language that is not supported by LASER.
#'     To check this manually, pass \code{x$lang} to \code{\link{check_laser_language}}.
#'
#'
#'
#' @section Out-of-memory problems:
#'     Note that for each sentence/text (row) in \code{x}, \code{laserize} obtains a numeric vector of length 1024.
#'     Given that for each row you thus obtain an object of (1024 times 8 bytes) 8.192 kilobytes (KB),
#'     obtaining many sentence/text representations at once can quickly cause out-of-memory problems.
#'
#'     This can be avoided by simply splitting the input data frame passed to argument \code{x} in fixed-size batches (say 100'000 rows),
#'     and, while iterating over batches, by either appending the output into a \code{\link[data.table]{data.table}} or by writing it to disk.
#'
#'
#' @note Using \code{laserize} presupposes that you have both Python 3 and the \code{laserembeddings} Python module installed.
#'     If you are not sure whether or not this applies to your system, run \code{\link{setup_laser}}.
#'
#' @return a \code{matrix} or a \code{list}, depending on value passed to argument \code{simplify}.
#' @import reticulate
#' @export
#'
#' @examples \dontrun{
#'   test_df <- tibble::tribble(
#'     ~id, ~text, ~lang,
#'     001, "Hallo Welt", "de",
#'     002, "Auf wiedersehen", "de",
#'     003, "Hello world", "en",
#'     004, "XXGWRXYYFGEG", "unkown",
#'   )
#'
#'   res <- laserize(test_df)
#'   str(res, 1) # name list with four elements
#'   str(res[[1]], 1) # list with elements 'id', 'text', 'lang', and 'e'
#'
#'   # simplified output
#'   res <- laserize(test_df, simplify = TRUE)
#'   is.matrix(res) # a matrix
#'   # rows as many as sentences in 'test_df',
#'   # columns as many as embedding dimensions
#'   dim(res)
#'
#'   # check sentence similarities
#'   cosine_sim <- function(x, y) sum(x*y)/sqrt(sum(x**2)*sum(y**2))
#'   # representations of greetings in German and English are very similar
#'   cosine_sim(x = res[1, ], y = res[3, ])
#'   # representations of German greeting and goodbye are somewhat dissimilar
#'   cosine_sim(x = res[1, ], y = res[2, ])
#' }
laserize <- function(x, check.languages = TRUE, .py.venv = Sys.getenv("RETICULATE_PYTHON_ENV"), simplify = FALSE, .init.python = TRUE) {

  if (is.null(check.languages) || is.na(check.languages))
    check.languages <- TRUE
  if (!is.null(.py.venv) && (is.na(.py.venv) || is.logical(.py.venv) && !.py.venv || is.character(.py.venv) && .py.venv == ""))
    .py.venv <- NULL
  if (is.null(simplify) || is.na(simplify))
    simplify <- FALSE

  # check inputs
  stopifnot(
    TRUE
    # check data frame input
    , "`x` must inherit from/be a data frame with columns 'id', 'text', 'lang'." = inherits(x, "data.frame")
    , "`x` must inherit from/be a data frame with columns 'id', 'text', 'lang'." = all(c("id", "text", "lang") %in% names(x))
    , "Columns 'text' and 'lang' of `x` must be character or factor vectors." = all(vapply(x[c("text", "lang")], class, NA_character_) %in% c("character", "factor"))
  )

  # check python setup
  if (!py_available(initialize = .init.python))
    stop("Cannot embed sentences. Python is not available on your system.", call. = FALSE)

  # check virtual environment
  if (!is.null(.py.venv)) {
    if (!dir.exists(.py.venv))
      stop("Cannot embed sentences. Path ", .py.venv, " (`.py.venv`) does not exist.", call. = FALSE)

    venv <- tryCatch(use_virtualenv(.py.venv), error = function(err) err)
    if (inherits(venv, "error"))
      stop("Cannot embed sentences. Path ", .py.venv, " (`.py.venv`) is not a valid Python virtual environment.", call. = FALSE)
  }

  # load module
  message("Loading Python ", sQuote("laserembeddings"), " module ...")
  laserembeddings <- tryCatch(import("laserembeddings"), error = function(err) err)

  if (inherits(laserembeddings, "error")) {
    if (grepl("^ModuleNotFoundError:", laserembeddings$message)) {
      warning(
        sQuote("laserembeddings"), " Python module is not installed. "
        , 'Run: py_install("laserembeddings"', ifelse(is.null(.py.venv), ")", paste0(', envname = "', .py.venv, '")'))
        , call. = FALSE
      )
    }

    stop("Cannot embed sentences. Loading ", sQuote("laserembeddings"), " module failed. Error message reads ", dQuote(laserembeddings$message), call. = FALSE)
  }

  # instantiate embedder
  message("Instantiating embedder ...")
  embedder <- tryCatch(laserembeddings$Laser(), error = function(err) err)

  if (inherits(embedder, "error"))
    stop("Cannot embed sentences. Could not intantiate embedder. Error message reads ", dQuote(embedder$message))

  # check languages ?
  if (check.languages) {
    test_langs <- x[!duplicated(x$lang), "lang"]
    message("Trying embedding sentence in ", length(test_langs), " different languages ...")

    langs_test <- tryCatch(check_laser_language(test_langs), error = function(err) err)

    if (inherits(langs_test, "error"))
      stop("Cannot embed sentences. Error when checking language codes. Error message reads ", langs_test$message)

    laser_lang <- vapply(langs_test[!unlist(langs_test)], attr, FALSE, which = "laser.lang")
    not_laser_lang <- names(laser_lang[which(!laser_lang)])
    n_sentences <- lengths(vapply(not_laser_lang, match, table = x$lang, NA_integer_))

    unknown_langs <- lapply(sprintf("%s is unknown to LASER (%d sentences affected).", sQuote(not_laser_lang), n_sentences), warning, call. = FALSE, immediate. = TRUE)
    cont <- askYesNo(msg = paste(length(unknown_langs), "languages are unknown to LASER. Do you want to embed all sentences nevertheless?"), default = TRUE)
    if (!cont)
      stop("Cannot embed sentences. ", length(unknown_langs), " languages are unknown to LASER: ", paste(sprintf("%s (%d sentences affected)", sQuote(not_laser_lang), n_sentences), collapse = ", "))

    test_res <- tryCatch(
      embedder$embed_sentences(x[!duplicated(x$lang), "text"], x[!duplicated(x$lang), "lang"])
      , error = function(err) err
    )

    if (inherits(test_res, "error"))
      stop("Cannot embed sentences. Embedding sentences failed. Error message reads ", test_res$message, call. = FALSE)
  }

  message("Embedding sentences ...")
  res <- list()
  res[[1]] <- tryCatch(embedder$embed_sentences(sentences = x$text, lang = x$lang), error = function(err) err)

  if (inherits(res[[1]], "error"))
    stop("Cannot embed sentences. Embedding sentences failed. Error message reads ", res$message, call. = FALSE)

  if (simplify) {
    rownames(res[[1]]) <- x$id
    return(res[[1]])
  }

  setNames(lapply(1:nrow(x), function(i) list(id = x$id, text = x$text, lang = x$lang, e = as.vector(res[[1]][i, ]))), x$id)
}

