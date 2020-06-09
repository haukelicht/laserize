#' LASER languages.
#'
#' A dataset containing the internal codes, names, ISO 639-1 and 639-3 codes (if available)
#'      of the languages used to train the LASER models.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{code}{The \bold{Tatoeba} codes of languages used to train LASER models (see \url{https://tatoeba.org/eng/stats/sentences_by_language})}
#'   \item{name}{Language name}
#'   \item{iso2c}{ISO 639-1 (two-character) code of language (if exists)}
#'   \item{iso3c}{ISO 639-3 (two-character) code of language (if exists)}
#' }
#'
#' @note Tatoeba language codes scraped from{https://github.com/facebookresearch/LASER/tree/master/data/tatoeba/v1}. Codes matched to ISO codes based on names and this table \url{https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes}
#' @source \url{}
"laser.languages"
