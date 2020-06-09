
.onAttach <- function(libname, pkgname) {
  startup_msg <- "\033[1mLASERize\033[22m: Obtaining LASER embedding text representations in R!\n"

  python_lib <- Sys.getenv("RETICULATE_PYTHON")
  if (python_lib == "") {
    note <- paste(
      "There has no default been set specifying which Python distribution to use on your system"
      , "(see https://rstudio.github.io/reticulate/articles/python_packages.html)."
    )
  } else {
    note <- paste("Using Python distribution on path", python_lib)
  }
  startup_msg <- paste0(startup_msg, "Note: ", note)

  packageStartupMessage(startup_msg)
}

