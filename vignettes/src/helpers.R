
# *********************************************************
#  Helper routines to pre-compile vignettes
# *********************************************************

.precompile_vignette <- function(x, render = TRUE) {
  input <- paste0("vignettes/src/_", x, ".Rmd")
  output <- paste0("vignettes/", x, ".Rmd")
  knitr::knit(input, output, envir = new.env())

  if (render)
    .render_vignette(x)
}

.precompile_all <- function(render = TRUE) {
  sapply(.list_vignettes_names(), .precompile_vignette, render = render)
  invisible(TRUE)
}

.render_vignette <- function(x) {

  dir <- tempfile("html")
  dir.create(dir, showWarnings = FALSE)
  output_file <- file.path(dir, paste0(x, ".html"))
  litedown:::mark(paste0("vignettes/", x, ".Rmd"),
                  output = output_file)
  rstudioapi::viewer(output_file)
  invisible(TRUE)
}

.list_vignettes_names <- function(pattern = ".Rmd"){
  f <- list.files("vignettes/src/", pattern = pattern)
  f <- sub("_", "", tools::file_path_sans_ext(f))
  f
}
