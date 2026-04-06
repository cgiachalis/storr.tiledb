# *********************************************************
# Pre-compile and render vignettes
#
# * Tip: source helper routines first
#
# *********************************************************

source("./vignettes/src/helpers.R")

.precompile_vignette("getting_started", render = TRUE)
.precompile_vignette("introduction", render = TRUE)
.precompile_vignette("fag", render = TRUE)

# Pre-compile all vignettes
.precompile_all(render = TRUE)

# *********************************************************

# Build pkgdown articles (optional)
pkgdown::build_articles(lazy = TRUE)
