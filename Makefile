PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)


manual :
	cd ..;\
	R CMD Rd2pdf --no-preview $(PKGNAME)

install :
	Rscript -e "remotes::install_local(build = TRUE, build_vignettes = TRUE, upgrade = FALSE)"


sitedev :
	Rscript \
	   -e "pkgdown::build_reference(preview = FALSE)" \
     -e "pkgdown::build_news(preview = FALSE)" \
     -e "pkgdown::build_home(preview = FALSE)" \
     -e "pkgdown::build_articles(preview = FALSE)"


siteprod :
	Rscript \
	   -e "pkgdown::build_reference(preview = FALSE, lazy = FALSE)" \
     -e "pkgdown::build_news(preview = FALSE)" \
     -e "pkgdown::build_home(preview = FALSE)" \
     -e "pkgdown::build_articles(preview = FALSE, lazy = FALSE)"


check :
	Rscript -e "rcmdcheck::rcmdcheck(env=c('_R_CHECK_SYSTEM_CLOCK_'=0, 'NOT_CRAN'=TRUE, '_R_CHECK_CRAN_INCOMING_'=FALSE))"

test :
	Rscript -e "devtools::test()"

coverage :
	Rscript \
	   -e "source(paste0(getwd(), '/data-coverage/test-coverage.R'))"


# help
# -------------------------------------------------------------------
define HELP

Usage: make <rule>

Rules:
  manual              Build R manual
  install             Install R package
  test                Run tests
  check               Run R CMD check
  coverage            Run test coverage
  sitedev             Build dev website
  siteprod            Build prod website

Examples:

  Install Package:  make install


endef
export HELP

.PHONY: help
help:
	@printf "$${HELP}"
