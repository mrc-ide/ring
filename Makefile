PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file

all: install

test:
	${RSCRIPT} -e 'library(methods); devtools::test()'

test_leaks:
	R -d 'valgrind --leak-check=full' -e 'devtools::test()'

roxygen:
	@mkdir -p man
	${RSCRIPT} -e "library(methods); devtools::document()"

install:
	R CMD INSTALL .

build:
	R CMD build .

check:
	_R_CHECK_CRAN_INCOMING_=FALSE make check_all

check_all: build
	R CMD check --as-cran --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

clean:
	rm -f src/*.o src/*.so src/*.dll

autodoc:
	${RSCRIPT} autodoc.R process

staticdocs:
	@mkdir -p inst/staticdocs
	${RSCRIPT} -e "library(methods); staticdocs::build_site()"
	rm -f vignettes/*.html
	@rmdir inst/staticdocs
website: staticdocs
	./update_web.sh

README.md: README.Rmd
	Rscript -e 'library(methods); devtools::load_all(); knitr::knit("README.Rmd")'
	sed -i.bak 's/[[:space:]]*$$//' $@
	rm -f $@.bak

vignettes/%.Rmd: vignettes/src/%.R
	${RSCRIPT} -e 'library(sowsear); sowsear("$<", output="$@")'
vignettes: vignettes/ring.Rmd
	${RSCRIPT} -e 'library(methods); devtools::build_vignettes()'

# No real targets!
.PHONY: all test document install vignettes
