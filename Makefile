# h/t to @jimhester and @yihui for this parse block:
# https://github.com/yihui/knitr/blob/dc5ead7bcfc0ebd2789fe99c527c7d91afb3de4a/Makefile#L1-L4
# Note the portability change as suggested in the manual:
# https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Writing-portable-packages
PKGNAME = `sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION`
PKGVERS = `sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION`

.PHONY: paper

all: check

build:
	R CMD build .

check: build
	R CMD check --no-manual $(PKGNAME)_$(PKGVERS).tar.gz

install_deps:
	Rscript \
	-e 'if (!requireNamespace("remotes")) install.packages("remotes")' \
	-e 'remotes::install_deps(dependencies = TRUE)'

install: install_deps build
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

build_pkgdown:
	Rscript \
	-e 'devtools::load_all(path = here::here())' \
	-e 'devtools::document()' \
	-e 'pkgdown::build_site()' \

paper_clean: 
	rm -rf paper/*.html paper/*.tex paper/*.log paper/*.md paper/submission/*.md
paper:
	Rscript -e "rmarkdown::render(input = here::here('paper', 'paper.Rmd'))"
	Rscript -e "knitr::purl(here::here('paper', 'paper.Rmd'), documentation = 0)"
	mv paper.R paper/code.R
	Rscript -e "knitr::spin(hair = here::here('paper', 'code.R'))"
	mv code.html code.md paper/code.R paper/submission/
	mv paper/paper.pdf paper/submission/
	tar -cvzf maars_submission.tar.gz paper/submission
	mv maars_submission.tar.gz paper/submission/
	make paper_clean

clean:
	@rm -rf $(PKGNAME)_$(PKGVERS).tar.gz $(PKGNAME).Rcheck
