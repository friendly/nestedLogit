
document:
	Rscript -e "devtools::document()"

documentrich:
	Rscript -e "Sys.setenv('pkgdown' = 'true');devtools::document()"

check: document
	Rscript -e "devtools::check()"

install: document
	Rscript -e "devtools::install()"

buildsite: document
	Rscript -e "pkgdown::build_site()"

# deploysite: document
#	Rscript -e "pkgdown::deploy_to_branch()"

buildpdf: document
	R CMD Rd2pdf .
