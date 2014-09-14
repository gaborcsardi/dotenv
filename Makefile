
all: README.md

README.md: vignettes/README.Rmd
	Rscript -e "library(knitr); knit('$<', output = '$@', quiet = TRUE)"


