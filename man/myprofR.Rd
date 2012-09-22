\docType{package}
\name{myprofR}
\alias{myprofR}
\alias{myprofR-package}
\alias{package-myprofR}
\title{Cleans and extracts information from files produced by Rprof}
\description{
  Cleans and extracts information from files produced by
  Rprof
}
\examples{
\dontrun{
## Use examples from productplots for profiling...
if(!("Prodplot.txt" \%in\% dir())){
    Rprof("Prodplot.txt")
    library(productplots)
    if (require("ggplot2")) {
        prodplot(happy, ~ happy, "hbar")
        prodplot(happy, ~ happy, "hspine")

        prodplot(happy, ~ sex + happy, c("vspine", "hbar"))
        prodplot(happy, ~ sex + happy, stacked())

        ## The levels argument can be used to extract a given level of the plot
        prodplot(happy, ~ sex + happy, stacked(), level = 1)
        prodplot(happy, ~ sex + happy, stacked(), level = 2)
    }
    Rprof(NULL)
}
}
}

