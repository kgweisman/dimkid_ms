# install all required packages

# install packages via CRAN
package_list <- c(
  "knitr",
  "tidyverse",
  "GPArotation",
  "psych",
  "lme4",
  "mgcv",
  "kableExtra",
  "parameters",
  "cowplot",
  "bookdown"
)

install.packages(package_list)

# install one package from github
devtools::install_github("langcog/langcog-package")

