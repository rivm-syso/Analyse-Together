######################################################################
# setup: installs packages etc.
######################################################################
#
# The Analyse Together application needs several packages to run
# properly. Some packages are from CRAN but also development packages
# from github are needed.
#
# IMPORTANT: this scripts install packages, so it alters your R setup

# CRAN packages:
packages <- c("remotes", "openair", "magrittr", "dplyr", "tidyr")
install.packages(setdiff(packages, rownames(installed.packages())))

# Github packages
remotes::install_github("jspijker/datafile")
remotes::install_github("rivm-syso/samanapir")


