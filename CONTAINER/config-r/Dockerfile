######################################################################
## base image
FROM rivm-shinyapps-ct/rocker-shiny-verse:latest

RUN R -e "installed.packages()[,c(3:4)]"

## adding R-packages

RUN R -e "install.packages('remotes', force=FALSE, repos='https://mirror.lyrahosting.com/CRAN/')"

RUN R -e "remotes::install_github("jspijker/datafile", build_opts ="")"  && \
 R -e "remotes::install_github("jspijker/samanapir", ref = "Issue_2")"  && \
# R -e "remotes::install_github("rivm-syso/samanapir", ref = "Issue_2")"  && \
 R -e "remotes::install_github("rivm-syso/ATdatabase", ref = "develop", build_opts ="")"

RUN R -e "remove.packages('remotes')"

RUN R -e "install.packages(c('datafile','samanapir','ATdatabase','shinycssloaders','shinyWidgets','shinyjs','shiny.i18n','RSQLite','pool','leaflet','leaflet.extras','sp','sf','DT','plotly','latex2exp','openair','lubridate'), force=FALSE, repos='https://mirror.lyrahosting.com/CRAN/')"

RUN R -e "installed.packages()[,c(3:4)]"