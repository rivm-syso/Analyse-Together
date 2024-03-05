######################################################################
## base image
FROM rocker/shiny-verse:4.3.1

ENV TZ Europe/Amsterdam
RUN cat /etc/os-release

######################################################################
# Create layers

# Adding system level libs etc.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libudunits2-dev \
    libgdal-dev \
    gdal-bin \
    libgeos-dev \
    libproj-dev \
    libssl-dev \
    rsync \
    wget \
    && rm -rf /var/lib/apt/lists/*

# install R pkgs
RUN install2.r --error --skipinstalled --ncpus -1 \
     tidyverse \
     lubridate \
     shiny \
     shinycssloaders \
     shinyWidgets \
     RSQLite \
     pool \
     leaflet \
     leaflet.extras \
     sp \
     sf \
     DT \
     latex2exp \
     openair \
     logger \
     remotes \
     plyr \
     shinyjs \
     shiny.i18n \
     dbplyr \
     here \
     shinyalert \
     datawizard \
     shinybusy \
     && rm -rf /tmp/downloaded_packages

# install some more R pkgs (in a new layer)
# RUN install2.r --error --skipinstalled --ncpus -1 \
#      && rm -rf /tmp/downloaded_packages

# Create folder
# copy app

RUN mkdir /app
WORKDIR /app
COPY . .

## install remotes packages
RUN R -e "remotes::install_github('rivm-syso/samanapir', ref = 'main')"  && \
 R -e "remotes::install_github('rivm-syso/ATdatabase', ref = 'main', build_opts ='')"


## expose app
EXPOSE 3838

# launch app
CMD ["R", "-e", "shiny::runApp('.' ,host = '0.0.0.0',port=3838)"]
