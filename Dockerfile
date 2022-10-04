# Image based on Ubuntu LTS (focal), with current R version
FROM rocker/r-ver:4.2.1


# Install system libraries

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libudunits2-dev \
    libgdal-dev \
    gdal-bin \
    libgeos-dev \
    libproj-dev \
    libssl-dev \
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
     plotly \
     latex2exp \
     openair \
     logger \
     && rm -rf /tmp/downloaded_packages

# install some more R pkgs (in a new layer)
RUN install2.r --error --skipinstalled --ncpus -1 \
     remotes \
     && rm -rf /tmp/downloaded_packages

# Create folder 
# copy app

RUN mkdir /app
WORKDIR /app
COPY . .

## expose app
EXPOSE 3838

# launch app
CMD ["R", "-e", "shiny::runApp('.' ,host = '0.0.0.0',port=3838)"]
