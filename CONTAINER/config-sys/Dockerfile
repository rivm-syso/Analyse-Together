######################################################################
## base image
FROM rivm-shinyapps-ct/rocker-shiny-verse:latest

LABEL maintainer="william.schuch@rivm.nl"
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
    && rm -rf /var/lib/apt/lists/*