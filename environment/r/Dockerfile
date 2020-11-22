FROM rocker/geospatial:latest
WORKDIR /home/rstudio
RUN apt-get update \
    && apt-get -y install libgsl0-dev \
    && apt -y install libxml2-dev
# install R packages
RUN install2.r lubridate wru rnaturalearth epitools caret cowplot

EXPOSE 8787
