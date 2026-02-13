# Dockerfile for School Attainment Policy Simulator Shiny App
# -----------------------------------------------------------
# Lightweight deployment using pre-extracted model coefficients
# (no lme4 model objects needed at runtime).
#
# Build: docker build -t school-attainment-app .
# Run:   docker run -p 3838:3838 school-attainment-app
# -----------------------------------------------------------

FROM rocker/shiny:4.4.1

# Install system dependencies for R packages (sf, etc.)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN install2.r --error --skipinstalled \
    tidyverse \
    shiny \
    bslib \
    leaflet \
    plotly \
    DT \
    sf \
    scales \
    here \
    janitor \
    purrr

# Remove default Shiny server apps
RUN rm -rf /srv/shiny-server/*

# Copy the app
COPY app/ /srv/shiny-server/app/

# Copy R helper scripts that the app sources
COPY R/helpers.R /srv/shiny-server/R/helpers.R
COPY R/predict_scenario.R /srv/shiny-server/R/predict_scenario.R
COPY R/predict_slim.R /srv/shiny-server/R/predict_slim.R

# Set the working directory so here::here() resolves correctly
WORKDIR /srv/shiny-server

# Create a .here file so here::here() finds the project root
RUN touch /srv/shiny-server/.here

# Configure Shiny Server to serve the app at the root
RUN echo 'run_as shiny; \n\
server { \n\
  listen 3838; \n\
  location / { \n\
    site_dir /srv/shiny-server/app; \n\
    log_dir /var/log/shiny-server; \n\
    directory_index on; \n\
  } \n\
}' > /etc/shiny-server/shiny-server.conf

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
