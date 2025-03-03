# OpenDataApp Dockerfile
# Version: 1.0.0
# Date: 2023-10-05
# Description: Dockerfile for the OpenDataApp Shiny application

FROM rocker/shiny:latest

# Install dependencies
RUN R -e "install.packages(c('shiny', 'ggplot2', 'dplyr'))"

# Copy application files
WORKDIR /srv/shiny-server
COPY . .

# Expose Shiny app port
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
