# OpenDataApp Dockerfile
# Version: 1.0.0
# Date: 2023-10-05
# Description: Dockerfile for the OpenDataApp Shiny application

# Base R Shiny image
FROM rocker/shiny

# Make a directory in the container
RUN mkdir /home/shiny-app

# Install R dependencies
RUN R -e "install.packages(c('gapminder', 'leaflet', 'dplyr', 'sf', 'wesanderson', 'shinycssloaders', 'shinyjs', 'shinyBS', 'httr', 'jsonlite', 'tidyverse', 'dplyr', 'ggplot2', 'lubridate', 'extrafont', 'stringr', 'RColorBrewer', 'plotly', 'DT', 'Cairo', 'writexl'))"

# Copy the Shiny app code
COPY OpenDataApp/ /home/OpenDataApp/

# Expose the application port
EXPOSE 8180

# Run the R Shiny app
# CMD Rscript /home/OpenDataApp/weatherapp_rainonly.R
CMD Rscript /home/OpenDataApp/app.R
