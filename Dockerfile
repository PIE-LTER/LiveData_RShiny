FROM rocker/shiny:latest
RUN install2.r rsconnect dplyr lubridate ggplot2 plotly
WORKDIR /home/LiveData
COPY LiveData.R LiveData.R
COPY CR1000XSeries_IBYC_raw.dat CR1000XSeries_IBYC_raw.dat
COPY CR1000XSeries_MFM_raw.dat CR1000XSeries_MFM_raw.dat
COPY deploy.R deploy.R
CMD Rscript deploy.R
