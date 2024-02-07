FROM rocker/shiny:latest
RUN install2.r rsconnect dplyr lubridate ggplot2
WORKDIR /home/LiveData
COPY PIE_live_data_app.R PIE_live_data_app.R
COPY CR1000XSeries_IBYC_raw.dat CR1000XSeries_IBYC_raw.dat
COPY CR1000XSeries_MFM_raw.dat CR1000XSeries_MFM_raw.dat
COPY deploy.R deploy.R
CMD Rscript deploy.R
