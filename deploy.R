library(rsconnect)

# a function to stop the script when one of the variables cannot be found
# and to strip quotation marks from the secrets when you supplied them
error_on_missing_name <- function(name) {
  var <- Sys.getenv(name, unset = NA)
  if(is.na(var)) {
    stop(paste0("cannot find ", name, " !"), call. = FALSE)
  }
  gsub("\"", "", var)
}

# Authenticate
setAccountInfo(name='pie-lter', 
               token='C485E1BA4F0E356AF3D8430767343B01', 
               secret='NleW5hW3Qscjm2wi3dKTAumivKVjoA7rqbzZ8sV9')
# Deploy
deployApp(appFiles = c("PIE_live_data_app.R", 
                       "CR1000XSeries_IBYC_raw.dat", 
                       "CR1000XSeries_MFM_raw.dat"))
