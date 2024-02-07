# Authenticate
setAccountInfo(name='pie-lter', 
               token='C485E1BA4F0E356AF3D8430767343B01', 
               secret='NleW5hW3Qscjm2wi3dKTAumivKVjoA7rqbzZ8sV9')
# Deploy
deployApp(appFiles = c("PIE_live_data_app.R", 
                       "CR1000XSeries_IBYC_raw.dat", 
                       "CR1000XSeries_MFM_raw.dat"))
