# Gestation at birth download data

this_excel_measure_name <- "gestation_at_birth"

output$gest_at_birth_download_data1 <- output$gest_at_birth_download_data2 <- 
  
  download_excel_file(this_excel_measure_name)