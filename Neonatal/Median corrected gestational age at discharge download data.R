# Median corrected gestational age at discharge download data

this_excel_measure_name <- "median_corrected_gestational_age"

output$corrected_gestational_age_download_data <- 
  
  download_excel_file(this_excel_measure_name)
