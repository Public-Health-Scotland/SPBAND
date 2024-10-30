# Gestation by BAPM level of care download data

this_excel_measure_name <- "admissions_to_neocare"

output$gest_by_BAPM_LOC_download_data <- 
  
  download_excel_file(this_excel_measure_name)
