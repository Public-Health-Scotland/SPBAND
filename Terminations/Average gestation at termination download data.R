# Average gestation at termination download data

this_excel_measure_name <- "gestation_at_termination"

output$gest_at_termination_download_data1 <- output$gest_at_termination_download_data2 <- 
  
  download_excel_file(this_excel_measure_name)