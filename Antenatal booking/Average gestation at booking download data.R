# Average gestation at booking download data

this_excel_measure_name <- "gestation_at_booking"

output$gest_at_booking_download_data1 <- output$gest_at_booking_download_data2 <-
  
  download_excel_file(this_excel_measure_name)
