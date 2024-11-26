# Multi measure overview download data

this_excel_measure_name <- "multi_indicator_overview"

output$multi_indicator_download_data1 <- output$multi_indicator_download_data2 <- 
  
  download_excel_file(this_excel_measure_name)
