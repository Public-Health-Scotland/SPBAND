# Apgar5 download data

# apgar5_download_data <- builds_download_data("APGAR5")
# 
# output$apgar5_download_data1 <- output$apgar5_download_data2 <- 
#   
#   downloadHandler(
#   
#   filename = function() {
#       paste0(first(apgar5_download_data$measure), "_", refresh_date, ".csv", sep = "")
#     },
#   
#   content = function(file) {
#     write.csv(apgar5_download_data, file, row.names = FALSE)
#     }
#   )

this_excel_measure_name <- "apgar5"

output$apgar5_download_data1 <- output$apgar5_download_data2 <- 
  
  download_excel_file(this_excel_measure_name)