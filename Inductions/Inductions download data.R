# Inductions download data

# inductions_download_data <- builds_download_data("INDUCTIONS")
# 
# output$inductions_download_data1 <- output$inductions_download_data2 <- 
#   
#   downloadHandler(
#   
#   filename = function() {
#       paste0(first(inductions_download_data$measure), "_", refresh_date, ".csv", sep = "")
#     },
#   
#   content = function(file) {
#     write.csv(inductions_download_data, file, row.names = FALSE)
#     }
#   )

this_excel_measure_name <- "inductions"

output$inductions_download_data1 <- output$inductions_download_data2 <- 
  
  download_excel_file(this_excel_measure_name)