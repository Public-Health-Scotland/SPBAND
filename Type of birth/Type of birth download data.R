# Type of birth download data

# type_of_birth_download_data <- builds_download_data("TYPE OF BIRTH")
# 
# output$type_of_birth_download_data1 <- output$type_of_birth_download_data2 <- 
#   
#   downloadHandler(
#   
#   filename = function() {
#       paste0(first(type_of_birth_download_data$measure), "_", refresh_date, ".csv", sep = "")
#     },
#   
#   content = function(file) {
#     write.csv(type_of_birth_download_data, file, row.names = FALSE)
#     }
#   )

this_excel_measure_name <- "type_of_birth"

output$type_of_birth_download_data1 <- output$type_of_birth_download_data2 <- 
  
  download_excel_file(this_excel_measure_name)


