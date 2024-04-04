# Average gestation at termination download data

# gest_at_termination_download_data <- builds_download_data("GESTATION AT TERMINATION")
# 
# output$gest_at_termination_download_data1 <- output$gest_at_termination_download_data2 <- 
#   
#   downloadHandler(
#   
#   filename = function() {
#       paste0(first(gest_at_termination_download_data$measure), "_", refresh_date, ".csv", sep = "")
#     },
#   
#   content = function(file) {
#     write.csv(gest_at_termination_download_data, file, row.names = FALSE)
#     }
#   )

this_excel_measure_name <- "gestation_at_termination"

output$gest_at_termination_download_data1 <- output$gest_at_termination_download_data2 <- 
  
  download_excel_file(this_excel_measure_name)


