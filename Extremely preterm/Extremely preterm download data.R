# Extremely preterm download data

extremely_preterm_download_data <- extremely_preterm_data

output$extremely_preterm_download_data <- #output$extremely_preterm_download_data2 <- 
  
  downloadHandler(
  
  filename = function() {
      paste0(first(extremely_preterm_download_data$indicator), "_", refresh_date, ".csv", sep = "")
    },
  
  content = function(file) {
    write.csv(extremely_preterm_download_data, file, row.names = FALSE)
    }
  )




