# Apgar5 download data

apgar5_download_data <- builds_download_data("APGAR5")

output$apgar5_download_data1 <- output$apgar5_download_data2 <- 
  
  downloadHandler(
  
  filename = function() {
      paste0(first(apgar5_download_data$indicator), "_", refresh_date, ".csv", sep = "")
    },
  
  content = function(file) {
    write.csv(apgar5_download_data, file, row.names = FALSE)
    }
  )




