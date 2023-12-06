# Tears download data

tears_download_data <- builds_download_data("TEARS")

output$tears_download_data1 <- output$tears_download_data2 <- 
  
  downloadHandler(
  
  filename = function() {
      paste0(first(tears_download_data$measure), "_", refresh_date, ".csv", sep = "")
    },
  
  content = function(file) {
    write.csv(tears_download_data, file, row.names = FALSE)
    }
  )




