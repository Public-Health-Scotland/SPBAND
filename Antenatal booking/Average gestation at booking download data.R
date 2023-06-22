# Average gestation at booking download data

gest_at_booking_download_data <- builds_download_data("GESTATION AT BOOKING")

output$gest_at_booking_download_data1 <- output$gest_at_booking_download_data2 <- 
  
  downloadHandler(
  
  filename = function() {
      paste0(first(gest_at_booking_download_data$INDICATOR), "_", extract_date, ".csv", sep = "")
    },
  
  content = function(file) {
    write.csv(gest_at_booking_download_data, file, row.names = FALSE)
    }
  )




