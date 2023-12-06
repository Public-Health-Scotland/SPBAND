# Multi measure overview download data

multi_indicator_download_data <- annual_dataframe %>% 
  arrange(key_measure_ref, hbtype, period, date, hbname) %>% 
  select(-c(key_measure_ref, measure_cat, MIN:plotlylabel)) %>% 
  janitor::remove_empty(., which = c("cols"), quiet = TRUE)

output$multi_indicator_download_data1 <- output$multi_indicator_download_data2 <- 
  
  downloadHandler(
  
  filename = function() {
      paste0("MULTI_INDICATOR_OVERVIEW_", refresh_date, ".csv", sep = "")
    },
  
  content = function(file) {
    write.csv(multi_indicator_download_data, file, row.names = FALSE)
    }
  )



