# Multi indicator overview download data

multi_indicator_download_data <- annual_dataframe %>% 
  arrange(KEY_INDICATOR_REF, HBTYPE, PERIOD, DATE, HBNAME) %>% 
  select(-c(KEY_INDICATOR_REF, INDICATOR_CAT, MIN:PLOTLYLABEL)) %>% 
  janitor::remove_empty(., which = c("cols"), quiet = TRUE)

output$multi_indicator_download_data1 <- output$multi_indicator_download_data2 <- 
  
  downloadHandler(
  
  filename = function() {
      paste0("MULTI_INDICATOR_OVERVIEW_", extract_date, ".csv", sep = "")
    },
  
  content = function(file) {
    write.csv(multi_indicator_download_data, file, row.names = FALSE)
    }
  )



