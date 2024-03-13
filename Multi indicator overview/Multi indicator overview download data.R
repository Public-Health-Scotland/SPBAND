# Multi measure overview download data

# multi_indicator_download_data <- annual_dataframe %>% 
#   arrange(dataset, hbtype, hbname, measure, period, date) %>% 
#   select(-c(MIO_measure_ref, measure_cat, MIN:plotlylabel)) %>% 
#   janitor::remove_empty(., which = c("cols"), quiet = TRUE)
# 
# output$multi_indicator_download_data1 <- output$multi_indicator_download_data2 <- 
#   
#   downloadHandler(
#   
#   filename = function() {
#       paste0("MULTI_INDICATOR_OVERVIEW_", refresh_date, ".csv", sep = "")
#     },
#   
#   content = function(file) {
#     write.csv(multi_indicator_download_data, file, row.names = FALSE)
#     }
#   )

this_excel_measure_name <- "multi_indicator_overview"

output$multi_indicator_download_data1 <- output$multi_indicator_download_data2 <- 
  
  download_excel_file(this_excel_measure_name)

