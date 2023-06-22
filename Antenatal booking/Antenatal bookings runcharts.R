# a) data ----

bookings_runchart_data <- reactive({
  # selects data
  
  #req(input$HBNAME)
  
  data <- bookings_data %>%
  filter(HBNAME == Selected$HBName &
           HBTYPE == Selected$HBType) %>%
    set_variable_labels(
    MEASURE = "Number of pregnancies booked",
    MEDIAN = " average to end Feb 2020",
    EXTENDED = " projected average from Mar 2020"
  ) %>% 
  mutate(mytext = paste0("Month: ", 
                         format(DATE, "%b %Y"),
                         "<br>",
                         var_label(MEASURE),
                         ": ",
                         MEASURE),
         orig_trend = FALSE, # to prevent this line being plotted
         orig_shift = FALSE # ditto
         )
  
  if (is.null(data()))
  {
  return()
  }

  else {
    data
  }
})

# b) chart ----

output$bookings_runcharts <- renderPlotly({
  
creates_runcharts(plotdata = bookings_runchart_data(),
                  yaxislabel = "Number of pregnancies booked"
                  )
})

# c) chart title ----
  
output$bookings_runcharts_title <- renderText({
  paste0("Board of ",
         str_to_sentence(input$organisation),
         ": ",
         input$hbname
  )
})

# d) download data

bookings_download <- builds_download_data("BOOKINGS")

output$bookings_download_data <- downloadHandler(

  filename = function() {
      paste0(first(bookings_download$INDICATOR), "_", extract_date, ".csv", sep = "")
    },

  content = function(file) {
    write.csv(bookings_download, file, row.names = FALSE)
    }
  )
  
