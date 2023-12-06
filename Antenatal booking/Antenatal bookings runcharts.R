# a) data ----

bookings_runchart_data <- reactive({
  # selects data
  
  #req(input$hbname)
  
  data <- bookings_data %>%
  filter(hbname == Selected$HBName &
           hbtype == Selected$HBType) %>%
    set_variable_labels(
    measure_value = "Number of pregnancies booked",
    median = " average to end Feb 2020",
    extended = " projected average from Mar 2020"
  ) %>% 
  mutate(mytext = paste0("Month: ", 
                         format(date, "%b %Y"),
                         "<br>",
                         var_label(measure_value),
                         ": ",
                         prettyNum(measure_value, big.mark = ",")),
         trend = NA, # to prevent this line being plotted
         shift = NA # ditto
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
      paste0(first(bookings_download$measure), "_", refresh_date, ".csv", sep = "")
    },

  content = function(file) {
    write.csv(bookings_download, file, row.names = FALSE)
    }
  )
  
