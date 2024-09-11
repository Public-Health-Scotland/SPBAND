# a) data ----

terminations_runchart_data <- reactive({
  # selects data
  
  req(input$hbname)
  
  data <- terminations_data %>%
  filter(hbname == Selected$HBName &
           hbtype == Selected$HBType) %>%
    set_variable_labels(
    measure_value = "Number of terminations",
    pre_pandemic_median = " average to end Feb 2020",
    extended_pre_pandemic_median = " projected average from Mar 2020"
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

output$terminations_runcharts <- renderPlotly({

  if (input$hbname %in% island_names){
    
    plot_nodata(height = 450,
                text_nodata = paste("This chart is not shown due to small numbers.", "<br>",
                "Data for the Island Boards are included in the Scotland total.")
                )
    
    } else {

  creates_runcharts(plotdata = terminations_runchart_data(),
                  yaxislabel = "Number of terminations"
                  ) %>%
    layout(xaxis = list(range = 
                          range(terminations_runchart_data()$date) + c(months(-1), months(1))))
      
    }
  
})

# c) chart title ----
  
output$terminations_runcharts_title <- renderText({
  paste0("Board of ",
         str_to_sentence(input$organisation),
         ": ",
         input$hbname
  )
})

# d) download runchartdata

this_excel_measure_name <- "terminations"

output$terminations_download_data <-
  
  download_excel_file(this_excel_measure_name)
  
