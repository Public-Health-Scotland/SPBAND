# a) data ----

terminations_runchart_data <- reactive({
  # selects data
  
  req(input$hbname)
  
  data <- terminations_data %>%
  filter(HBNAME == Selected$HBName &
           HBTYPE == Selected$HBType) %>%
    set_variable_labels(
    MEASURE = "Number of terminations",
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
         orig_shift = FALSE, # ditto
         HBNAME2 = factor(HBNAME2, 
                          levels = c("NHS Ayrshire & Arran", "NHS Borders",
                                     "NHS Dumfries & Galloway", "NHS Fife", "NHS Forth Valley",
                                     "NHS Grampian", "NHS Greater Glasgow & Clyde", "NHS Highland",
                                     "NHS Lanarkshire", "NHS Lothian", "NHS Tayside", "Scotland")))
  
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
                          range(terminations_runchart_data()$DATE) + c(months(-1), months(1))))
      
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

terminations_download <- builds_download_data("TERMINATIONS")


output$terminations_download_data <- downloadHandler(
  
  filename = function() {
      paste0(first(terminations_download$INDICATOR), "_", extract_date, ".csv", sep = "")
    },
  
  content = function(file) {
    write.csv(terminations_download, file, row.names = FALSE)
    }
  )
  
