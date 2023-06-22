# a) data ----

max_plots_type_of_birth <- 5

type_of_birthplotListNames = c("Caesarean births", "Assisted births",
                       "Planned caesarean births", "Unplanned caesarean births", 
                       "Spontaneous vaginal births")

type_of_birth_runchart_data <- reactive({
  # selects data
  
  #req(input$period)
  
  data <- type_of_birth_data %>%
    filter(HBNAME == Selected$HBName &
             PERIOD == "Q" &
             HBTYPE == Selected$HBType) %>% 
    mutate(NUM_label = if_else(INDICATOR_CAT == "all caesarean births",
                               paste0("Number of births that were caesarean births: "),
                               paste0("Number of births that were ", INDICATOR_CAT, ": ")),
           MEASURE_label = paste0("Percentage of births that were ", INDICATOR_CAT, " (%)"),
           #DATE = QUARTER_LABEL
    ) %>% 
    set_variable_labels(
      DEN = "Total number of births: ",
      MEDIAN = " average to Oct-Dec 2019",
      EXTENDED = " projected average from Jan-Mar 2020")

  new_labels = unique(c(data$NUM_label, data$MEASURE_label))
  
  data <- data %>% 
    split(.$INDICATOR_CAT)
  
  for (i in seq_along(data)){
    var_label(data[[i]]$NUM) <- new_labels[[i]]
    var_label(data[[i]]$MEASURE) <- new_labels[[i+5]]
  }
  
  for (i in seq_along(data)){
    data[[i]]$mytext <- paste0("Quarter: ",
                               data[[i]]$QUARTER_LABEL,
                               "<br>",
                               var_label(data[[i]]$NUM), data[[i]]$NUM, "<br>",
                               var_label(data[[i]]$DEN), data[[i]]$DEN, "<br>",
                               "Percentage of births: ", # not MEASURE_LABEL - too long
                               format(data[[i]]$MEASURE,
                                      digits = 1,
                                      nsmall = 1),
                               "%")
  }
  
  if (is.null(data)) {
    return()
  } else {
    data
  }
})

# b) chart ----

# Insert the right number of plot output objects into the web page
output$type_of_birth_runcharts <- renderUI({
  tagList(
    fluidRow(
      column(4, 
             h4("caesarean births"),
             plotlyOutput(type_of_birthplotListNames[1])
             ),
      column(4, 
             h4("planned caesarean births"),
             plotlyOutput(type_of_birthplotListNames[3])
             ),
      column(4, 
             h4("unplanned caesarean births"),
             plotlyOutput(type_of_birthplotListNames[5])
             )
      ), # fluidRow
    
    br(),
    
    fluidRow(
      column(4,
             h4( "assisted births (includes forceps, ventouse and vaginal breech births)"),
             plotlyOutput(type_of_birthplotListNames[2])
             ),
      column(7,
             h4( "spontaneous vaginal births"),
             br(),
             plotlyOutput(type_of_birthplotListNames[4])
             )
    ) # fluidRow
  )
})

for (i in 1:max_plots_type_of_birth) {
  # Need local so that each item gets its own number. Without it, the value
  # of i in the renderPlot() will be the same across all instances, because
  # of when the expression is evaluated.
  local({
    my_i <- i
    plotname <- type_of_birthplotListNames[my_i]
    
    output[[plotname]] <- renderPlotly({
      creates_runcharts(plotdata = type_of_birth_runchart_data()[[my_i]]) %>%
        layout(xaxis = list(#dtick = "6",
                            tickangle = -45),
               yaxis = list(range = c(0, y_max_type_of_birth * 1.05)), # forces y axis to same value on all charts
               legend = list(orientation = "v",
                             x = 1.2,
                             y = 0.5, 
                             xref = "container",
                             xanchor = "left"))
    })
  })
}

# c) chart title ----

output$type_of_birth_runcharts_title <- renderText({
  paste0("Board of ",
         str_to_sentence(input$organisation),
         ": ", 
         input$hbname
  )
  
})
