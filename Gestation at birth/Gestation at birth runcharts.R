# a) data ----

max_plots_gestation_at_birth <- 4

gest_at_birthplotListNames = c("42 weeks and over (inclusive)", "between 32 and 36 weeks (inclusive)", 
                               "under 32 weeks", "under 37 weeks")

y_max_gestation <- reactiveVal(0) # initialise y_max_gestation

gest_at_birth_runchart_data <- reactive ({
  # selects data
  
  #req(input$period)
  
  data <- gest_at_birth_data %>%
    filter(hbname == Selected$HBName &
             period == "Q" &
             hbtype == Selected$HBType &
             measure_cat %in% gest_at_birthplotListNames
           ) %>% 
    mutate(num_label = paste0("Number of births that were at ", formatted_name, ": "),
           measure_label = paste0("Percentage of births that were at ", formatted_name, " (%)"),
    ) %>% 
    set_variable_labels(
      den = "Total number of births with a known gestation: ",
      median = " average to Oct-Dec 2019",
      extended = " projected average from Jan-Mar 2020") %>% 
      droplevels()
  
  new_labels <- unique(c(data$num_label, data$measure_label))
  
  new_max <- max(data$measure_value) # local maximum measure_value
  
  observeEvent(new_max, {   # update local maximum measure_value when Board changes
    y_max_gestation(new_max)
  }
  )
  
  data <- data %>% 
    split(.$measure_cat)
  
  for (i in seq_along(data)){
    var_label(data[[i]]$num) <- new_labels[[i]]
    var_label(data[[i]]$measure_value) <- new_labels[[i+4]]
  }
  
  for (i in seq_along(data)){
    data[[i]]$mytext <- paste0("Quarter: ",
                               data[[i]]$quarter_label,
                               "<br>",
                               var_label(data[[i]]$num),
                               prettyNum(data[[i]]$num, big.mark = ","), #data[[i]]$num,
                               "<br>",
                               var_label(data[[i]]$den),
                               prettyNum(data[[i]]$den, big.mark = ","), #data[[i]]$den,
                               "<br>",
                               "Percentage of births: ", # not MEASURE_LABEL - too long
                               format(data[[i]]$measure_value,
                                      digits = 2,
                                      nsmall = 2),
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

output$gest_at_birth_runcharts <- renderUI({
  tagList(
    fluidRow(
      column(4, 
             h4("under 32 weeks"),
             plotlyOutput(gest_at_birthplotListNames[[3]])
             ),
      column(1),
      column(7, 
             h4(HTML(paste0("32", tags$sup("+0"),
                            " to 36", tags$sup("+6"), " weeks"))),
             plotlyOutput(gest_at_birthplotListNames[[2]])
      )
      ), # fluidRow
    
    br(),
    
    fluidRow(
      column(4,
             h4( "under 37 weeks"),
             plotlyOutput(gest_at_birthplotListNames[[4]])
             ),
      column(1),
      column(4,
             h4(HTML(paste0("42", tags$sup("+0"), " weeks and over"))),
             plotlyOutput(gest_at_birthplotListNames[[1]])
             )
    ) # fluidRow
  )
})



for (i in 1:max_plots_gestation_at_birth) {
  # Need local so that each item gets its own number. Without it, the value
  # of i in the renderPlot() will be the same across all instances, because
  # of when the expression is evaluated.
  local({
    my_i <- i
    plotname <- gest_at_birthplotListNames[my_i]
    
    output[[plotname]] <- renderPlotly({
      creates_runcharts(plotdata = gest_at_birth_runchart_data()[[my_i]]) %>%
        layout(xaxis = list(#dtick = "6",
                            tickangle = -45),
               yaxis = list(range = c(0, y_max_gestation() * 1.05)), # forces y axis to same value on all charts
               legend = list(orientation = "v",
                             x = 1.2,
                             y = 0.5,
                             xref = "container",
                             xanchor = "left"))
    })
  })
}

# c) chart title ----

output$gest_at_birth_runcharts_title <- renderText({
  paste0("Board of ",
         str_to_sentence(input$organisation),
         ": ", 
         input$hbname
  )
  
})
