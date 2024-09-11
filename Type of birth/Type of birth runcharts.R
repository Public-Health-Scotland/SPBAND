# a) data ----

max_plots_type_of_birth <- 5

type_of_birthplotListNames = c("Caesarean births", "Assisted births",
                               "Planned caesarean births", "Unplanned caesarean births", 
                               "Spontaneous vaginal births")

type_of_birth_runchart_data <- reactive({
  # selects data
  
  req(input$hbname)
  
  data <- type_of_birth_data %>%
    filter(hbname == Selected$HBName &
             period == "Q" &
             hbtype == Selected$HBType) %>% 
    mutate(num_label = if_else(measure_cat == "all caesarean births",
                               paste0("Number of caesarean births: "),
                               paste0("Number of ", measure_cat, ": ")),
           measure_label = paste0("Percentage of births that were ", measure_cat, " (%)"),
    ) %>% 
    set_variable_labels(
      den = "Total number of births: ",
      pre_pandemic_median = " average to Oct-Dec 2019",
      extended_pre_pandemic_median = " projected average from Jan-Mar 2020")
  
  new_labels = unique(c(data$num_label, data$measure_label))
  
  data <- data %>% 
    split(.$measure_cat)
  
  for (i in seq_along(data)){
    var_label(data[[i]]$num) <- new_labels[[i]]
    var_label(data[[i]]$measure_value) <- new_labels[[i+5]]
  }
  
  for (i in seq_along(data)){
    data[[i]]$mytext <- paste0("Quarter: ",
                               data[[i]]$quarter_label,
                               "<br>",
                               var_label(data[[i]]$num), 
                               prettyNum(data[[i]]$num, big.mark = ","), # data[[i]]$num, 
                               "<br>",
                               var_label(data[[i]]$den),
                               prettyNum(data[[i]]$den, big.mark = ","), # data[[i]]$den,
                               "<br>",
                               "Percentage of births: ", # not MEASURE_LABEL - too long
                               format(data[[i]]$measure_value,
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

# Puts an asterisk next to subtitles when NHS Borders is selected # temporary till issue fixed

planned_title <- reactive({
  
  if_else(Selected$HBName == "NHS Borders",
          "planned caesarean births*",
          "planned caesarean births")
})

unplanned_title <- reactive({
  
  if_else(Selected$HBName == "NHS Borders",
          "unplanned caesarean births*",
          "unplanned caesarean births")
})

# Insert the right number of plot output objects into the web page

output$type_of_birth_runcharts <- renderUI({
  
  tagList(
    fluidRow(
      column(4, 
             h4("caesarean births"),
             plotlyOutput(type_of_birthplotListNames[1])
      ),
      column(4, 
             h4(planned_title()),
             plotlyOutput(type_of_birthplotListNames[3])
      ),
      column(4, 
             h4(unplanned_title()),
             plotlyOutput(type_of_birthplotListNames[5])
      )
    ), # fluidRow
    
    br(),
    
    fluidRow(
      column(4,
             h4( "assisted vaginal births (includes forceps, ventouse and vaginal breech births)"),
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
  
  if_else(input$hbname == "NHS Borders",
          paste0("Board of ",
                 str_to_sentence(input$organisation),
                 ": ",
                 input$hbname,
                 "*"),
          paste0("Board of ",
                 str_to_sentence(input$organisation),
                 ": ",
                 input$hbname)
  )
})

