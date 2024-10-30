# a) data ----

max_plots_gestation_by_BAPM_LOC <- 3

BAPM_LOC_plotListNames = c("intensive care", "high dependency care", "special care") # placeholder for plots

y_max_gestation_by_BAPM_LOC <- reactiveVal(0) # initialise y_max_gestation_by_BAPM

Selected$Nicename <- paste0("late pre-term (34", "<sup>+0</sup>",
                            " to 36", "<sup>+6</sup>", " weeks gestation)")

Selected$BAPM_LOC_Subgroup_cat <- "between 34 and 36 weeks (inclusive)"

observeEvent(input$BAPM_LOC_subgroup_cat, Selected$Nicename <- case_when(
  input$BAPM_LOC_subgroup_cat == "between 34 and 36 weeks (inclusive)" ~ paste0("late pre-term (34", "<sup>+0</sup>",
                                                         " to 36", "<sup>+6</sup>", " weeks gestation)"),
  input$BAPM_LOC_subgroup_cat == "between 37 and 42 weeks (inclusive)" ~ paste0("term and post-term (37", "<sup>+0</sup>",
                                                         " to 42", "<sup>+6</sup>", " weeks gestation)")
  )
)

gest_by_BAPM_LOC_runchart_data <- reactive({
  # selects data
  
  req(input$BAPM_LOC_subgroup_cat)
  
  data <- gest_by_BAPM_LOC_data |> 
    filter(!is.na(measure_cat2) & subgroup_cat == Selected$BAPM_LOC_Subgroup_cat) |> 
    mutate(num_label = paste0("Number of ", short_formatted_name, " babies", "<br>", "admitted to ", measure_cat, ": "),
           den_label = paste0("Total number of ", short_formatted_name, " babies: "), 
           measure_label = paste0("Percentage of ", short_formatted_name, " babies admitted to ", measure_cat, " (%)"),
    ) |>  
    set_variable_labels(
      pre_pandemic_median = " average to Oct-Dec 2019",
      extended_pre_pandemic_median = " projected average from Jan-Mar 2020"
      #post_pandemic_median = paste0("average from Jul 2022", "<br>", "to end Jun 2024"),
      #extended_post_pandemic_median = "projected average from Jul 2024"
      ) 
  
  new_labels = c(unique(c(data$num_label, data$measure_label)), rep(unique(data$den_label), 3))
  
  new_max <- max(data$measure_value) # local maximum measure_value
  
  observeEvent(new_max, {   # update local maximum measure_value when subgroup_cat changes
    y_max_gestation_by_BAPM_LOC(new_max)
  }
  )
  
  data <- data %>% 
    split(.$measure_cat2)
  
  for (i in seq_along(data)){
    var_label(data[[i]]$num) <- new_labels[[i]]
    var_label(data[[i]]$measure_value) <- new_labels[[i+3]]
    var_label(data[[i]]$den) <- new_labels[[i+6]]
  }
  
  for (i in seq_along(data)){
    data[[i]]$mytext <- paste0("Quarter: ",
                               data[[i]]$date_label,
                               "<br>",
                               var_label(data[[i]]$num),
                               prettyNum(data[[i]]$num, big.mark = ","), # data[[i]]$num,
                               "<br>",
                               var_label(data[[i]]$den),
                               prettyNum(data[[i]]$den, big.mark = ","), # data[[i]]$den,
                               "<br>",
                               "Percentage of babies: ", # not MEASURE_LABEL - too long
                               format(data[[i]]$measure_value,
                                      digits = 1,
                                      nsmall = 1),
                               "%")
  }
  
  if (is.null(data()))
  {
    return()
  }
  
  else {
    data
  }
})


# b) chart ----

# Insert the right number of plot output objects into the web page

output$gestation_by_BAPM_LOC_runcharts <- renderUI({
  
  tagList(
    fluidRow(
      column(3,
             h4("intensive care"),
             plotlyOutput(BAPM_LOC_plotListNames[1])
      ),

      column(3,
             h4("high dependency care"),
             plotlyOutput(BAPM_LOC_plotListNames[2])
      ),

      column(6,
             h4("special care"),
             plotlyOutput(BAPM_LOC_plotListNames[3])
      )
    ) # fluidRow
  )
})

for (i in 1:max_plots_gestation_by_BAPM_LOC) {
  # Need local so that each item gets its own number. Without it, the value
  # of i in the renderPlot() will be the same across all instances, because
  # of when the expression is evaluated.
  local({
    my_i <- i
    plotname <- BAPM_LOC_plotListNames[my_i]
    
    output[[plotname]] <- renderPlotly({
      creates_runcharts(plotdata = gest_by_BAPM_LOC_runchart_data()[[my_i]]) |> 
        layout(xaxis = list(#dtick = "6",
          tickangle = -45),
          yaxis = list(range = c(0, y_max_gestation_by_BAPM_LOC() * 1.05)), # forces y axis to same value on all charts
          legend = list(orientation = "v",
                        x = 1.2,
                        y = 0.5,
                        xref = "container",
                        xanchor = "left"))
    }) 
  }) 
}

# c) chart title ----

output$gestation_by_BAPM_LOC_runcharts_title <- renderText({
  "Scotland"
})

output$gestation_by_BAPM_LOC_runcharts_sub_title <- renderText({
  HTML(paste0("Percentage of ", Selected$Nicename, " babies admitted to a neonatal unit")
  )
})
