# a) data ----

BAPM_LOC_plotListNames = c("intensive care", "high dependency care", "special care") # placeholder for plots

# initialise Selected$Nicename2

Selected$Nicename2 <- paste0("late pre-term (34", "<sup>+0</sup>",
                             " to 36", "<sup>+6</sup>", " weeks gestation)")

Selected$BAPM_LOC_Subgroup_cat <- "between 34 and 36 weeks (inclusive)"

observeEvent(input$BAPM_LOC_subgroup_cat, Selected$Nicename2 <- case_when(
  input$BAPM_LOC_subgroup_cat == "between 34 and 36 weeks (inclusive)" ~ paste0("late pre-term (34", "<sup>+0</sup>",
                                                                                " to 36", "<sup>+6</sup>", " weeks gestation)"),
  input$BAPM_LOC_subgroup_cat == "between 37 and 42 weeks (inclusive)" ~ paste0("term and post-term (37", "<sup>+0</sup>",
                                                                                " to 42", "<sup>+6</sup>", " weeks gestation)")
)
)

y_max_gest_by_BAPM_LOC <- reactiveVal(0) # initialise y_max_gest_by_BAPM_LOC

gest_by_BAPM_LOC_runchart_data <- reactive({
  # selects data
  
  req(input$BAPM_LOC_subgroup_cat)
  
  data <- gest_by_BAPM_LOC_data %>% 
    filter(measure_cat %in% BAPM_LOC_plotListNames & subgroup_cat == Selected$BAPM_LOC_Subgroup_cat & date >= "2018-01-01") %>% 
    droplevels() %>%
   # filter(!is.na(measure_cat2) & subgroup_cat == Selected$BAPM_LOC_Subgroup_cat) %>% 
    mutate(num_label = paste0("Number of ", short_formatted_name, " babies", "<br>", "admitted to ", measure_cat, ": "),
           den_label = paste0("Total number of ", short_formatted_name, " babies: "), 
           measure_label = paste0("Percentage of ", short_formatted_name, " babies admitted to ", measure_cat, " (%)"),
           measure_cat_label = measure_cat
    ) %>%   
    set_variable_labels(
      measure_value = "Percentage of births (%)"
      #pre_pandemic_median = " average to Oct-Dec 2019",
      #extended_pre_pandemic_median = " projected average from Jan-Mar 2020"
      #post_pandemic_median = paste0("average from Jul 2022", "<br>", "to end Jun 2024"),
      #extended_post_pandemic_median = "projected average from Jul 2024"
    ) %>% 
    mutate(mytext = paste0("Quarter: ",
                           date_label,
                           "<br>",
                           num_label,
                           prettyNum(num, big.mark = ","),
                           "<br>",
                           den_label,
                           prettyNum(den, big.mark = ","),
                           "<br>",
                           "Percentage of babies: ", # not MEASURE_LABEL - too long
                           format(measure_value,
                                  digits = 1,
                                  nsmall = 1),
                           "%"),
           shift = NA,
           trend = NA
    )

  new_max <- max(data$measure_value) # local maximum measure_value
  
  observeEvent(new_max, {   # update maximum measure_value when subgroup_cat changes
    y_max_gest_by_BAPM_LOC(new_max)
  }
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

output$gestation_by_BAPM_LOC_runcharts <- renderPlotly({
  
  gest_by_BAPM_LOC_runchart_data() %>% 
    group_by(.$measure_cat) %>%
    group_map(~
                creates_runcharts(plotdata = .,
                                  yaxislabel = "Percentage of babies (%)",
                ) %>% 
                layout(showlegend = ~ unique(measure_cat) == "special care",
                       yaxis = list(range = c(0, y_max_gest_by_BAPM_LOC() * 1.05))
                ) # forces y axis to same value on all charts
    ) %>% 
    subplot(.,
            nrows = 2,
            margin = c(0.04, 0.04, 0.15, 0.15),
            shareX = FALSE,
            shareY = FALSE,
            titleY = TRUE) %>% 
    layout(legend =
             list(x = 0.53,
                  y = 0.25)
    )
})

# c) chart title ----

output$gestation_by_BAPM_LOC_runcharts_title <- renderText({
  "Scotland"
})

output$gestation_by_BAPM_LOC_runcharts_sub_title <- renderText({
  HTML(paste0("Percentage of ", Selected$Nicename2, " babies admitted to a neonatal unit")
  )
})
