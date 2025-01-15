# a) data ----

corrected_gestational_age_runchart_data <- ({
  # selects data
  
  data <- babies_30_32_admitted_to_neocare_data %>% 
    # temporarily using this data so rename measure to make create_runcharts function work
    # mutate(measure = "MEDIAN CORRECTED GEST AGE",
    #        suffix = "weeks") %>% 
    # filter(measure_cat %in% "all admissions to a neonatal unit" & subgroup_cat == "between 34 and 36 weeks (inclusive)" & date >= "2018-01-01") %>% 
    # droplevels() %>%
    # mutate(num_label = paste0("Number of ", short_formatted_name, " babies", "<br>", "admitted to ", measure_cat, ": "),
    #        den_label = paste0("Total number of ", short_formatted_name, " babies: "), 
    #        measure_label = paste0("Percentage of ", short_formatted_name, " babies admitted to ", measure_cat, " (%)"),
    #        measure_cat_label = measure_cat
    # ) %>%   
    set_variable_labels(
      measure_value = "Median corrected gestational age at discharge"
      #pre_pandemic_median = " average to Oct-Dec 2019",
      #extended_pre_pandemic_median = " projected average from Jan-Mar 2020",
      #post_pandemic_median = paste0("average from Jul 2022", "<br>", "to end Jun 2024"),
      # extended_post_pandemic_median = "projected average from Jul 2024"
    ) %>% 
    mutate(mytext = paste0("Quarter: ",
                           quarter_label,
                           "<br>",
                           var_label(measure_value),
                           ": ",
                           format(measure_value,
                                  digits = 1,
                                  nsmall = 1),
                           " weeks"),
           shift = NA,
           trend = NA
    )

  if (is.null(data))
  {
    return()
  }
  
  else {
    data
  }
})

# b) chart ----

output$corrected_gestational_age_runcharts <- renderPlotly({
  
  creates_runcharts(plotdata = corrected_gestational_age_runchart_data,
                    yaxislabel = "Median corrected gestational age at discharge (weeks)"
                    ) %>% 
    layout(showlegend = TRUE)
  })

# c) chart title ----

output$corrected_gestational_age_runcharts_title <- renderText({
  "Scotland"
})
