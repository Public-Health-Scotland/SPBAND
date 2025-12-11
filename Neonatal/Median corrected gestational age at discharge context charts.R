# a) data ----

corrected_gest_age_at_discharge_context_data <- ({ 
  # selects data
  
  data <- corrected_gest_at_discharge_data %>% 
    #filter(measure_cat != "median corrected gestational age") %>% 
    rename(den = num) %>% # so function works
    set_variable_labels(
      den = "Number of babies discharged to home or foster care", # to pass into function
    ) %>% 
    mutate(mytext1 = "",
           mytext2 = paste0("Quarter: ",
                            date_label,
                            "<br>",
                            var_label(den), ": ", prettyNum(den, big.mark = ",")),
           num = NA
    ) %>% 
    ungroup()
    
  if (is.null(data))
  {
    return()
  }
  
  else {
    data
  }
})

# b) chart ---- 

output$corrected_gest_age_at_discharge_context_charts <- renderPlotly({
  
  creates_context_charts(plotdata = corrected_gest_age_at_discharge_context_data,
                         yaxislabel = "Number of babies"
                    ) #%>% 
    # layout(showlegend = TRUE)
})
