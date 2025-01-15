# a) data ----

corrected_gestational_age_context_data <- ({ 
  # selects data
  
  data <- babies_30_32_admitted_to_neocare_data %>%
    mutate(den = num) %>%  # to fix y-axis ignoring den 
    set_variable_labels(
      den = "Babies born at 30 to 32 weeks gestation admitted to a neonatal unit",
      #den = "Babies born alive at 30 to 32 weeks gestation"
    ) %>% 
    mutate(mytext1 = "",
             # paste0("Quarter: ", 
             #                date_label,
             #                "<br>",
             #                var_label(num), ": ", prettyNum(num, big.mark = ",")),
           mytext2 = paste0("Quarter: ",
                            quarter_label,
                            "<br>",
                            var_label(den), ": ", prettyNum(den, big.mark = ","))
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

output$corrected_gestational_age_context_charts <- renderPlotly({
  
  creates_context_charts(plotdata = corrected_gestational_age_context_data,
                         yaxislabel = "Number of babies"
                    ) %>% 
    layout(showlegend = TRUE)
})
