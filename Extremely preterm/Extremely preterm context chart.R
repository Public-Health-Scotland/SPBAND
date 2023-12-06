# a) data ----

extremely_preterm_context_data <- 
  
  extremely_preterm_data %>% 
  filter(measure_cat == "NICU_22_26"
         ) %>% 
  set_variable_labels(
    num = "Births at 22-26 weeks in a hospital with a NICU",
    den = "All births at 22-26 weeks"
  ) %>% 
  mutate(mytext1 = paste0("Quarter: ", 
                         quarter_label,
                         "<br>",
                         var_label(num), ": ", prettyNum(num, big.mark = ",")),
         mytext2 = paste0("Quarter: ", 
                         quarter_label,
                         "<br>",
                         var_label(den), ": ", prettyNum(den, big.mark = ","))
         )

# b) chart ---- 

output$extremely_preterm_context_chart <- renderPlotly({
  creates_context_charts(plotdata = extremely_preterm_context_data,
                         date = "date",
                         num = "num",
                         den = "den"
                    )
})

