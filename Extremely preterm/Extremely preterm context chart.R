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
                         date_label,
                         "<br>",
                         var_label(num), ": ", prettyNum(num, big.mark = ",")),
         mytext2 = paste0("Quarter: ", 
                         date_label,
                         "<br>",
                         var_label(den), ": ", prettyNum(den, big.mark = ","))
         )

# b) chart ---- 

output$extremely_preterm_context_chart <- renderPlotly({
  
  context <- 
    
  creates_context_charts(plotdata = extremely_preterm_context_data
                    ) %>% 
    layout(yaxis = list(title = list(standoff = 10)
    )
    )
  
  # Add dynamic alt text using htmlwidgets::onRender
  
  context <- htmlwidgets::onRender(context, "
      function(el, x) {
        el.setAttribute('aria-label', 'Timeseries chart showing the number of births at 22-26 weeks gestation, and those that occurred in a hospital with a neonatal intensive care unit (NICU) on site, for each quarter, from Jan-Mar 2018 onwards');
      }
      ")
})

