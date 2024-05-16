# a) data ----

apgar5_context_data <- reactive({
  # selects data
  
  req(input$hbname)

data <- apgar5_data %>%
  filter(hbname == Selected$HBName &
           period == "Q" &
           hbtype == Selected$HBType) %>%
  set_variable_labels(
    num = "Babies that had an Apgar5 score less than 7",
    den = "Babies that had a known Apgar5 score"
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

  if (is.null(data()))
  {
    return()
  }

  else {
    data
  }
})

# b) chart ---- 

output$apgar5_context_charts <- renderPlotly({
  creates_context_charts(plotdata = apgar5_context_data()
                    )
})
