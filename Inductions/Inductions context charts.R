# a) data ----

inductions_context_data <- reactive({
  # selects data
  
  req(input$hbname)

data <- inductions_data %>%
  filter(hbname == Selected$HBName &
           period == "Q" &
           hbtype == Selected$HBType) %>%
  set_variable_labels(
    num = "Births that followed induction",
    den = "Births with a known induction status"
  ) %>% 
  mutate(mytext1 = paste0("Quarter: ", 
                         quarter_label,
                         "<br>",
                         var_label(num),
                         ": ",
                         prettyNum(num, big.mark = ",")),
         mytext2 = paste0("Quarter: ", 
                         quarter_label,
                         "<br>",
                         var_label(den),
                         ": ",
                         prettyNum(den, big.mark = ","))
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

output$inductions_context_charts <- renderPlotly({
  creates_context_charts(plotdata = inductions_context_data()
                    )
})

