# a) data ----

apgar5_context_data <- reactive({
  # selects data
  
  #req(input$period)

data <- apgar5_data %>%
  filter(hbname == Selected$HBName &
           period == "Q" &
           hbtype == Selected$HBType) %>%
  set_variable_labels(
    num = "Babies with an Apgar5 score less than 7",
    den = "Babies with a known Apgar5 score"
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

# # c) chart title ----
# 
# output$inductions_runcharts_title <- renderText({
#   paste0("Board of ",
#          str_to_sentence(input$organisation),
#          ": ", 
#          input$hbname
#   )
#   
# })

