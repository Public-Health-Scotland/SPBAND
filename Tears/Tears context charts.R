# a) data ----

tears_context_data <- reactive({
  # selects data
  
  #req(input$period)

data <- tears_data %>%
  filter(hbname == Selected$HBName &
           period == "Q" &
           hbtype == Selected$HBType) %>%
  set_variable_labels(
    num = "Women who had a 3rd or 4th degree perineal tear",
    den = "Women with a known perineal tear status"
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

output$tears_context_charts <- renderPlotly({
  creates_context_charts(plotdata = tears_context_data()
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

