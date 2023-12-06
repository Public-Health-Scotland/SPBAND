# a) data ----

type_of_birthNames = c("all live births", "spontaneous vaginal births",
                                "unplanned caesarean births", "planned caesarean births",
                                "assisted births including breech births")

type_of_birth_context_data <- reactive({ 
  # selects data
  
  #req(input$period)

data <- type_of_birth_data %>%
  filter(hbname == Selected$HBName &
           period == "Q" &
           hbtype == Selected$HBType & 
           measure_cat != "all caesarean births") %>%
  mutate(measure_cat = if_else(measure_cat == "assisted births",
                             "assisted births including breech births",
                             measure_cat
                             )
         )

# pick one category to get den (all live births)

den_data <- filter(data, measure_cat == "assisted births including breech births") %>% # pick one category to get den (all live births)
  mutate(measure_cat = "all live births",
         num = den
         )

data <- bind_rows(den_data, data) %>%
    mutate(measure_cat = factor(measure_cat,
                                  levels = type_of_birthNames),
           mytext1 = paste0("Quarter: ",
                            quarter_label, 
                            "<br>",
                            str_to_sentence(measure_cat),
                            ": ",
                            prettyNum(num, big.mark = ",")
                            )
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

output$type_of_birth_context_charts <- renderPlotly({
  
  y_max <- max(type_of_birth_context_data()$den, na.rm = TRUE) # allows a margin to be set around y-axis
  
    # ensures ticks and tick labels correspond (different for ABC, TERMINATIONS, SMR02)
  
   select_date_tickvals <- SMR02_date_tickvals
   select_date_ticktext <- SMR02_date_ticktext
   
   xaxis_plots <- orig_xaxis_plots
   xaxis_plots[["tickmode"]] <- "array"
   xaxis_plots[["tickvals"]] <- select_date_tickvals
   xaxis_plots[["ticktext"]] <- select_date_ticktext
   
   yaxis_plots <- orig_yaxis_plots
   yaxis_plots[["range"]] <- list(0, y_max * 1.05) # expands the y-axis range to prevent cut-offs
   yaxis_plots[["title"]] <- list(text = "Number of births",
                                  standoff = 30) # distance between axis and chart
  
  plot_ly(
      data = type_of_birth_context_data(),
      x = ~ date,
      y = ~ num,
      type = "scatter",
      mode = "lines+markers",
      color = ~ measure_cat,
      colors = selected_colours[1:5],
      symbol = ~ measure_cat,
      symbols = ~ c("circle", "square-x-open", "diamond", "star", "circle-open"),
      line = list(width = 2),
      hovertext = ~ mytext1,
      hoverinfo = "text"
      ) %>%
    layout(
      xaxis = xaxis_plots,
      yaxis = yaxis_plots,
      legend = list(
        title = list(text = paste0(type_of_birth_context_data()$hbname, "<br>")),
        orientation = "v",
        x = 1.0,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        itemclick = FALSE),
        # groupclick = "togglegroup") 
      margin = list(pad = 30) # distance between axis and first data point
    ) %>% 
    config(displaylogo = F, displayModeBar = FALSE)
                    
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

