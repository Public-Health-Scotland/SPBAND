# Multi indicator overview - chart ----

# a) data ----

multi_indicator_chart_data <- reactive({
  # selects data
  
  req(input$date)
  
  data <- filter(annual_dataframe,
                 date == Selected$Date
                 & hbtype == Selected$HBType) %>% 
    arrange(desc(key_measure_ref)) %>% 
    mutate(label = sub("Percentage", "%", plotlylabel)) %>% 
    select(- plotlylabel) %>% 
    mutate(label = factor(label, levels = as.character(unique(label)))) # updates the factor levels
  
  if (is.null(data()))
  {
    return()
  }
  
  else {
    data
  }
})

# b) bullet chart ----

output$multi_indicator_chart <- output$multi_indicator_chart2 <- renderPlotly({
  # creates bullet chart
  
fig <- plot_ly(
    data = filter(
      multi_indicator_chart_data(),
      !hbname %in% c("Scotland",
                     as.character(Selected$HBName)) # dots for non-selected Boards exc. Scotland
    ),
    x = ~ RESCALED,
    y = ~ label,
    type = "scatter",
    mode = "markers",
    orientation = "h",
    name = "other Boards",
    visible = TRUE,
    #showlegend = FALSE,
    hovertext = ~ paste0(hbname,
                         ": ",
                         format(
                           measure,
                           digits = 1, nsmall = 1
                         ),
                         suffix),
    hoverinfo = "text",
    opacity = .5,
    marker = list(color = "#CAC6D1",
                  line = list(color = "#000000", width = 1)), # phs-graphite-50
    size = I(20)
  ) %>% 
    add_markers(data = filter(multi_indicator_chart_data(),
                              hbname == "Scotland"), # dots for Scotland
                x = ~ RESCALED,
                y = ~ label,
                name = "Scotland",
                #visible = TRUE,
                opacity = 1,
                marker = list(color = "#000000",
                              line = list(color = "#000000", width = 1) ),
                size = I(50)) %>%
    add_markers(data = filter(multi_indicator_chart_data(),
                              hbname == as.character(Selected$HBName)), # dots for selected Board
                x = ~ RESCALED,
                y = ~ label,
                name = ~ hbname,
                opacity = 1,
                marker = list(color = "#83BB26", # phs-green want #0078D4 phs-blue?
                              line = list(color = "#000000", width = 1) ),
                size = I(50)) %>%
    layout(
      font = plotly_global_font,
      margin = list(pad = 60, r = 90), # makes min (pad) and max (r) values stand clear of lines
      xaxis = list(title = "",
                   showgrid = FALSE,
                   showticklabels = FALSE,
                   zeroline = FALSE),
      yaxis = list(title = "",
                   ticks = "",
                   showgrid = TRUE,
                   showticklabels = TRUE,
                   tickfont = list(size = 14),
                   zeroline = TRUE,
                   categoryorder = "trace"), # plots traces in key_measure_ref order
      legend = list(orientation = "h",
                    xanchor = "auto",
                    font = list(size = 14),
                    x = 0.5,
                    y = 1.2) # moves legend clear of top of chart
    ) %>%
    config(displaylogo = F, displayModeBar = FALSE) #, modeBarButtonsToRemove = bttn_remove)

# puts min and max values on chart
  
  fig <- fig %>% add_annotations(x = -0.05,
                                 y = multi_indicator_chart_data()$label,
                                 text = paste0(format(multi_indicator_chart_data()$MIN,
                                                      digits = 1, nsmall = 1)),
                                 #data_multi_indicator_chart$suffix),
                                 font = list(
                                   color = "#CAC6D1",
                                   size = 14),
                                 showarrow = FALSE,
                                 xref = "paper",
                                 yref = "y"
  )
  
  fig <- fig %>% add_annotations(x = 1,
                                  y = multi_indicator_chart_data()$label,
                                  text = paste0(format(multi_indicator_chart_data()$MAX,
                                                       digits = 1, nsmall = 1),
                                                multi_indicator_chart_data()$suffix),
                                  xanchor = 'left',
                                  font = list(
                                    color = "#CAC6D1",
                                    size = 14),
                                  showarrow = FALSE,
                                  xref = "paper",
                                  yref = "y"
  )
})

# c) title

output$multi_indicator_chart_title <- output$multi_indicator_chart_title2 <- renderText({
  paste0("Core Maternity Indicators, by Board of ",
         str_to_sentence(input$organisation),
         ", for ",
         if_else(input$date %like% "/", "financial year ",
                 "calendar year "),
         input$date)
})

  
