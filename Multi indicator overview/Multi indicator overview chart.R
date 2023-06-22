# Multi indicator overview - chart ----

# a) data ----

multi_indicator_chart_data <- reactive({
  # selects data
  
  req(input$date)
  
  data <- filter(annual_dataframe,
                 DATE == Selected$Date
                 & HBTYPE == Selected$HBType) %>% 
    arrange(desc(KEY_INDICATOR_REF)) %>% 
    mutate(LABEL = sub("Percentage", "%", PLOTLYLABEL)) %>% 
    select(- PLOTLYLABEL) %>% 
    mutate(LABEL = factor(LABEL, levels = as.character(unique(LABEL)))) # updates the factor levels
  
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
      !HBNAME %in% c("Scotland",
                     as.character(Selected$HBName)) # dots for non-selected Boards exc. Scotland
    ),
    x = ~ RESCALED,
    y = ~ LABEL,
    type = "scatter",
    mode = "markers",
    orientation = "h",
    name = "other Boards",
    visible = TRUE,
    #showlegend = FALSE,
    hovertext = ~ paste0(HBNAME,
                         ": ",
                         format(
                           MEASURE,
                           digits = 1, nsmall = 1
                         ),
                         SUFFIX),
    hoverinfo = "text",
    opacity = .5,
    marker = list(color = "#CAC6D1",
                  line = list(color = "#000000", width = 1)), # phs-graphite-50
    size = I(20)
  ) %>% 
    add_markers(data = filter(multi_indicator_chart_data(),
                              HBNAME == "Scotland"), # dots for Scotland
                x = ~ RESCALED,
                y = ~ LABEL,
                name = "Scotland",
                #visible = TRUE,
                opacity = 1,
                marker = list(color = "#000000",
                              line = list(color = "#000000", width = 1) ),
                size = I(50)) %>%
    add_markers(data = filter(multi_indicator_chart_data(),
                              HBNAME == as.character(Selected$HBName)), # dots for selected Board
                x = ~ RESCALED,
                y = ~ LABEL,
                name = ~ HBNAME,
                opacity = 1,
                marker = list(color = "#83BB26", # phs-green want #0078D4 phs-blue?
                              line = list(color = "#000000", width = 1) ),
                size = I(50)) %>%
    layout(
      font = list(size = 14),
      margin = list(pad = 60, r = 90), # makes min (pad) and max (r) values stand clear of lines
      xaxis = list(title = "",
                   showgrid = FALSE,
                   showticklabels = FALSE,
                   zeroline = FALSE),
      yaxis = list(title = "",
                   ticks = "",
                   showgrid = TRUE,
                   showticklabels = TRUE,
                   zeroline = TRUE,
                   categoryorder = "trace"), # plots traces in KEY_INDICATOR_REF order
      legend = list(orientation = "h",
                    xanchor = "auto",
                    x = 0.5,
                    y = 1.2) # moves legend clear of top of chart
    ) %>%
    config(displaylogo = F, displayModeBar = FALSE) #, modeBarButtonsToRemove = bttn_remove)

# puts min and max values on chart
  
  fig <- fig %>% add_annotations(x = -0.05,
                                 y = multi_indicator_chart_data()$LABEL,
                                 text = paste0(format(multi_indicator_chart_data()$MIN,
                                                      digits = 1, nsmall = 1)),
                                 #data_multi_indicator_chart$SUFFIX),
                                 font = list(
                                   color = "#CAC6D1"),
                                 showarrow = FALSE,
                                 xref = "paper",
                                 yref = "y"
  )
  
  fig <- fig %>% add_annotations(x = 1,
                                  y = multi_indicator_chart_data()$LABEL,
                                  text = paste0(format(multi_indicator_chart_data()$MAX,
                                                       digits = 1, nsmall = 1),
                                                multi_indicator_chart_data()$SUFFIX),
                                  xanchor = 'left',
                                  font = list(
                                    color = "#CAC6D1"),
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

  
