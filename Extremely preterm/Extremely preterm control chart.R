# a) data ----

extremely_preterm_control_chart_data <- filter(extremely_preterm_data,
                                               measure_cat == "NICU_22_26"
                                               )

# b) chart ---- 

extremely_preterm_control_chart <- 
  
  plot_ly(
    data = extremely_preterm_control_chart_data,
    x = ~ date_label,
    y = ~ measure_value, # percentage
    type = "scatter",
    mode = "lines+markers",
    line = list(
      color = "black", # black line
      width = 2
    ),
    marker = list(
      color = "black", # black dots
      size = 5
    ),
    name = ~ "percentage",
    hovertext = ~ paste0("Quarter: ",
                         date_label,
                         "<br>",
                         "Percentage",
                         ": ",
                         format(measure_value,
                                digits = 1,
                                nsmall = 1),
                         "%"
    ), # need to add hover text for percentage only
    hoverinfo = "text"
  ) %>%
  add_lines(
    y = ~ centreline, # mean (centreline)
    line = list(
      color = phs_colours("phs-blue"), # dotted blue line
      dash = "4",
      width = 2
    ),
    marker = NULL,
    name = "centreline",
    hoverinfo = "y"
  ) %>%
  add_lines(
    y = ~ lower_warning_limit, # lower warning limit
    line = list(
      color = selected_colours[11], # phs-blue-80 line
      dash = "1",
      width = 2
    ),
    marker = NULL,
    name = "warning limits",
    legendgroup = "warning limits",
    showlegend = TRUE,
    hoverinfo = "none"
  ) %>%
  add_lines(
    y = ~ upper_warning_limit, # upper warning limit
    line = list(
      color = selected_colours[11], # phs-blue-80 line
      dash = "1",
      width = 2
    ),
    marker = NULL,
    name = "warning limits",
    legendgroup = "warning limits",
    showlegend = FALSE,
    hoverinfo = "none"
  ) %>%
  add_lines(
    y = ~ lower_control_limit, # lower control limit
    line = list(
      color = "red", # red line
      dash = "2",
      width = 2
    ),
    marker = NULL,
    name = "control limits",
    legendgroup = "control limits",
    showlegend = TRUE,
    hoverinfo = "none"
  ) %>%
  add_lines(
    y = ~ upper_control_limit, # upper control limit
    line = list(
      color = "red", # red line
      dash = "2",
      width = 2
    ),
    marker = NULL,
    name = "control limits",
    legendgroup = "control limits",
    showlegend = FALSE,
    hoverinfo = "none"
  ) %>%  
  layout(
    legend = list(title = list(text = paste0("Scotland", "<br>"))),
    font = plotly_global_font,
    xaxis = orig_xaxis_plots,
    yaxis = orig_yaxis_plots
  )

output$extremely_preterm_control_chart <- renderPlotly({
  
  extremely_preterm_control_chart <- extremely_preterm_control_chart %>% 
    layout(
      legend = list(orientation = "v",
                    groupclick = "togglegroup"
      ),
      yaxis = list(
        title = list(text = "Percentage of births (%)", standoff = 10)
      ),
      margin = list(pad = 10) # distance between axis and plot
    )%>% 
    config(displaylogo = F, displayModeBar = FALSE)
})

# c) chart title ----

output$extremely_preterm_control_chart_title <- renderText({
  "Scotland"
})

