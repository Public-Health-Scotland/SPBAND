# a) data ----

extremely_preterm_control_chart_data <- filter(extremely_preterm_data,
                                               indicator_cat == "NICU_22_26"
                                               )

# b) chart ---- 

extremely_preterm_control_chart <- 
  
  plot_ly(
    data = extremely_preterm_control_chart_data,
    x = ~ quarter_label,
    y = ~ measure, # percentage
    type = "scatter",
    mode = "lines+markers",
    line = list(
      color = selected_colours[1], # phs-purple line
      width = 2
    ),
    marker = list(
      color = selected_colours[1], # phs-purple line
      size = 5
    ),
    name = ~ "percentage",
    hovertext = ~ paste0("Quarter: ",
                         quarter_label,
                         "<br>",
                         "Percentage",
                         ": ",
                         format(measure,
                                digits = 1,
                                nsmall = 1),
                         "%"
    ), # need to add hover text for percentage only
    hoverinfo = "text"
  ) %>%
  add_lines(
    y = ~ `mean`, # mean (centreline)
    line = list(
      color = selected_colours[2], # phs-magenta line
      dash = "4",
      width = 2
    ),
    marker = NULL,
    name = "centreline",
    hoverinfo = "none"
  ) %>%
  add_lines(
    y = ~ lower_warning_limit, # lower warning limit
    line = list(
      color = selected_colours[3], # phs-blue line
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
      color = selected_colours[3], # phs-blue line
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
      color = selected_colours[4], # phs-green line
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
      color = selected_colours[4], # phs-green line
      dash = "2",
      width = 2
    ),
    marker = NULL,
    name = "control limits",
    legendgroup = "control limits",
    showlegend = FALSE,
    hoverinfo = "none"
  ) %>%  
  layout(xaxis = orig_xaxis_plots,
         yaxis = orig_yaxis_plots)


output$extremely_preterm_control_chart <- renderPlotly({
  
extremely_preterm_control_chart <- extremely_preterm_control_chart %>% 
    layout(
      legend = list(orientation = "v",
                    groupclick = "togglegroup"
                    )
      ) %>% 
    config(displaylogo = F, displayModeBar = FALSE)
})

# c) chart title ----

output$extremely_preterm_control_chart_title <- renderText({
  "Scotland"
})

