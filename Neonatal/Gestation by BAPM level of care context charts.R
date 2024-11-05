# a) data ----

Selected$Nicename <- paste0("late pre-term (34", "<sup>+0</sup>",
                            " to 36", "<sup>+6</sup>", " weeks gestation)")

Selected$BAPM_LOC_Subgroup_cat <- "between 34 and 36 weeks (inclusive)"

observeEvent(input$BAPM_LOC_subgroup_cat, Selected$Nicename <- case_when(
  input$BAPM_LOC_subgroup_cat == "between 34 and 36 weeks (inclusive)" ~ paste0("late pre-term (34", "<sup>+0</sup>",
                                                         " to 36", "<sup>+6</sup>", " weeks gestation)"),
  input$BAPM_LOC_subgroup_cat == "between 37 and 42 weeks (inclusive)" ~ paste0("term and post-term (37", "<sup>+0</sup>",
                                                         " to 42", "<sup>+6</sup>", " weeks gestation)")
  )
)

legend_name_order <- c("born alive",
                       "admitted to a neonatal unit",
                       "admitted to special care",
                       "admitted to intensive care",
                       "admitted to high dependency care")

gest_by_BAPM_LOC_context_data <- reactive({ 
  # selects data
  
  req(input$BAPM_LOC_subgroup_cat)
  
  data <- gest_by_BAPM_LOC_data %>%
    filter(measure_cat != "other or not needed" & subgroup_cat == Selected$BAPM_LOC_Subgroup_cat
    ) %>% 
    mutate(mytext = case_when(
      measure_cat == "total" ~
        paste0("Quarter: ",
               date_label,
               "<br>",
               "Total number of ",
               short_formatted_name,
               " babies born alive: ",
               prettyNum(num, big.mark = ",")
        ),
      measure_cat == "all admissions to a neonatal unit" ~
        paste0("Quarter: ",
               date_label,
               "<br>",
               "Total number of ",
               short_formatted_name,
               " babies admitted to a neonatal unit: ",
               prettyNum(num, big.mark = ",")
        ),
      .default = 
        paste0("Quarter: ",
               date_label,
               "<br>",
               "Number of ",
               short_formatted_name,
               " babies admitted to ",
               measure_cat,
               ": ",
               prettyNum(num, big.mark = ",")
        )
    ),
    legend_name = case_match(
      measure_cat,
      "all admissions to a neonatal unit" ~ "admitted to a neonatal unit",
      "total" ~ "born alive",
      .default = paste0("admitted to ", measure_cat)
    ),
    legend_name = factor(legend_name,
                         levels = legend_name_order
    )
    ) %>%
    
    arrange(date, subgroup_cat, legend_name) %>% 
    
    droplevels()
  
  if (is.null(data()))
  {
    return()
  }
  
  else {
    data
  }
})

# b) chart ---- 

output$gest_by_BAPM_LOC_context_charts <- renderPlotly({
  
    # ensures ticks and tick labels correspond (different for ABC, TERMINATIONS, SMR02)
  
  select_date_tickvals <- SMR02_date_tickvals
  select_date_ticktext <- SMR02_date_ticktext

  xaxis_plots <<- orig_xaxis_plots
  xaxis_plots[["tickmode"]] <- "array"
  xaxis_plots[["tickvals"]] <- select_date_tickvals
  xaxis_plots[["ticktext"]] <- select_date_ticktext
  
  yaxis_plots <<- orig_yaxis_plots
  
  yaxislabeltext <- list(title = list(
    text =  "Number of babies")
  )

  breakdown_chart <- plot_ly(
    data = filter(gest_by_BAPM_LOC_context_data(),
                  measure_cat != "total"
    ) %>% droplevels(),
    x = ~ date,
    y = ~ num,
    type = "scatter",
    mode = "lines+markers",
    color = ~ legend_name,
    colors = ~ selected_colours[2:5],
    symbol = ~ legend_name,
    symbols = ~ c("circle", "square-x-open", "diamond", "star"),
    line = list(width = 2),
    hovertext = ~ mytext,
    hoverinfo = "text"
  ) %>% 
    layout(
      xaxis = xaxis_plots,
      yaxis = yaxis_plots
    )
  
  totals_chart <- plot_ly(
    data = filter(gest_by_BAPM_LOC_context_data(),
                  measure_cat == "total" # %in% c("total", "all admissions to a neonatal unit"
    ) %>% droplevels(),
    x = ~ date,
    y = ~ num,
    type = "scatter",
    mode = "lines+markers",
    color = ~ legend_name,
    colors = ~ selected_colours[1:2],
    symbol = ~ legend_name,
    symbols = ~ c("circle-open", "circle"),
    line = list(width = 2),
    showlegend = TRUE,
    hovertext = ~ mytext,
    hoverinfo = "text"
  ) %>% 
    add_trace(
      data = filter(gest_by_BAPM_LOC_context_data(),
                  measure_cat == "all admissions to a neonatal unit"
      ) %>% droplevels(),
      showlegend = FALSE
    ) %>% 
    layout(
      xaxis = xaxis_plots,
      yaxis = yaxis_plots
    )

  chart <- subplot(totals_chart, breakdown_chart,
                   margin = 0.075,
                   shareX = TRUE,
                   shareY = FALSE,
                   titleY = TRUE) %>%
    layout(
      legend = list(
        title = list(text = "Scotland"),
        orientation = "v",
        x = 1.0,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        itemclick = FALSE),
      yaxis = yaxislabeltext,
      yaxis2 = yaxislabeltext,
      margin = list(pad = 10) # distance between axis and plot
    )  %>%
    config(displaylogo = F, displayModeBar = FALSE)
  
  })

# c) chart title ----

output$gest_by_BAPM_LOC_context_chart_sub_title <- renderText({
  HTML(paste0("Number of ", Selected$Nicename, " babies that were"))
})
