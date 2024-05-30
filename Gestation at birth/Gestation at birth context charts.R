# a) data ----

term <- c("between 18 and 44 weeks (inclusive)", "between 37 and 41 weeks (inclusive)")

not_term <- c("under 32 weeks", "between 32 and 36 weeks (inclusive)", "42 weeks and over (inclusive)")

gest_at_birth_context_data <- reactive({ 
  # selects data
  
  req(input$hbname)
  
  data <- gest_at_birth_data %>%
    filter(hbname == Selected$HBName &
             period == "Q" &
             hbtype == Selected$HBType &
             measure_cat != "under 37 weeks"
    ) %>% 
    mutate(mytext = if_else(measure_cat == "between 18 and 44 weeks (inclusive)",
                            paste0("Quarter: ",
                                   quarter_label,
                                   "<br>",
                                   "Number of singleton live births with a known gestation (18-44 weeks)",
                                   ": ",
                                   prettyNum(num, big.mark = ",")
                            ),
                            paste0("Quarter: ",
                                   quarter_label,
                                   "<br>",
                                   "Number of singleton live births at ",
                                   formatted_name,
                                   ": ",
                                   prettyNum(num, big.mark = ",")
                            )
    )
    ) %>% 
    
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

plotdata <- reactive({gest_at_birth_context_data()})

output$gest_at_birth_context_charts <- renderPlotly({

  # ensures ticks and tick labels correspond (different for ABC, TERMINATIONS, SMR02)
  
  select_date_tickvals <- SMR02_date_tickvals
  select_date_ticktext <- SMR02_date_ticktext
  
  # adds an asterisk to these Board names when there is a related footnote to show
  
  # legend_board_name <- if_else(
  #   (first(plotdata()$measure == "TYPE OF BIRTH") &
  #      first(plotdata()$hbname == "NHS Borders")
  #   ),
  #   paste0(first(plotdata()$hbname), "*"),
  #   first(plotdata()$hbname)
  # )

  xaxis_plots <- orig_xaxis_plots
  xaxis_plots[["tickmode"]] <- "array"
  xaxis_plots[["tickvals"]] <- select_date_tickvals
  xaxis_plots[["ticktext"]] <- select_date_ticktext
  
  yaxis_plots <- orig_yaxis_plots
  yaxis_plots[["title"]] <- list(text = "Number of births",
                                 standoff = 30) # distance between axis and chart
  
  term_chart <- plot_ly(
    data = filter(plotdata(),
                  measure_cat %in% term),
    x = ~ date,
    y = ~ num,
    type = "scatter",
    mode = "lines+markers",
    color = ~ formatted_name,
    colors = ~ selected_colours[1:5],
    symbol = ~ formatted_name,
    symbols = ~ c("circle", "square-x-open", "diamond", "star", "circle-open"),
    line = list(width = 2),
    hovertext = ~ mytext,
    hoverinfo = "text"
  ) %>% 
    layout(
      xaxis = xaxis_plots,
      yaxis = yaxis_plots
    )
  
  not_term_chart <- plot_ly(
    data = filter(plotdata(),
                  measure_cat %in% not_term),
    x = ~ date,
    y = ~ num,
    type = "scatter",
    mode = "lines+markers",
    color = ~ formatted_name,
    colors = ~ selected_colours[1:5],
    symbol = ~ formatted_name,
    symbols = ~ c("circle", "square-x-open", "diamond", "star", "circle-open"),
    line = list(width = 2),
    hovertext = ~ mytext,
    hoverinfo = "text"
  ) %>% 
    layout(
      xaxis = xaxis_plots,
      yaxis = yaxis_plots
    )
  
  chart <- subplot(term_chart, not_term_chart,
                   margin = 0.05,
                   shareX = TRUE,
                   shareY = FALSE,
                   titleY = TRUE) %>%
    layout(
      legend = list(
        title = list(text = paste0(plotdata()$hbname, "<br>")), # legend_board_name if needed
        orientation = "v",
        x = 1.0,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        itemclick = FALSE),
      # groupclick = "togglegroup")
      margin = list(pad = 30) # distance between axis and first data point
    )  %>%
    config(displaylogo = F, displayModeBar = FALSE)
  
})


