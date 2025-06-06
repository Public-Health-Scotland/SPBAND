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
                                   measure_cat_label,
                                   ": ",
                                   prettyNum(num, big.mark = ",")
                            )
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

output$gest_at_birth_context_charts <- renderPlotly({
  
  # ensures ticks and tick labels correspond (different for ABC, TERMINATIONS, SMR02)
  
  select_date_tickvals <- SMR02_date_tickvals
  select_date_ticktext <- SMR02_date_ticktext
  
  xaxis_plots <- orig_xaxis_plots
  xaxis_plots[["tickmode"]] <- "array"
  xaxis_plots[["tickvals"]] <- select_date_tickvals
  xaxis_plots[["ticktext"]] <- select_date_ticktext
  
  yaxis_plots <- orig_yaxis_plots
  
  yaxislabeltext <- list(title = list(
    text =  "Number of births")
  )
  
  yaxis_plots[["title"]] <- list(standoff = 10)

  term_chart <- plot_ly(
    data = filter(gest_at_birth_context_data(),
                  measure_cat %in% term) %>% 
      droplevels(),
    x = ~ date,
    y = ~ num,
    type = "scatter",
    mode = "lines+markers",
    color = ~ measure_cat_label,
    colors = ~ selected_colours[1:2],
    symbol = ~ measure_cat_label,
    symbols = ~ c("circle", "square-x-open"),
    line = list(width = 2),
    hovertext = ~ mytext,
    hoverinfo = "text"
  ) %>% 
    layout(
      xaxis = xaxis_plots,
      yaxis = yaxis_plots
    )
  
  not_term_chart <- plot_ly(
    data = filter(gest_at_birth_context_data(),
                  measure_cat %in% not_term) %>% 
      droplevels(),
    x = ~ date,
    y = ~ num,
    type = "scatter",
    mode = "lines+markers",
    color = ~ measure_cat_label,
    colors = ~ selected_colours[3:5],
    symbol = ~ measure_cat_label,
    symbols = ~ c("diamond", "star", "circle-open"),
    line = list(width = 2),
    hovertext = ~ mytext,
    hoverinfo = "text"
  ) %>% 
    layout(
      xaxis = xaxis_plots,
      yaxis = yaxis_plots
    )
  
  chart <- subplot(term_chart, not_term_chart,
                   margin = 0.075,
                   shareX = TRUE,
                   shareY = FALSE,
                   titleY = TRUE) %>%
    layout(
      legend = list(
        title = list(text = paste0(gest_at_birth_context_data()$hbname, "<br>")), # legend_board_name if needed
        orientation = "v",
        x = 1.0,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        itemclick = FALSE),
      yaxis = yaxislabeltext,
      yaxis2 = yaxislabeltext,
      # groupclick = "togglegroup")
      margin = list(pad = 10) # distance between axis and plot
    )  %>%
    config(displaylogo = F, displayModeBar = FALSE)
  
  # Add dynamic alt text using htmlwidgets::onRender
  
  chart <- htmlwidgets::onRender(chart, "
      function(el, x) {
        el.setAttribute('aria-label', 'Timeseries charts showing the the number of singleton live births at 18-44 weeks, 37+0 to 41+6 weeks, 32+0 to 36+6 weeks, 42+0 weeks and over and under 32 weeks gestation, for each quarter, from Jan-Mar 2017 onwards');
      }
      ")

  return(chart)
  
})


