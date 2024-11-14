# a) data ----

type_of_birthNames = c("all live births (where type is known)", "spontaneous vaginal births",
                       "unplanned caesarean births", "planned caesarean births",
                       paste(
                         strwrap(
                           "assisted vaginal births (including forceps, ventouse, and vaginal breech births)",
                           width = 50), collapse = "<br>"))

type_of_birth_context_data <- reactive({
  # selects data
  
  req(input$hbname)
  
  data <- type_of_birth_data %>%
    filter(hbname == Selected$HBName &
             period == "Q" &
             hbtype == Selected$HBType & 
             measure_cat != "all caesarean births") %>%
    mutate(measure_cat = if_else(measure_cat == "assisted vaginal births",
                                 paste(
                                   strwrap(
                                     "assisted vaginal births (including forceps, ventouse, and vaginal breech births)",
                                     width = 50), collapse = "<br>"),
                                 measure_cat
    )
    )
  
  # pick one category to get den (all live births)
  
  den_data <- filter(data, measure_cat == "spontaneous vaginal births") %>% # pick one category to get den (all live births)
    mutate(measure_cat = "all live births (where type is known)",
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
    ) %>%
    droplevels() %>% 
    select(- measure_cat_label)
  
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
  
  # adds an asterisk to these Board names when there is a related footnote to show
  
  legend_board_name <- if_else(
    (first(type_of_birth_context_data()$measure == "TYPE OF BIRTH") &
       first(type_of_birth_context_data()$hbname == "NHS Borders")
    ),
    paste0(first(type_of_birth_context_data()$hbname), "*"),
    first(type_of_birth_context_data()$hbname)
  )
  
  xaxis_plots <<- orig_xaxis_plots
  xaxis_plots[["tickmode"]] <- "array"
  xaxis_plots[["tickvals"]] <- select_date_tickvals
  xaxis_plots[["ticktext"]] <- select_date_ticktext
  
  yaxis_plots <<- orig_yaxis_plots
  yaxis_plots[["range"]] <- list(0, y_max * 1.05) # expands the y-axis range to prevent cut-offs
  
  yaxislabeltext <- list(title = list(
    text =  "Number of births")
  )
  
  yaxis_plots[["title"]] <- list(standoff = 10)

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
        title = list(text = paste0(legend_board_name, "<br>")),
        orientation = "v",
        x = 1.0,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        itemclick = FALSE),
      # groupclick = "togglegroup") 
      margin = list(pad = 10) # distance between axis and plot
    ) %>% 
    layout(yaxis = yaxislabeltext) %>% 
    config(displaylogo = F, displayModeBar = FALSE)
  
})

