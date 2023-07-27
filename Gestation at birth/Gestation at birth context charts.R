# a) data ----

term <- c("all known gestations (18-44 weeks)", "between 37 and 41 weeks")

not_term <- c("under 32 weeks", "between 32 and 36 weeks", "42 weeks and over")

gest_at_birth_context_data <- reactive({ 
  # selects data
  
  #req(input$period)

data <- gest_at_birth_data %>%
  filter(hbname == Selected$HBName &
           period == "Q" &
           hbtype == Selected$HBType &
           indicator_cat != "under 37 weeks"
         ) %>% 

# den_data <- filter(data, indicator_cat == "under 32 weeks") %>% 
#   mutate(indicator_cat = "all known gestations (18-44 weeks)",
#          num = den)

# data <- bind_rows(den_data, data) %>% 
  mutate(mytext = if_else(indicator_cat == "all known gestations (18-44 weeks)",
                          paste0("Quarter: ",
                                 quarter_label,
                                 "<br>",
                                 "Number of babies born with a known gestation (18-44 weeks)",
                                 ": ",
                                 prettyNum(num, big.mark = ",")
                          ),
                          paste0("Quarter: ",
                                 quarter_label,
                                 "<br>",
                                 "Number of babies born at ",
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

output$gest_at_birth_context_charts <- renderPlotly({
  
# ensures ticks and tick labels correspond (different for ABC, TERMINATIONS, SMR02)
  
select_date_tickvals <- SMR02_date_tickvals
select_date_ticktext <- SMR02_date_ticktext

xaxis_plots <- orig_xaxis_plots
xaxis_plots[["tickmode"]] <- "array"
xaxis_plots[["tickvals"]] <- select_date_tickvals
xaxis_plots[["ticktext"]] <- select_date_ticktext

yaxis_plots <- orig_yaxis_plots
yaxis_plots[["title"]] <- list(text = "Number of births",
                               standoff = 30) # distance between axis and chart

term_chart <- plot_ly(
  data = filter(gest_at_birth_context_data(),
                indicator_cat %in% term),
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
  data = filter(gest_at_birth_context_data(),
                indicator_cat %in% not_term),
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
      title = list(text = paste0(gest_at_birth_context_data()$hbname, "<br>")),
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

