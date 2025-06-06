# a) data ----

stillbirths_runchart_data <-

NRS_timeseries %>%
  filter(!measure_cat %like% "total" & # don't want the "total" values
           date_label != "Apr-Jun 2020") %>% # remove point from plot for a balanced look
  mutate(measure_label = paste0("Rate per 1,000 ", den_description),
         hover_date_label = if_else(date_label == "2020",
                              paste0("Year: ", date_label),
                              paste0("Quarter: ", date_label)
         )
         ) %>% 
  set_variable_labels(
    mean = " average to Oct-Dec 2019",
    extended = " projected average from Jan-Mar 2020"
    ) %>% 
    mutate(mytext = paste0(date_label,
                           "<br>",
                           str_to_sentence(measure_cat),
                           "<br>",
                           measure_label,
                           ": ",
                           format(measure_value,
                                  digits = 1,
                                  nsmall = 1)
                           )
    )

# b) chart ----

# set y axis labels for charts

yaxislabeltext1 <- list(title = list(text = "Rate per 1,000 total (live + still) births",
                                     font = list(size = 12),
                                     standoff = 10))

yaxislabeltext2 <- list(title = list(text = "Rate per 1,000 live births",
                                     font = list(size = 12),
                                     standoff = 10))

xaxis_plots <- orig_xaxis_plots
xaxis_plots[["tickmode"]] = "array"
xaxis_plots[["ticktext"]] = NRS_date_ticktext
xaxis_plots[["tickvals"]] = NRS_date_tickvals

yaxis_plots <- orig_yaxis_plots

yaxis_plots[["range"]] <- list(0, y_max_NRS * 1.05) # expands the y-axis range to prevent cut-offs

# create plotly chart

stillbirth_charts <- stillbirths_runchart_data %>%
  split(.$measure_cat2) %>% 
  lapply(
    function(d)
      plot_ly(d, 
              x = ~ date,
              y = ~ measure_value,
              type = "scatter",
              mode = "lines+markers",
              line = list(color = "black", # black lines
                          width = 1,
                          dash = "solid"),
              marker = list(color = "black", # black dots
                            size = 5),
              name = "rate per 1,000 related births", # retrieves label of variable
              legendrank = 100,
              legendgroup = "measure_value",
              showlegend = ~ unique(measure_cat) == "infant deaths",
              hovertext = ~ mytext,
              hoverinfo = "text"
      ) %>% 
      add_trace(
        y = ~ mean, # solid blue line
        type = "scatter",
        mode = "lines",
        line = list(color = phs_colours("phs-blue"), 
                    width = 1, dash = "solid"),
        marker = NULL,
        name = "average to Oct-Dec 2019", # label of variable
        legendrank = 200,
        legendgroup = "mean",
        showlegend = ~ unique(measure_cat) == "infant deaths",
        hoverinfo = "y",
        yhoverformat = ".2f"
      ) %>% 
      add_trace(
        y = ~ extended, # dotted blue line # this line first as plotting last leads to overrun 
        type = "scatter",
        mode = "lines",
        line = list(
          color = phs_colours("phs-blue"), 
          width = 1,
          dash = "4"
        ),
        marker = NULL,
        name = "projected average from Jan-Mar 2020", # label of variable
        legendrank = 300, 
        legendgroup = "extended",
        showlegend = ~ unique(measure_cat) == "infant deaths",
        hoverinfo = "y",
        yhoverformat = ".2f"
      ) %>% 
      layout(
        font = plotly_global_font,
        xaxis = xaxis_plots,
        yaxis = yaxis_plots,
        annotations = list(
          x = 0.5,
          y = 1.0,
          text = ~ unique(measure_cat),
          font = list(size = 16),
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE
        )
      )
  )

stillbirth_charts <- stillbirth_charts %>% 
  subplot(nrows = 2,
          heights = c(0.45, 0.45),
          margin = c(0.02, 0.02, 0.075, 0.075),
          shareX = FALSE,
          shareY = FALSE,
          titleY = TRUE
          ) %>%
  layout(
    legend = list(title = list(text = paste0("Scotland", "<br>"))),
    yaxis = yaxislabeltext1,
    yaxis2 = yaxislabeltext2,
    yaxis3 = yaxislabeltext1,
    yaxis4 = yaxislabeltext2,
    yaxis5 = yaxislabeltext2
  )

output$stillbirths_runcharts <- renderPlotly({
  
stillbirth_charts <- stillbirth_charts %>% 
    layout(
      legend = list(orientation = "v",
                    x = 0.9,
                    xanchor = "auto",
                    y = 0.25,
                    groupclick = "togglegroup"
                    ),
      margin = list(pad = 10) # distance between axis and plot
      ) %>% 
    config(displaylogo = F, displayModeBar = FALSE)

# Add dynamic alt text using htmlwidgets::onRender

stillbirth_charts <- htmlwidgets::onRender(stillbirth_charts, "
      function(el, x) {
        el.setAttribute('aria-label', 'Timeseries charts showing the rates of stillbirths and infant deaths, for each quarter, from Jan-Mar 2016 onwards');
      }
      ")
  })

# c) chart title ----
  
output$stillbirths_runcharts_title <- renderText({
  "Scotland"
})

# d) download data

this_excel_measure_name <- "stillbirths_and_infant_deaths"

output$stillbirths_download_data <- 
  
  download_excel_file(this_excel_measure_name)