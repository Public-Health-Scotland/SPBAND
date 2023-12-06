# a) data ----

stillbirths_runchart_data <-

NRS_timeseries %>%
  filter(!measure_cat %like% "total" & # don't want the "total" values
           quarter_label != "Jan-Mar 2020") %>% # remove point from plot for a balanced look
  mutate(measure_label = paste0("Rate per 1,000 ", den_description),
         date_label = if_else(quarter_label == "2020",
                              paste0("Year: ", quarter_label),
                              paste0("Quarter: ", quarter_label)
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

# set y axis labels for charts

yaxislabel1 <- list(title = list(text = "rate per 1,000 total (live + still) births",
                                 font = list(size = 12)))

yaxislabel2 <- list(title = list(text = "rate per 1,000 live births",
                                 font = list(size = 12)))

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
              x = ~ quarter_label,
              y = ~ extended, # dotted blue line # this line first as plotting last leads to overrun 
              type = "scatter",
              mode = "lines",
              line = list(
                color = phs_colours("phs-blue"), 
                width = 1,
                dash = "4"
                ),
              name = "projected average from Jan-Mar 2020", # label of variable
              legendrank = 300, 
              legendgroup = "extended",
              showlegend = ~ unique(measure_cat) == "infant deaths",
              hovertext = "",
              hoverinfo = "none"
              ) %>%
      add_trace(
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
        name = "average to Oct-Dec 2019", # label of variable
        legendrank = 200,
        legendgroup = "mean",
        showlegend = ~ unique(measure_cat) == "infant deaths",
        hovertext = ""
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
          margin = c(0.01, 0.03, 0.075, 0.075),
          shareX = FALSE,
          shareY = FALSE,
          titleY = TRUE
          ) %>%
  layout(
      yaxis = yaxislabel1,
      yaxis2 = yaxislabel2,
      yaxis3 = yaxislabel1,
      yaxis4 = yaxislabel2,
      yaxis5 = yaxislabel2
  )
  
output$stillbirths_runcharts <- renderPlotly({
  
stillbirth_charts <- stillbirth_charts %>% 
    layout(
      legend = list(orientation = "v",
                    x = 0.9,
                    xanchor = "auto",
                    y = 0.25,
                    groupclick = "togglegroup"
                    )
      ) %>% 
    config(displaylogo = F, displayModeBar = FALSE)

  })

# c) chart title ----
  
output$stillbirths_runcharts_title <- renderText({
  "Scotland"
})

# d) download data

output$stillbirths_download_data <- downloadHandler(

  filename = function() {
      paste0(first(stillbirths_download$measure), "_", refresh_date, ".csv", sep = "")
    },

  content = function(file) {
    write.csv(stillbirths_download, file, row.names = FALSE)
    }
  )
  
