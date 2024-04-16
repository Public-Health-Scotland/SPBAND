# A) Inductions Overview with different y-axis limits for islands ----

Date <-  "Jun 2019"
HBType <- "RESIDENCE"
HBName <- "Scotland"

Selected <- data.frame(Date, HBType, HBName)

island_names <- c("NHS Orkney", "NHS Shetland", "NHS Western Isles")

# a) data ----

inductions_small_multiples_data <- #reactive({
  # selects data
  
  #req(input$organisation)
  
  data <- inductions_data %>%
    filter(hbtype == Selected$HBType & period == "Q") %>%
    set_variable_labels(
      measure_value = "Percentage of births following induction (%)",
      median = " average to Oct-Dec 2019",
      extended = " projected average from Jan-Mar 2020"
    ) %>% 
  mutate(mytext = paste0(hbname,
                           "<br>",
                         "Quarter: ", 
                         quarter_label,
                         "<br>",
                         "Percentage of births",
                         ": ",
                         format(measure_value,
                                digits = 1,
                                nsmall = 1),
                         "%"),
        date = quarter_label,
        HBGROUP = if_else(hbname %in% island_names, "island", "mainland"),
        nrows = if_else(hbname %in% island_names, 1, 4),
        #xshowticklabels = if_else(hbname %in% island_names, TRUE, FALSE)
  ) %>% 
  group_by(HBGROUP, hbtype) %>% 
  mutate(y_max = max(measure_value),
         hbname2 = factor(hbname, levels = HBnames)
         ) #%>% 
         #y_max = plyr::round_any(y_max, 10, f = ceiling)) %>%
  #ungroup()
  
  #})

mainland <- droplevels(
  filter(inductions_small_multiples_data, 
                   HBGROUP == "mainland"
  )
  )

islands <- droplevels(
  filter(inductions_small_multiples_data, 
         HBGROUP == "island"
  )
  
)

creates_overview_charts_without_median <- function(plotdata,
                                                   measure_value,
                                                   hover = "mytext",
                                                   yaxislabel = "Percentage of births (%)",
                                                   nrows,
                                                   heights){
  
  y_max <- unique(plotdata$y_max) # allows a margin to be set around y-axis
  
  xaxis_plots <- orig_xaxis_plots
  # xaxis_plots[["mirror"]] <- "all"
  # xaxis_plots[["showline"]] <- TRUE
  #xaxis_plots[["showticklabels"]] <- if_else(plotdata$HBGROUP == "island", TRUE, FALSE)
  xaxis_plots[["dtick"]] <- case_when(plotdata$period == "Q" ~ "3",
                                      TRUE ~ "M6") # frequency of tick marks on x-axis
  
  yaxis_plots <- orig_yaxis_plots
  # yaxis_plots[["mirror"]] <- "all"
  # yaxis_plots[["showline"]] <- TRUE
  yaxis_plots[["range"]] <- list(0, y_max * 1.05) # expands the y-axis range to prevent cut-offs
  
  overview <- plotdata %>% 
    split(.$hbname2) %>% 
    lapply(
      function(d)
        plot_ly(
          d,
          x = ~ date,
          y = ~ measure_value,
          type = "scatter",
          mode = "lines+markers",
          line = list(color = "black", # black lines
                      width = 1),
          marker = list(color = "black", # black dots
                        size = 5),
          hovertext = ~ mytext,
          hoverinfo = "text"
        ) %>%
        layout(
          font = plotly_global_font,
          xaxis = xaxis_plots,
          yaxis = yaxis_plots,
          showlegend = FALSE,
          annotations = list(
            x = 0.5,
            y = 1.0,
            text = ~ unique(hbname2),
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          )
        )
    )
  
  overview <- overview %>%
    subplot(nrows = nrows,
            heights = heights,
            margin = c(0.01, 0.01, 0.05, 0.02),
            shareX = TRUE,
            shareY = TRUE) %>%
    layout(
      annotations = list(
        text = ~ case_match(
          first(plotdata$measure),
          "TEARS" ~ "Percentage of women (%)",
          "APGAR5" ~ "Percentage of babies (%)",
          .default = yaxislabel
          ),
        font = list(size = 14),
        x = 0,
        y = 0.5,
        #standoff = 30, # distance between axis title and tick labels
        xshift = -60,
        textangle = 270,
        showarrow = FALSE,
        xref = "paper",
        yref = "paper"
      )
    ) %>%
  config(displaylogo = F, displayModeBar = FALSE)
  
  return(overview)
}

      
creates_overview_charts_without_median(mainland, nrows = 4, heights = c(0.2, 0.2, 0.2, 0.2))
creates_overview_charts_without_median(islands, nrows = 1, heights = 0.15)


# # using purrr::map
# mtcars %>% 
#   split(mtcars$cyl) %>% 
#   map(~{
#     plot_ly(data = .x, 
#             x = rownames(.x), 
#             y = .x$mpg, 
#             type = "bar")
#   }) %>% 
#   subplot(margin = .05)
# 
# # using lapply
# mtcars %>% 
#   split(mtcars$cyl) %>% 
#   lapply(function(x) {
#     plot_ly(data = x, 
#             x = rownames(x), 
#             y = ~mpg, 
#             type = "bar")
#   }) %>% 
#   subplot(margin = .05)
# 

creates_overview_charts_without_median(inductions_small_multiples_data)


inductions_small_multiples_data1 <- inductions_small_multiples_data %>% split(.$HBGROUP)

inductions_small_multiples_data2 <- inductions_small_multiples_data %>% split(.$hbname2)

inductions_small_multiples_data3 <- inductions_small_multiples_data %>% nest_by(.$HBGROUP, .$hbname2)
