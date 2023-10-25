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
  
  data <- apgar5_data %>%
    filter(hbtype == Selected$HBType & period == "Q") %>%
    set_variable_labels(
      measure = "Percentage of births following induction (%)",
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
                         format(measure,
                                digits = 1,
                                nsmall = 1),
                         "%"),
        date = quarter_label,
        HBGROUP = if_else(hbname %in% island_names, "island", "mainland")
        #xshowticklabels = if_else(hbname %in% island_names, TRUE, FALSE)
  ) %>% 
  group_by(HBGROUP, hbtype) %>% 
  mutate(y_max = max(measure)) %>% 
         #y_max = plyr::round_any(y_max, 10, f = ceiling)) %>%
  ungroup()

plotly_fn <- function(plotdata) {
  
  y_max <- unique(plotdata$y_max) # allows a margin to be set around y-axis
  
  xaxis_plots <- orig_xaxis_plots
  # xaxis_plots[["mirror"]] <- "all"
  # xaxis_plots[["showline"]] <- TRUE
  xaxis_plots[["showticklabels"]] <- if_else(plotdata$HBGROUP == "island", TRUE, FALSE)
  xaxis_plots[["dtick"]] <- case_when(plotdata$period == "Q" ~ "3",
                                      TRUE ~ "M6") # frequency of tick marks on x-axis

  yaxis_plots <- orig_yaxis_plots
  # yaxis_plots[["mirror"]] <- "all"
  # yaxis_plots[["showline"]] <- TRUE
  yaxis_plots[["range"]] <- list(0, y_max * 1.05) # expands the y-axis range to prevent cut-offs
  
  plot_ly(
    plotdata,
    x = ~ date,
    y = ~ measure,
    type = "scatter",
    mode = "lines+markers",
    line = list(color = "black", # black lines
                width = 1),
    marker = list(color = "black", # black dots
                  size = 3),
    hovertext = ~ mytext,
    hoverinfo = "text"
  ) %>%
    layout(
      font = plotly_global_font,
      plot_bgcolor = "#ECEBF3",
      xaxis = xaxis_plots,
      yaxis = yaxis_plots,
      showlegend = FALSE,
      annotations = list(
        x = 0.5,
        y = 0.8,
        text = ~ unique(hbname2),
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      )
    )
}

df_plots <- inductions_small_multiples_data %>% 
  nest_by(.$HBGROUP, .$hbname2) %>%
  mutate(plot = list(plotly_fn(data)))

# output$plotly_inductions_overview_M <- renderPlotly({

inductions1 <- 
  df_plots %>%
    filter(., `.$HBGROUP` == "mainland") %>%
    subplot(nrows = 4,
            shareX = TRUE,
            shareY = TRUE) %>%
    layout(
      annotations = list(
        x = 0,
        y = 0.4,
        text = "Percentage of births (%)",
        xshift = -50,
        textangle = 270,
        showarrow = FALSE,
        xref = "paper",
        yref = "paper"
      )
    )

# output$plotly_inductions_overview_I <- renderPlotly({

inductions2 <- 
  df_plots %>%
    filter(., `.$HBGROUP` == "island") %>%
    subplot(nrows = 1,
            shareX = FALSE,
            shareY = TRUE)

inductions_overview <- 
  subplot(inductions1,
          inductions2,
          heights = c(0.8, 0.2),
          # margin = c(0.01, 0.01, 0.05, 0.02), 
          nrows = 2)


# using purrr::map
mtcars %>% 
  split(mtcars$cyl) %>% 
  map(~{
    plot_ly(data = .x, 
            x = rownames(.x), 
            y = .x$mpg, 
            type = "bar")
  }) %>% 
  subplot(margin = .05)

# using lapply
mtcars %>% 
  split(mtcars$cyl) %>% 
  lapply(function(x) {
    plot_ly(data = x, 
            x = rownames(x), 
            y = ~mpg, 
            type = "bar")
  }) %>% 
  subplot(margin = .05)

