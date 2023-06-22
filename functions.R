#define function "percentage"

percentage = function(x, y, na.rm = TRUE) {
  x / y * 100
}

#Function to create empty plot when no data available
# Parameters:
# height_plot: height of the empty chart in pixels
# text_nodata: What text will show when no data is available

plot_nodata <- function(height_plot = 450, text_nodata) {
  text_na <- list(align = "centre",
                  text = text_nodata,
                  xref = "x",
                  yref = "y",
                  showarrow = FALSE,
                  name = "_no_data_plot_marker_") # so can check later if plot object has no data

  plot_ly(height = height_plot) %>%
    add_trace(x = 0,
              y = 0,
              visible = FALSE,
              type = "scatter",
              mode = "lines") %>%
    layout(annotations = text_na,
           #empty layout
           yaxis = list(showline = FALSE,
                        showticklabels = FALSE,
                        showgrid = FALSE,
                        fixedrange = TRUE),
           xaxis = list(showline = FALSE,
                        showticklabels = FALSE,
                        showgrid = FALSE,
                        fixedrange = TRUE),
           font = list(size = 14)) %>%
    config(displayModeBar = FALSE) # taking out plotly logo and collaborate button
}

creates_overview_charts_with_median <- function(plotdata,
                                                measure = "MEASURE",
                                                hover = "mytext",
                                                centreline = "MEDIAN",
                                                dottedline = "EXTENDED",
                                                yaxislabel = yaxislabel){
  
  y_max <- max(plotdata$MEASURE)

  xaxis_plots <- orig_xaxis_plots
  xaxis_plots[["showticklabels"]] <- if_else(plotdata$HBNAME %in% island_names, TRUE, FALSE)
  xaxis_plots[["dtick"]] <-  case_when(plotdata$PERIOD == "Q" ~ "2",
                                       TRUE ~ "M6")
  yaxis_plots <- orig_yaxis_plots
  yaxis_plots[["range"]] <- list(-1.0, y_max * 1.05)
  
 
  
  # annotations
  a <- list(
    x = max(plotdata$DATE), 
    y = 10,
    xanchor = 'left',
    yanchor = 'middle',
    text = " 10 weeks",
    font = list(family = 'Arial',
                size = 14,
                color = phs_colours("phs-blue")),
    showarrow = FALSE
  )
  
  if(first(plotdata$INDICATOR) == "GESTATION AT BOOKING"){
    
    overview <- plotdata %>%
      split(.$HBNAME2) %>% 
      lapply(
        function(d)
        plot_ly(
            d,
            x = ~ DATE,
            y = ~ get(centreline), # solid blue line
            type = "scatter",
            mode = "lines",
            line = list(color = phs_colours("phs-blue"),
                        width = 1),
            hovertext = ""
          ) %>%
          add_trace(
            y = ~ get(dottedline), # dotted blue line
            type = "scatter",
            mode = "lines",
            line = list(
              color = phs_colours("phs-blue"),
              width = 1,
              dash = "4"
            ),
            hovertext = ""
          ) %>%
          add_trace(
            y = ~ get(measure),
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "black", # black lines
                        width = 1),
            marker = list(color = "black", # black dots
                          size = 5),
            hovertext = ~ get(hover),
            hoverinfo = "text"
          ) %>%
          add_trace(
            x = ~ max(plotdata$DATE), 
            y = 10,
            type = 'scatter',
            mode = "lines+markers", # dot for 10 weeks
            line = list(opacity = 0),
            marker = list(color = phs_colours("phs-blue"),
                          size = 5),
            hovertext = ""
          ) %>%
            add_trace(
            data = filter(gest_at_booking_small_multiples_data,!is.na(NEW_MEDIAN)),
            y = ~ NEW_MEDIAN, # green line (where applicable)
            type = "scatter",
            mode = "lines",
            line = list(
              color = phs_colours("phs-green"),
              width = 1
            ),
            marker = NULL,
            name = ~ paste0(case_when(
              HBNAME == "NHS Tayside" ~
                paste0(HBNAME, " average from Aug 2020 to end Dec 2020"),
              HBNAME == "NHS Forth Valley" ~
                paste0(HBNAME, " average from Mar 2021 to end Jun 2021"),
              TRUE ~ ""
            )
            ),
            legendrank = 400,
            hovertext = ""
            ) %>%
            add_trace(
              data = filter(gest_at_booking_small_multiples_data,!is.na(NEW_EXTENDED)),
              y = ~ NEW_EXTENDED, # dotted green line (where applicable)
              type = "scatter",
              mode = "lines",
              line = list(
                color = phs_colours("phs-green"),
                width = 1,
                dash = "4"
              ),
              marker = NULL,
              name = ~ paste0(case_when(
                HBNAME == "NHS Tayside" ~
                  paste0(HBNAME, " projected average from Jan 2021"),
                HBNAME == "NHS Forth Valley" ~
                  paste0(HBNAME, " projected average from Jul 2021"),
                TRUE ~ ""
              )
              ),
              legendrank = 500,
              hovertext = ""
              ) %>% 
          layout(annotations = a) %>%
          layout(
            #font = list(size = 12),
            xaxis = xaxis_plots,
            yaxis = yaxis_plots,
            showlegend = FALSE,
            annotations = list(
              x = 0.5,
              y = 1.0,
              text = ~ unique(HBNAME2),
              xref = "paper",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            )
          )
      )
    
  } else {
    overview <- plotdata %>% 
      split(.$HBNAME2) %>% 
      lapply(
        function(d)
          plot_ly(
            d,
            x = ~ DATE,
            y = ~ get(centreline), # solid blue line
            type = "scatter",
            mode = "lines",
            line = list(color = phs_colours("phs-blue"),
                        width = 1),
            hovertext = ""
          ) %>%
          add_trace(
            y = ~ get(dottedline), # dotted blue line
            type = "scatter",
            mode = "lines",
            line = list(
              color = phs_colours("phs-blue"),
              width = 1,
              dash = "4"
            ),
            hovertext = ""
          ) %>%
          add_trace(
            y = ~ get(measure),
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "black", # black dots
                        width = 1),
            marker = list(color = "black", # black lines
                          size = 5),
            hovertext = ~ get(hover),
            hoverinfo = "text"
          ) %>%
          layout(
            #font = list(size = 12),
            xaxis = xaxis_plots,
            yaxis = yaxis_plots,
            showlegend = FALSE,
            annotations = list(
              x = 0.5,
              y = 1.0,
              text = ~ unique(HBNAME2),
              xref = "paper",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            )
          )
      )
  }
  
  overview <- overview %>%   
    subplot(nrows = 5,
            heights = c(0.15, 0.2, 0.2, 0.2, 0.18),
            margin = c(0.01, 0.01, 0.05, 0.02),
            shareX = TRUE,
            shareY = TRUE) %>%
    layout(
      annotations = list(
        x = 0,
        y = 0.5,
        text = ~ yaxislabel,
        xshift = -50,
        textangle = 270,
        showarrow = FALSE,
        xref = "paper",
        yref = "paper"
      )
    ) %>% 
    config(displaylogo = F, displayModeBar = FALSE)
  
  return(overview)
}

creates_overview_charts_without_median <- function(plotdata,
                                                   measure = "MEASURE",
                                                   hover = "mytext",
                                                   yaxislabel = "Percentage of births (%)"){
  
  y_max <- max(plotdata$MEASURE) # allows a margin to be set around y-axis
  
  xaxis_plots <- orig_xaxis_plots
  
  #xaxis_plots[["showticklabels"]] <- if_else(plotdata$HBNAME %in% island_names, TRUE, FALSE)
  
  xaxis_plots[["dtick"]] <- case_when(plotdata$PERIOD == "Q" ~ "3",
                                      TRUE ~ "M6") # frequency of tick marks on x-axis

  #xaxis_plots[["tickangle"]] <- -45
  
  yaxis_plots <- orig_yaxis_plots
  yaxis_plots[["range"]] <- list(0, y_max * 1.05) # expands the y-axis range to prevent cut-offs
  
  # annotations
  a <- list( # this annotation is used on the BOOKINGS small multiples to highlight 10 weeks
    x = max(plotdata$DATE), 
    y = 10,
    xanchor = 'left',
    yanchor = 'middle',
    text = " 10 weeks",
    font = list(family = 'Arial',
                size = 14,
                color = phs_colours("phs-blue")),
    showarrow = FALSE
  )
  
  if(first(plotdata$INDICATOR) == "GESTATION AT BOOKING"){ # adds annotation at 10 weeks
    
    overview <- plotdata %>%
      split(.$HBNAME2) %>% 
      lapply(
        function(d)
          plot_ly(
            d,
            x = ~ DATE,
            y = ~ get(measure),
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "black", # black lines
                        width = 1),
            marker = list(color = "black", # black dots
                          size = 5),
            hovertext = ~ get(hover),
            hoverinfo = "text"
          ) %>%
          add_trace(
            x = ~ max(plotdata$DATE),
            y = 10,
            type = 'scatter',
            mode = "lines+markers",
            line = list(opacity = 0),
            marker = list(color = phs_colours("phs-blue"),
                          size = 5),
            hovertext = ""
          )
        %>% 
          layout(annotations = a) %>%
          layout(
            #font = list(size = 12),
            xaxis = xaxis_plots,
            yaxis = yaxis_plots,
            showlegend = FALSE,
            annotations = list(
              x = 0.5,
              y = 1.0,
              text = ~ unique(HBNAME2),
              xref = "paper",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            )
          )
      )
  } else {
    
    overview <- plotdata %>% 
      split(.$HBNAME2) %>% 
      lapply(
        function(d)
          plot_ly(
            d,
            x = ~ DATE,
            y = ~ get(measure),
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "black", # black lines
                        width = 1),
            marker = list(color = "black", # black dots
                          size = 5),
            hovertext = ~ get(hover),
            hoverinfo = "text"
          ) %>%
          layout(
            #font = list(size = 12),
            xaxis = xaxis_plots,
            yaxis = yaxis_plots,
            showlegend = FALSE,
            annotations = list(
              x = 0.5,
              y = 1.0,
              text = ~ unique(HBNAME2),
              xref = "paper",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            )
          )
      )
  }
  
  overview <- overview %>%
    subplot(nrows = 5,
            heights = c(0.15, 0.2, 0.2, 0.2, 0.18),
            margin = c(0.01, 0.01, 0.05, 0.02), 
            shareX = TRUE,
            shareY = TRUE) %>% 
    layout(
      annotations = list(
        x = 0,
        y = 0.5,
        text = ~ if_else(first(plotdata$INDICATOR) == "TEARS", 
                         "Percentage of women (%)", yaxislabel),
        xshift = -50,
        textangle = 270,
        showarrow = FALSE,
        xref = "paper",
        yref = "paper"
      )
    ) %>% 
    config(displaylogo = F, displayModeBar = FALSE)
  
  return(overview)
}

creates_runcharts <- function(plotdata,
                              measure = "MEASURE",
                              hover = "mytext",
                              centreline = "MEDIAN",
                              dottedline = "EXTENDED",
                              trend = "orig_trend",
                              shift = "orig_shift",
                              yaxislabel = "Percentage of births (%)"){
  
  y_max <- max(plotdata$MEASURE, na.rm = TRUE) # allows a margin to be set around y-axis
  
  # include_legend = TRUE for ONE of multiple runcharts (otherwise the legends get repeated) 
  # need to see if a different method can utilise the subgroup function (will need to reformat the
  # dataframe fed into plotly)

  include_legend <- case_when(
    first(plotdata$INDICATOR) == "TYPE OF BIRTH" &
      first(plotdata$INDICATOR_CAT) != "spontaneous vaginal births" ~ FALSE,
    first(plotdata$INDICATOR) == "GESTATION AT BIRTH" &
      first(plotdata$INDICATOR_CAT) != ">= 32 and <= 36 weeks" ~ FALSE,
    TRUE ~ TRUE)
  
  # include_trend_shift_legend = TRUE ensures that the shift and trend legends appear even when the 
  # chart "linked" to the legend doesn't have any shifts or trends 
  
  include_trend_shift_legend <- case_when(
    first(plotdata$INDICATOR_CAT) == "spontaneous vaginal births" ~ FALSE,
          first(plotdata$INDICATOR_CAT) == ">= 32 and <= 36 weeks" ~ FALSE,
    TRUE ~ include_legend)
  
  # ensures ticks and tick labels correspond (different for ABC, TERMINATIONS, SMR02)
  
   select_date_tickvals <- switch( # tells plotly where ticks will show
   first(plotdata$INDICATOR), 
   "BOOKINGS" = bookings_date_tickvals,
   "GESTATION AT BOOKING" = bookings_date_tickvals,
   "TERMINATIONS" = terminations_date_tickvals,
   "GESTATION AT TERMINATION" = terminations_date_tickvals,
   "INDUCTIONS" = SMR02_date_tickvals,
   "TYPE OF BIRTH" = SMR02_multiples_date_tickvals,
   "TEARS" = SMR02_date_tickvals,
   "GESTATION AT BIRTH" = SMR02_multiples_date_tickvals,
   "APGAR5" = SMR02_date_tickvals
   ) 
  
  select_date_ticktext <- switch( # telss plotly what text to show on ticks
   first(plotdata$INDICATOR), 
   "BOOKINGS" = bookings_date_ticktext,
   "GESTATION AT BOOKING" = bookings_date_ticktext,
   "TERMINATIONS" = terminations_date_ticktext,
   "GESTATION AT TERMINATION" = terminations_date_ticktext,
   "INDUCTIONS" = SMR02_date_ticktext,
   "TYPE OF BIRTH" = SMR02_multiples_date_ticktext,
   "TEARS" = SMR02_date_ticktext,
   "GESTATION AT BIRTH" = SMR02_multiples_date_ticktext,
   "APGAR5" = SMR02_date_ticktext
   )

  xaxis_plots <- orig_xaxis_plots
  xaxis_plots[["tickmode"]] <- "array"
  xaxis_plots[["tickvals"]] <- select_date_tickvals
  xaxis_plots[["ticktext"]] <- select_date_ticktext

  yaxis_plots <- orig_yaxis_plots
  yaxis_plots[["range"]] <- list(0, y_max * 1.05) # expands the y-axis range to prevent cut-offs
  
runcharts <-
    plot_ly(
      data = plotdata,
      x = ~ DATE,
      y = ~ get(measure),
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "black", # black dots
                  width = 1),
      marker = list(color = "black", # black lines
                    size = 5),
      name = ~ if_else(first(plotdata$INDICATOR) %in% c("TYPE OF BIRTH", "GESTATION AT BIRTH"),
                     "percentage of births (%)",
                     str_to_lower(var_label(get(measure)))),
      #legendgroup = "measure"
      #legendrank = 100,
      showlegend = include_legend,
      hovertext = ~ get(hover),
      hoverinfo = "text"
      # height = plot_height
    ) %>%
    add_trace(
      y = ~ get(centreline), # solid blue line
      type = "scatter",
      mode = "lines",
      line = list(color = phs_colours("phs-blue"),
                  width = 1),
      marker = NULL,
      name = ~ paste0(var_label(get(centreline))), # retrieves label of variable
      #legendgroup = "median"
      #legendrank = 200,
      showlegend = ~ include_legend,
      hovertext = ""
    ) %>%
    add_trace(
      y = ~ get(dottedline), # dotted blue line
      type = "scatter",
      mode = "lines",
      line = list(
        color = phs_colours("phs-blue"),
        width = 1,
        dash = "4"
      ),
      marker = NULL,
      name = ~ paste0(var_label(get(dottedline))), # retrieves label of variable
      #legendgroup = "extended"
      #legendrank = 300, 
      showlegend = ~ include_legend,
      hovertext = ""
    ) %>%
    add_trace(
      data = plotdata %>% filter_at(trend,
                                    all_vars(. == TRUE)), # green squares
      mode = "markers",
      marker = list(
        color = "green",
        size = 7.5,
        symbol = "square"
      ),
      name = orig_trend_label,
      legendgroup = "trend", 
      legendrank = 1003,
      showlegend = ~ include_trend_shift_legend,
      line = NULL,
      hovertext = ""
    ) %>%
    add_trace(
      data = plotdata %>% filter_at(shift,
                                    all_vars(. == TRUE)), # orange circles
      mode = "markers",
      marker = list(
        color = "orange",
        size = 6.5,
        symbol = "circle"
      ),
      name = orig_shift_label,
      legendgroup = "shift", 
      legendrank = 1004,
      showlegend = ~ include_trend_shift_legend,
      line = NULL,
      hovertext = ""
    ) %>%
    layout(
      annotations = list(
        x = 0,
        y = 0.5,
        text = ~ if_else(first(plotdata$INDICATOR) == "TEARS", 
                               "Percentage of women (%)", yaxislabel),
        xshift = -50,
        textangle = 270,
        showarrow = FALSE,
        xref = "paper",
        yref = "paper"
        ),
      xaxis = xaxis_plots,
      yaxis = yaxis_plots,
      legend = list(title = list(text = paste0(plotdata$HBNAME, "<br>")),
                    orientation = "v",
                    x = 1.0,
                    y = 0.5,
                    xref = "paper",
                    yref = "paper",
                    xanchor = "left",
                    itemclick = FALSE)
                    # groupclick = "togglegroup") 
    ) %>% 
    config(displaylogo = F, displayModeBar = FALSE)

# adds "dummy" traces for multiple runcharts to force shift and trend legends to appear even if there
# are none in these charts
  
if(first(plotdata$INDICATOR_CAT) %in% c("spontaneous vaginal births",
                                        ">= 32 and <= 36 weeks")) {
  runcharts <- runcharts %>%
    add_trace(
    data = plotdata,
    x = ~ min(DATE), # fake trend to show legend even when no trend exists on chart
    y = ~ -5,
    mode = "markers",
    marker = list(
      color = "green",
      size = 7.5,
      symbol = "square"
    ),
    name = orig_trend_label, # retrieves label of variable
    legendgroup = "trend", 
    #legendrank = 600,
    showlegend = TRUE,
    line = NULL,
    hovertext = ""
    ) %>%
    add_trace(
      data = plotdata,
      x = ~ max(DATE), # fake shift to show legend even when no trend exists on chart
      y = ~ -5,
      mode = "markers",
      marker = list(
        color = "orange",
        size = 6.5,
        symbol = "circle"
      ),
      name = orig_shift_label, # retrieves label of variable
      legendgroup = "shift", 
      #legendrank = 700,
      showlegend = TRUE,
      line = NULL,
      hovertext = ""
    )
}

# additional traces for the "special" Boards in GESTATION AT BOOKING indicator
  
  if(first(plotdata$INDICATOR) == "GESTATION AT BOOKING" & 
     first(plotdata$HBNAME) %in% c("NHS Forth Valley", "NHS Tayside")) {
    
    runcharts <- runcharts %>%
      add_trace(
        data = filter(plotdata,!is.na(NEW_MEDIAN)),
        y = ~ NEW_MEDIAN, # green line 
        type = "scatter",
        mode = "lines",
        line = list(
          color = phs_colours("phs-green"),
          width = 1
        ),
        marker = NULL,
        name = ~ case_when(
          HBNAME == "NHS Forth Valley" ~
            paste0("average gestation from Mar 2021", "<br>", "to end Feb 2022"),
          HBNAME == "NHS Tayside" ~
            paste0("average gestation from Aug 2020", "<br>", "to end Jul 2021"),
          TRUE ~ ""
        ),
        legendrank = 1001,
        #legendgroup = "additional median"
        hovertext = ""
      ) %>%
      add_trace(
        data = filter(plotdata,!is.na(NEW_EXTENDED)),
        y = ~ NEW_EXTENDED, # dotted green line 
        type = "scatter",
        mode = "lines",
        line = list(
          color = phs_colours("phs-green"),
          width = 1,
          dash = "4"
        ),
        marker = NULL,
        name = ~ paste0(case_when(
          HBNAME == "NHS Forth Valley" ~ "projected average gestation from Mar 2022",
          HBNAME == "NHS Tayside" ~ "projected average gestation from Aug 2021",
          TRUE ~ ""
        )
        ),
        legendrank = 1002,
        # legendgroup = "additional extended"
        hovertext = ""
      )
  }
#}
  
  return(runcharts)
}

builds_download_data <- function(indicator) {
  
  downloaddata <- download_dataframe[[{{indicator}}]] 
    
  return(downloaddata)
  
  }
