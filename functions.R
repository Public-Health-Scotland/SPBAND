# define function "percentage"

percentage = function(x, y, na.rm = TRUE) {
  x / y * 100
}

# Function to show spinner when loading charts/tables
# Parameter: whats_loading: 
# e.g. plotlyOutput("multi_indicator_chart", height = "33em")

loading <- function(whats_loading){
  
 withSpinner(whats_loading, type = 5, color = "#3F3685", size = 0.5)
}

# Function to read in data and split by indicator, remove redundant columns

load_and_split_dataframe <- function(indicator) {
  
  data <- filter(runchart_dataframe, indicator == {{indicator}}) %>% 
  janitor::remove_empty(., which = c("cols"), quiet = TRUE)
  
  return(data)
}

# Function to create empty plot when no data available
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

# function to make the sidebar menu accessible (https://www.jumpingrivers.com/blog/accessible-shiny-standards-wcag/)

accessible_menu = function(bad_menu) {
  tab_input = tags$script(
    "
function customMenuHandleClick(e) {
  let n = $(e.currentTarget).find('a')[0].dataset.value;
  doSomethingWith(n);
}
function doSomethingWith(val) {
  Shiny.setInputValue('sidebarMenu', val);
}
$(document).ready(
  function() {
    $('ul.sidebar-menu li').click(customMenuHandleClick)
  });
"
  )
  bad_menu$children[[length(bad_menu$children)]] = NULL
  real_menu = tagList(bad_menu, tab_input)
  real_menu
}

# functions to remove aria labels where they are not needed

rem_aria_label <- function(icon) {
  icon[["attribs"]][["aria-label"]] = NULL
  return(icon)
  }

rem_menu_aria_label <- function(menu) {
  menu[["children"]][[1]][["children"]][[3]][["attribs"]][["aria-label"]] = NULL
  return(menu)
}

# Function to create the small multiple charts with a blue median line
# Parameters:
# plotdata: dataframe with data to be plotted
# measure: variable to be plotted as black dots/lines
# hover: hovertext for the measure
# centreline: variable to be plotted as a solid blue line
# dottedline: variable to be plotted as a dotted blue line
# yaxislabel: text to appear on y axis

# This function is not as up-to-date as the next one as it was deemed not required

creates_overview_charts_with_median <- function(plotdata,
                                                measure,
                                                hover = "mytext",
                                                centreline = "median",
                                                dottedline = "extended",
                                                yaxislabel = yaxislabel){
  
  y_max <- max(plotdata$measure)

  xaxis_plots <- orig_xaxis_plots
  xaxis_plots[["showticklabels"]] <- if_else(plotdata$hbname %in% island_names, TRUE, FALSE)
  xaxis_plots[["dtick"]] <-  case_when(plotdata$period == "Q" ~ "2",
                                       TRUE ~ "M6")
  yaxis_plots <- orig_yaxis_plots
  yaxis_plots[["range"]] <- list(-1.0, y_max * 1.05)
  
  # annotations - plots a single blue dot at 10 weeks on last data point for
  # AVERAGE GESTATION AT BOOKING only
  
  a <- list(
    x = max(plotdata$date), 
    y = 10,
    xanchor = 'left',
    yanchor = 'middle',
    text = " 10 weeks",
    font = list(family = 'Arial',
                size = 14,
                color = phs_colours("phs-blue")),
    showarrow = FALSE
  )
  
  if(first(plotdata$indicator) == "GESTATION AT BOOKING"){
    
    overview <- plotdata %>%
      split(.$hbname2) %>% 
      lapply(
        function(d)
        plot_ly(
            d,
            x = ~ date,
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
            y = ~ measure,
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
            x = ~ max(plotdata$date), 
            y = 10,
            type = 'scatter',
            mode = "lines+markers", # dot for 10 weeks
            line = list(opacity = 0),
            marker = list(color = phs_colours("phs-blue"),
                          size = 5),
            hovertext = ""
          ) %>%
            add_trace(
            data = filter(gest_at_booking_small_multiples_data,!is.na(new_median)),
            y = ~ NEW_MEDIAN, # green line (where applicable)
            type = "scatter",
            mode = "lines",
            line = list(
              color = phs_colours("phs-green"),
              width = 1
            ),
            marker = NULL,
            name = ~ paste0(case_when(
              hbname == "NHS Tayside" ~
                paste0(hbname, " average from Aug 2020 to end Dec 2020"),
              hbname == "NHS Forth Valley" ~
                paste0(hbname, " average from Mar 2021 to end Jun 2021"),
              TRUE ~ ""
            )
            ),
            legendrank = 400,
            hovertext = ""
            ) %>%
            add_trace(
              data = filter(gest_at_booking_small_multiples_data,!is.na(new_extended)),
              y = ~ new_extended, # dotted green line (where applicable)
              type = "scatter",
              mode = "lines",
              line = list(
                color = phs_colours("phs-green"),
                width = 1,
                dash = "4"
              ),
              marker = NULL,
              name = ~ paste0(case_when(
                hbname == "NHS Tayside" ~
                  paste0(hbname, " projected average from Jan 2021"),
                hbname == "NHS Forth Valley" ~
                  paste0(hbname, " projected average from Jul 2021"),
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
              text = ~ unique(hbname2),
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
      split(.$hbname2) %>% 
      lapply(
        function(d)
          plot_ly(
            d,
            x = ~ date,
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
            y = ~ measure,
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
              text = ~ unique(hbname2),
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

# Function to create the small multiple charts without a blue median line
# Parameters:
# plotdata: dataframe with data to be plotted
# measure: variable to be plotted as black dots/lines
# hover: hovertext for the measure
# yaxislabel: text to appear on y axis

creates_overview_charts_without_median <- function(plotdata,
                                                   measure,
                                                   hover = "mytext",
                                                   yaxislabel = "Percentage of births (%)"){
  
  y_max <- max(plotdata$measure) # allows a margin to be set around y-axis
  
  xaxis_plots <- orig_xaxis_plots

  xaxis_plots[["dtick"]] <- case_when(plotdata$period == "Q" ~ "3",
                                      TRUE ~ "M6") # frequency of tick marks on x-axis

  yaxis_plots <- orig_yaxis_plots
  yaxis_plots[["range"]] <- list(0, y_max * 1.05) # expands the y-axis range to prevent cut-offs
  # yaxis_plots[["title"]] <- list(
  #   standoff = 30) # distance between axis title and tick labels
  
  # annotations - plots a single blue dot at 10 weeks on last data point for
  # AVERAGE GESTATION AT BOOKING only
  
  a <- list(
    x = max(plotdata$date), 
    y = 10,
    xanchor = 'left',
    yanchor = 'middle',
    text = " 10 weeks",
    font = list(family = 'Arial',
                size = 14,
                color = phs_colours("phs-blue")),
    showarrow = FALSE
  )
  
  if(first(plotdata$indicator) == "GESTATION AT BOOKING"){ # adds annotation at 10 weeks
    
    overview <- plotdata %>%
      split(.$hbname2) %>% 
      lapply(
        function(d)
          plot_ly(
            d,
            x = ~ date,
            y = ~ measure,
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
            x = ~ max(plotdata$date),
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
  } else {
    
    overview <- plotdata %>% 
      split(.$hbname2) %>% 
      lapply(
        function(d)
          plot_ly(
            d,
            x = ~ date,
            y = ~ measure,
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
  }
  
  overview <- overview %>%
    subplot(nrows = 5,
            heights = c(0.15, 0.2, 0.2, 0.2, 0.18),
            margin = c(0.01, 0.01, 0.05, 0.02), 
            shareX = TRUE,
            shareY = TRUE) %>%
    layout(
      annotations = list(
        text = ~ case_match(
          first(plotdata$indicator),
          "TEARS" ~ "Percentage of women (%)",
          c("GESTATION AT BIRTH", "APGAR5") ~ "Percentage of babies (%)",
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

# Function to create the runcharts/timeseries charts
# Parameters:
# plotdata: dataframe with data to be plotted
# measure: variable to be plotted as black dots/lines
# hover: hovertext for the measure
# centreline: variable to be plotted as a solid blue line
# dottedline: variable to be plotted as a dotted blue line
# trend: green squares for 5 or more points going up or going down
# shift: orange circles for 6 or more points above or below the median
# yaxislabel: text to appear on y axis

creates_runcharts <- function(plotdata,
                              measure,
                              hover = "mytext",
                              centreline = "median",
                              dottedline = "extended",
                              #trend = "orig_trend",
                              #shift = "orig_shift",
                              yaxislabel = "Percentage of births (%)"){
  
  y_max <- max(plotdata$measure, na.rm = TRUE) # allows a margin to be set around y-axis
  
  # temp fix
  
  # plotdata <-  
  #   plotdata %>% 
  #   mutate(shift = if_else(orig_shift == TRUE, measure, NA),
  #          trend = if_else(orig_trend == TRUE, measure, NA)
  #   )
  
  # include_legend = TRUE for ONE of multiple runcharts (otherwise the legends get repeated) 
  # need to see if a different method can utilise the subgroup function (will need to reformat the
  # dataframe fed into plotly)

  include_legend <- case_when(
    first(plotdata$indicator) == "TYPE OF BIRTH" &
      first(plotdata$indicator_cat) != "spontaneous vaginal births" ~ FALSE,
    first(plotdata$indicator) == "GESTATION AT BIRTH" &
      first(plotdata$indicator_cat) != "between 32 and 36 weeks" ~ FALSE,
    TRUE ~ TRUE)
  
  # include_trend_shift_legend = TRUE ensures that the shift and trend legends appear even when the 
  # chart "linked" to the legend doesn't have any shifts or trends 
  
  include_trend_shift_legend <- case_when(
    first(plotdata$indicator_cat) == "spontaneous vaginal births" ~ FALSE,
          first(plotdata$indicator_cat) == "between 32 and 36 weeks" ~ FALSE,
    TRUE ~ include_legend)
  
  # ensures ticks and tick labels correspond (different for ABC, TERMINATIONS, SMR02)
  
   select_date_tickvals <- switch( # tells plotly where ticks will show
   first(plotdata$indicator), 
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
   first(plotdata$indicator), 
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
  yaxis_plots[["title"]] <- list(
    text = ~ case_match(
      first(plotdata$indicator),
      "TEARS" ~ "Percentage of women (%)",
      c("GESTATION AT BIRTH", "APGAR5") ~ "Percentage of babies (%)",
      .default = yaxislabel
      ),
    standoff = 30) # distance between axis and chart
  yaxis_plots[["tickformat"]] <- 
    if_else(first(plotdata$indicator) %in% c("APGAR5", "TEARS"),
            ".1f",
            ",d")
  yaxis_plots[["range"]] <- list(0, y_max * 1.05) # expands the y-axis range to prevent cut-offs
  
  runcharts <-
    plot_ly(
    data = plotdata,
    x = ~ date,
    y = ~ trend, # green trend line needs to be plotted first or it obliterates the others
    type = "scatter",
    mode = "lines",
    line = list(
      color = "lightgreen",
      width = 10
    ),
    name = orig_trend_label,
    legendgroup = "trend",
    legendrank = 1003,
    showlegend = ~ include_trend_shift_legend,
    hovertext = "",
    hoverinfo = "none"
  ) %>% 
    add_trace(
      y = ~ measure,
      mode = "lines+markers",
      line = list(
        color = "black", # black lines
        width = 1),
      marker = list(
        color = "black", # black dots
        size = 5),
      name = ~ case_match(
        first(plotdata$indicator),
        "TYPE OF BIRTH" ~ "percentage of births (%)",
        "GESTATION AT BIRTH" ~ "percentage of babies (%)",
        .default = str_to_lower(var_label(measure))
        ),
      legendgroup = "measure",
      legendrank = 100,
      showlegend = include_legend,
      hovertext = ~ mytext,
      hoverinfo = "text"
    ) %>% 
    add_trace(
      y = ~ get(centreline), # solid blue line
      type = "scatter",
      mode = "lines",
      line = list(
        color = phs_colours("phs-blue"),
        width = 1),
      marker = NULL,
      name = ~ paste0(var_label(get(centreline))), # retrieves label of variable
      legendgroup = "median",
      legendrank = 200,
      showlegend = ~ include_legend,
      hovertext = "",
      hoverinfo = "none"
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
      legendgroup = "extended",
      legendrank = 300,
      showlegend = ~ include_legend,
      hovertext = "",
      hoverinfo = "none"
    ) %>%
      add_trace(
      y = ~ shift, # orange lines
      mode = "lines",
      line = list(
        color = "orange", # orange lines (prevents missing data warning)
        width = 2),
      marker = NULL,
      # marker = list(
      #   color = "orange", # orange dots
      #   size = 6,
      #   symbol = "circle"
      # ),
      name = orig_shift_label,
      legendgroup = "shift",
      legendrank = 1004,
      showlegend = ~ include_trend_shift_legend,
      hovertext = "",
      hoverinfo = "none"
    ) %>%
    layout(
      font = plotly_global_font,
      xaxis = xaxis_plots,
      yaxis = yaxis_plots,
      legend = list(title = list(text = paste0(plotdata$hbname, "<br>")),
                    tracegroupgap = 15,
                    orientation = "v",
                    x = 1.0,
                    y = 0.5,
                    xref = "paper",
                    yref = "paper",
                    xanchor = "left",
                    itemclick = FALSE)
    ) %>%
    #config(modeBarButtons = list(list("zoomIn2d"), list("zoomOut2d"), list("pan3d")))
    config(displaylogo = F, displayModeBar = FALSE)

# adds "dummy" traces for multiple runcharts to force shift and trend legends to appear even if there
# are none in these charts

if(first(plotdata$indicator_cat) %in% c("spontaneous vaginal births",
                                        "between 32 and 36 weeks")) {
  runcharts <- runcharts %>%
    add_trace(
    data = plotdata,
    x = ~ min(date), # fake trend to show legend even when no trend exists on chart
    y = ~ -5,
    mode = "lines",
    line = list(
      color = "lightgreen",
      width = 10
    ),
    #marker = NULL,
    name = orig_trend_label, # retrieves label of variable
    legendgroup = "trend",
    legendrank = 600,
    showlegend = TRUE,
    #line = NULL,
    hovertext = "",
    hoverinfo = "none"
    ) %>%
    add_trace(
      data = plotdata,
      x = ~ max(date), # fake shift to show legend even when no shift exists on chart
      y = ~ -5,
      mode = "lines",
      marker = NULL,
      line = list(
        color = "orange", # orange lines (prevents missing data warning)
        width = 2),
      # marker = list(
      #   color = "orange", # orange dots
      #   size = 6,
      #   symbol = "circle"
      # ),
      name = orig_shift_label, # retrieves label of variable
      legendgroup = "shift",
      legendrank = 700,
      showlegend = TRUE,
      #line = NULL,
      hovertext = "",
      hoverinfo = "none"
    )
}

# additional traces for the "special" Boards in GESTATION AT BOOKING indicator

  if(first(plotdata$indicator) == "GESTATION AT BOOKING" &
     first(plotdata$hbname) %in% c("NHS Forth Valley", "NHS Tayside")) {

    runcharts <- runcharts %>%
      add_trace(
        data = filter(plotdata,!is.na(new_median)),
        y = ~ new_median, # green line
        type = "scatter",
        mode = "lines",
        line = list(
          color = phs_colours("phs-green"),
          width = 1
        ),
        marker = NULL,
        name = ~ case_when(
          hbname == "NHS Forth Valley" ~
            paste0("average gestation from Mar 2021", "<br>", "to end Feb 2022"),
          hbname == "NHS Tayside" ~
            paste0("average gestation from Aug 2020", "<br>", "to end Jul 2021"),
          TRUE ~ ""
        ),
        legendrank = 1001,
        legendgroup = "additional median",
        hovertext = ""
      ) %>%
      add_trace(
        data = filter(plotdata,!is.na(new_extended)),
        y = ~ new_extended, # dotted green line
        type = "scatter",
        mode = "lines",
        line = list(
          color = phs_colours("phs-green"),
          width = 1,
          dash = "4"
        ),
        marker = NULL,
        name = ~ paste0(case_when(
          hbname == "NHS Forth Valley" ~ "projected average gestation from Mar 2022",
          hbname == "NHS Tayside" ~ "projected average gestation from Aug 2021",
          TRUE ~ ""
        )
        ),
        legendrank = 1002,
        legendgroup = "additional extended",
        hovertext = ""
      )
  }

  return(runcharts)
}

# Function to create the context charts (overall numbers relevant to the indicator)
# Parameters:
# plotdata: dataframe with data to be plotted
# date: name of the "date" variable (may be "QUARTER" rather than "date")
# num: main indicator variable to be plotted as line (e.g. number of Apgar5 scores < 7)
# num_hover: hovertext for the num
# den: "total" indicator variable to be plotted as a line (e.g. the total number of Apgar5 scores recorded)
# den_hover: hovertext for the den
# yaxislabel: text to appear on y axis

creates_context_charts <- function(plotdata,
                                   date,
                                   num,
                                   num_hover = "mytext1",
                                   den,
                                   den_hover = "mytext2",
                                   yaxislabel = "Number of births"){
  
  y_max <- max(plotdata$den, na.rm = TRUE) # allows a margin to be set around y-axis
  
  # include_legend = TRUE for ONE of multiple runcharts (otherwise the legends get repeated) 
  # need to see if a different method can utilise the subgroup function (will need to reformat the
  # dataframe fed into plotly)

  include_legend <- case_when(
    first(plotdata$indicator) == "TYPE OF BIRTH" &
      first(plotdata$indicator_cat) != "spontaneous vaginal births" ~ FALSE,
    first(plotdata$indicator) == "GESTATION AT BIRTH" &
      first(plotdata$indicator_cat) != ">= 32 and <= 36 weeks" ~ FALSE,
    TRUE ~ TRUE)
  
  # ensures ticks and tick labels correspond (different for ABC, TERMINATIONS, SMR02)
  
   select_date_tickvals <- switch( # tells plotly where ticks will show
   first(plotdata$indicator), 
   "BOOKINGS" = bookings_date_tickvals,
   "GESTATION AT BOOKING" = bookings_date_tickvals,
   "TERMINATIONS" = terminations_date_tickvals,
   "GESTATION AT TERMINATION" = terminations_date_tickvals,
   "EXTREMELY PRE-TERM BIRTHS" = SMR02_date_tickvals,
   "INDUCTIONS" = SMR02_date_tickvals,
   "TYPE OF BIRTH" = SMR02_multiples_date_tickvals,
   "TEARS" = SMR02_date_tickvals,
   "GESTATION AT BIRTH" = SMR02_multiples_date_tickvals,
   "APGAR5" = SMR02_date_tickvals
   ) 
  
  select_date_ticktext <- switch( # tells plotly what text to show on ticks
   first(plotdata$indicator), 
   "BOOKINGS" = bookings_date_ticktext,
   "GESTATION AT BOOKING" = bookings_date_ticktext,
   "TERMINATIONS" = terminations_date_ticktext,
   "GESTATION AT TERMINATION" = terminations_date_ticktext,
   "EXTREMELY PRE-TERM BIRTHS" = SMR02_date_ticktext,
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
  yaxis_plots[["title"]] <- list(
    text = ~ case_match(
      first(plotdata$indicator),
      "TEARS" ~ "Number of women",
      "APGAR5" ~ "Number of babies",
      .default = yaxislabel
      ),
    standoff = 30) # distance between axis and chart

context_charts <-
    plot_ly(
      data = plotdata,
      x = ~ date,
      y = ~ num,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = selected_colours[2], # magenta line with x
                  width = 2),
      marker = list(color = selected_colours[2],
                    symbol = "square-x-open"),
      name = ~ case_match( # retrieves label of variable
        first(plotdata$indicator),
        c("TYPE OF BIRTH", "GESTATION AT BIRTH") ~ "number of births",
        "APGAR5" ~ "babies with an Apgar5 score less than 7",
        "EXTREMELY PRE-TERM BIRTHS" ~ "births at 22-26 weeks in a hospital with a NICU",
      .default = str_to_lower(var_label(num))
      ),
      #legendgroup = "measure"
      legendrank = 200,
      showlegend = include_legend,
      hovertext = ~ get(num_hover),
      hoverinfo = "text"
    ) %>%
    add_trace(
      y = ~ den, # dashed purple line
      type = "scatter",
      mode = "lines+markers",
      line = list(color = selected_colours[1],
                  width = 2),
      marker = list(color = selected_colours[1],
                    symbol = "circle"),
      name = ~ case_match( # retrieves label of variable
        first(plotdata$indicator),
        "APGAR5" ~ "babies with a known Apgar5 score",
        .default = str_to_lower(var_label(den))
        ), 
      #legendgroup = "median"
      legendrank = 100,
      showlegend = ~ include_legend,
      hovertext = ~ get(den_hover),
      hovertext = "text"
    ) %>%
    layout(
      font = plotly_global_font,
      xaxis = xaxis_plots,
      yaxis = yaxis_plots,
      legend = list(
        title = list(text = paste0(plotdata$hbname, "<br>")),
        orientation = "v",
        x = 1.0,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        itemclick = FALSE),
        # groupclick = "togglegroup") 
      margin = list(pad = 30) # distance between axis and first data point
    ) %>% 
    config(displaylogo = F, displayModeBar = FALSE)

  return(context_charts)
}

# Function to build download data
# Parameter: 
# indicator: dataframe to be downloaded

builds_download_data <- function(indicator) {
  
  downloaddata <- download_dataframe[[{{indicator}}]] 
    
  return(downloaddata)
  
  }
