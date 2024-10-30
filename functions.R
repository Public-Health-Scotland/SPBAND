# Function to show spinner when loading charts/tables
# Parameter: whats_loading: 
# e.g. plotlyOutput("multi_indicator_chart", height = "33em")

loading <- function(whats_loading){
  
 withSpinner(whats_loading, type = 5, color = "#3F3685", size = 0.5)
}

# Function to read in data and split by measure, remove redundant columns

load_and_split_dataframe <- function(measure) {
  
  data <- filter(runchart_dataframe, measure == {{measure}}) %>% 
  janitor::remove_empty(which = c("cols"), quiet = TRUE)
  
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
# modified because it was causing a lag in the inputId updating (caused problems with filters)
# now there is no lag but it doesn't initialise with the value 'home' - have queried this  witn jumping rivers

accessible_menu = function(bad_menu) {
  tab_input = tags$script(
    "
function customMenuHandleClick(e) {
  let n = $(e.currentTarget).find('a')[0].dataset.value; 
  doSomethingWith(n);
}
function doSomethingWith(val) {
  Shiny.setInputValue('topics', val);
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

# removes aria label from left-hand menu icon "fas fa-angles-right" to satisfy accessibility check

rem_aria_label <- function(icon) {
  icon[["attribs"]][["aria-label"]] = NULL
  return(icon)
  }

# removes aria label from expanded (right-hand) menu icon "fas fa-angle-left pull-right" to satisfy accessibility check

rem_menu_aria_label <- function(menu) {
  menu[["children"]][[1]][["children"]][[3]][["attribs"]][["aria-label"]] = NULL
  return(menu)
}

# removes aria label from icon "fas fa-plus" "fas fa-minus" and adds title to "button" to satisfy accessibility check

rem_button_aria_label <- function(box) {
  box[["children"]][[1]][["children"]][[1]][["children"]][[2]][["children"]][[1]][["children"]][[1]][["attribs"]][["aria-label"]] = NULL
  box[["children"]][[1]][["children"]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]][["title"]] = "open and close button" 
  return(box)
}

# Function to create the small multiple charts with a blue median line
# Parameters:
# plotdata: dataframe with data to be plotted
# measure_value: variable to be plotted as black dots/lines
# hover: hovertext for the measure_value
# centreline: variable to be plotted as a solid blue line
# dottedline: variable to be plotted as a dotted blue line
# yaxislabel: text to appear on y axis

# This function is not as up-to-date as the next one as it was deemed not required

creates_overview_charts_with_median <- function(plotdata,
                                                measure_value,
                                                hover = "mytext",
                                                centreline = "median",
                                                dottedline = "extended",
                                                yaxislabel = yaxislabel){
  
  y_max <- max(plotdata$measure_value)

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
  
  if(first(plotdata$measure) == "GESTATION AT BOOKING"){
    
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
            y = ~ measure_value,
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
            y = ~ measure_value,
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
# measure_value: variable to be plotted as black dots/lines
# hover: hovertext for the measure_value
# yaxislabel: text to appear on y axis

creates_small_multiple_charts_without_median <- function(plotdata,
                                                   measure_value,
                                                   hover = "mytext"){
  
  # sorts plots in correct order (Scotland first)
  # average gestation at termination has hbname2 defined already, the other measures have 
  # hbname2 defined here
  
  # sorts plots in correct order (Scotland first)
  # average gestation at termination has hbname2 defined already, the other measures have 
  # hbname2 defined here
  
  if(!"hbname2" %in% names(plotdata)) {
    plotdata$hbname2 <- factor(plotdata$hbname, levels = HBnames)
  }
  
  plotdata <- droplevels(plotdata) # drop unused factor levels
  
  y_max <- max(plotdata$y_max) # allows a range to be set for y-axis
  
  # sets y-axis label
  
  yaxislabeltext  <- case_match(
    first(plotdata$measure),
    "GESTATION AT BOOKING" ~ "Average gestation at booking (weeks)",
    "GESTATION AT TERMINATION" ~ paste(strwrap("Average gestation at termination (weeks)",
                                               width = 25
                                         ),
                                       collapse = "\n"),
    "TEARS" ~ "Percentage of women (%)",
    "APGAR5" ~ "Percentage of babies (%)",
    .default = "Percentage of births (%)"
  )
  
  # sets whether y-axis label should be shown (prevents double-printing)
  
  yaxislabelswitch <- if_else(
    (first(plotdata$measure) == "GESTATION AT TERMINATION" | first(plotdata$hbgroup) == "mainland"),
    yaxislabeltext, "")
  
  # sets where y-axis label should be positioned
  
  yaxislabelposition <- if_else(
    first(plotdata$measure) == "GESTATION AT TERMINATION",
    0.5, 0.4)

  xaxis_plots <- orig_xaxis_plots
  
  # xaxis_plots[["range"]] <- range(unique(plotdata$date))

  xaxis_plots[["dtick"]] <- case_when(plotdata$period == "Q" ~ "3",
                                      TRUE ~ "M6") # frequency of tick marks on x-axis
  
  # remove x-axis ticklabels from "mainland" charts except for Average Gestation at Termination
  
  xaxis_plots[["showticklabels"]] <- if_else(
    first(plotdata$hbgroup) == "mainland" && first(plotdata$measure != "GESTATION AT TERMINATION"),
    FALSE, TRUE)

  yaxis_plots <- orig_yaxis_plots
  
  yaxis_plots[["tickmode"]] <- "auto"

  #yaxis_plots[["nticks"]] <- 4
  
  yaxis_plots[["range"]] <- list(0, y_max * 1.05) # expands the y-axis range to prevent cut-offs
  
  plot_heights = if(first(plotdata$hbgroup) == "mainland") {
    
    c(0.2, 0.25, 0.25, 0.2)} else {0.75}
  
  if(first(plotdata$measure) == "GESTATION AT BOOKING") { # adds annotation at 10 weeks, no markers
    
    # annotations - plots a single blue dot at 10 weeks on last data point for
  # AVERAGE GESTATION AT BOOKING only
  
  a <- list(
    x = max(plotdata$date) + months(1), 
    y = 11.9,
    xref ="x",
    yref = "y",
    xanchor = 'right',
    yanchor = 'middle',
    text = "10 weeks ",
    font = list(family = 'Arial',
                size = 12,
                color = phs_colours("phs-blue")),
    showarrow = FALSE
  )
    
    overview <- plotdata %>%
      split(.$hbname2) %>% 
      lapply(
        function(d)
          plot_ly(
            d,
            x = ~ date
          ) %>% 
          add_lines(
            y = ~ measure_value,
            line = list(color = "black", # black lines
                        width = 1),
            hovertext = ~ get(hover),
            hoverinfo = "text",
            color = I("black"),
            showlegend = FALSE,
          ) %>%
          add_markers(
            x = ~ max(plotdata$date) + months(1),
            y = 10,
            marker = list(color = phs_colours("phs-blue"),
                          size = 6),
            hoverinfo = "none",
            showlegend = FALSE
          ) %>% 
          layout(annotations = a) %>%
          layout(
            font = plotly_global_font,
            xaxis = xaxis_plots,
            yaxis = yaxis_plots,
            #plot_bgcolor='#ecebf3', 
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
    
  } else if(first(plotdata$measure) == "GESTATION AT TERMINATION") { # no markers
    
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
            marker = list(opacity = 0),
            hovertext = ~ get(hover),
            hoverinfo = "text",
            color = I("black")
          ) %>%
          layout(
            font = plotly_global_font,
            xaxis = xaxis_plots,
            yaxis = yaxis_plots,
            #plot_bgcolor='#ecebf3',
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
            y = ~ measure_value,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "black", # black lines
                        width = 1),
            marker = list(color = "black", # black dots
                          size = 5),
            hovertext = ~ get(hover),
            hoverinfo = "text",
            color = I("black")
          ) %>%
          layout(
            font = plotly_global_font,
            xaxis = xaxis_plots,
            yaxis = yaxis_plots,
            #plot_bgcolor='#ecebf3',
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
    subplot(nrows = if_else(first(plotdata$hbgroup) == "mainland", 4, 1),
            heights = plot_heights,
            margin = c(0.01, 0.01, 0.05, 0.05), # gives more room between plots
            shareX = TRUE,
            shareY = TRUE) %>% 
    layout(annotations = list(text = ~ yaxislabelswitch,
                              font = list(size = 14),
                              x = 0,
                              y = ~ yaxislabelposition,
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

# Function to run the small multiples charts code (for layout)
# Parameters:
# plotdata: dataframe with data to be plotted
# calls creates_small_multiple_charts_without_median
# produces either one chart or two charts in a list (GESTATION AT TERMINATION)

subplot_mainland_island_small_multiples <- function(plotdata) {
  
  if(first(plotdata$measure != "GESTATION AT TERMINATION")) {
    
    small_multiple_subplot <- 
      plotdata %>% 
      split(.$hbgroup) %>% 
      map(\(df) creates_small_multiple_charts_without_median(df)) %>% 
      subplot(nrows = 2, heights = c(0.8, 0.2), margin = c(0.01, 0.01, 0.05, 0.01)) # seems to give 
    # separation between mainland and island plots

  } else {

    small_multiple_subplot <-
      plotdata %>%
      split(.$hbgroup) %>%
      map(\(df) creates_small_multiple_charts_without_median(df))

  }
  
  return(small_multiple_subplot)
}

# Function to create the runcharts/timeseries charts
# Parameters:
# plotdata: dataframe with data to be plotted
# measure_value: variable to be plotted as black dots/lines
# hover: hovertext for the measure_value
# centreline: variable to be plotted as a solid blue line
# dottedline: variable to be plotted as a dotted blue line
# trend: green squares for 5 or more points going up or going down
# shift: orange circles for 6 or more points above or below the median
# yaxislabel: text to appear on y axis

creates_runcharts <- function(plotdata,
                              measure_value,
                              hover = "mytext",
                              centreline = "pre_pandemic_median",
                              dottedline = "extended_pre_pandemic_median",
                              yaxislabel = "Percentage of births (%)"){
  
  plotdata <- droplevels(plotdata) # drop unused factor levels
  
  y_max <- max(plotdata$measure_value, na.rm = TRUE) # allows a margin to be set around y-axis
  
  # temp fix
  
  # plotdata <-  
  #   plotdata %>% 
  #   mutate(shift = if_else(orig_shift == TRUE, measure_value, NA),
  #          trend = if_else(orig_trend == TRUE, measure_value, NA)
  #   )
  
  # include_legend = TRUE for ONE of multiple runcharts (otherwise the legends get repeated) 
  # need to see if a different method can utilise the subgroup function (will need to reformat the
  # dataframe fed into plotly)

  include_legend <- case_when(
    first(plotdata$measure) == "TYPE OF BIRTH" &
      first(plotdata$measure_cat) != "spontaneous vaginal births" ~ FALSE,
    first(plotdata$measure) == "GESTATION AT BIRTH" &
      first(plotdata$measure_cat) != "between 32 and 36 weeks (inclusive)" ~ FALSE,
    first(plotdata$measure) == "ADMISSIONS TO NEOCARE BY LEVEL OF CARE" &
      first(plotdata$measure_cat) != "special care" ~ FALSE,
    TRUE ~ TRUE)
  
  # include_trend_shift_legend = TRUE ensures that the shift and trend legends appear even when the 
  # chart "linked" to the legend doesn't have any shifts or trends 
  
  include_trend_shift_legend <- case_when(
    first(plotdata$measure_cat) == "spontaneous vaginal births" ~ FALSE,
    first(plotdata$measure_cat) == "between 32 and 36 weeks (inclusive)" ~ FALSE,
    first(plotdata$measure_cat) == "special care" ~ FALSE,
    TRUE ~ include_legend)
  
  select_date_tickvals <- switch( # tells plotly where ticks will show
   first(plotdata$measure), 
   "BOOKINGS" = bookings_date_tickvals,
   "GESTATION AT BOOKING" = gest_at_booking_date_tickvals, # temp
   "TERMINATIONS" = terminations_date_tickvals,
   "GESTATION AT TERMINATION" = terminations_date_tickvals,
   "INDUCTIONS" = SMR02_date_tickvals,
   "TYPE OF BIRTH" = SMR02_multiples_date_tickvals,
   "TEARS" = SMR02_date_tickvals,
   "GESTATION AT BIRTH" = SMR02_multiples_date_tickvals,
   "APGAR5" = SMR02_date_tickvals,
   "ADMISSIONS TO NEOCARE BY LEVEL OF CARE" = SMR02_multiples_date_tickvals
   )
  
  select_date_ticktext <- switch( # tells plotly what text to show on ticks
   first(plotdata$measure), 
   "BOOKINGS" = bookings_date_ticktext,
   "GESTATION AT BOOKING" = gest_at_booking_date_ticktext, # temp
   "TERMINATIONS" = terminations_date_ticktext,
   "GESTATION AT TERMINATION" = terminations_date_ticktext,
   "INDUCTIONS" = SMR02_date_ticktext,
   "TYPE OF BIRTH" = SMR02_multiples_date_ticktext,
   "TEARS" = SMR02_date_ticktext,
   "GESTATION AT BIRTH" = SMR02_multiples_date_ticktext,
   "APGAR5" = SMR02_date_ticktext,
   "ADMISSIONS TO NEOCARE BY LEVEL OF CARE" = SMR02_multiples_date_ticktext
   )
  
  # adds an asterisk to these Board names when there is a related footnote to show
  
  legend_board_name <- if_else(
    (first(plotdata$measure == "TYPE OF BIRTH") &
       first(plotdata$hbname == "NHS Borders")
     ) |
      # (first(plotdata$measure == "GESTATION AT BOOKING") & # retired as of October 2024 release
      # first(plotdata$hbname %in% c("NHS Forth Valley", "NHS Tayside"))
      # ) |
      (first(plotdata$measure == "GESTATION AT TERMINATION") &
      first(plotdata$hbname == "NHS Orkney, NHS Shetland and NHS Western Isles")
      ),
    paste0(first(plotdata$hbname), "*"),
    first(plotdata$hbname)
    )
  
  hoverinfo_format <- switch( # tells plotly how to format median hoverinfo
    first(plotdata$measure), 
   "BOOKINGS" = ",.0f",
   "GESTATION AT BOOKING" = ".1f",
   "TERMINATIONS" = ",.0f",
   "GESTATION AT TERMINATION" = ".1f",
   "INDUCTIONS" = ".1f",
   "TYPE OF BIRTH" = ".1f",
   "TEARS" = ".2f",
   "GESTATION AT BIRTH" = ".2f",
   "APGAR5" = ".2f",
   "ADMISSIONS TO NEOCARE BY LEVEL OF CARE" = ".1f"
   )

  xaxis_plots <- orig_xaxis_plots
  xaxis_plots[["tickmode"]] <- "array"
  xaxis_plots[["tickvals"]] <- select_date_tickvals
  xaxis_plots[["ticktext"]] <- select_date_ticktext

  yaxis_plots <- orig_yaxis_plots
  
  yaxis_plots[["title"]] <- list(
    text = ~ case_match(
      first(plotdata$measure),
      "TEARS" ~ "Percentage of women (%)",
      "APGAR5" ~ "Percentage of babies (%)",
      "ADMISSIONS TO NEOCARE BY LEVEL OF CARE" ~ "Percentage of babies (%)",
      .default = yaxislabel
      )
    )
  
  yaxis_plots[["tickformat"]] <- 
    if_else(first(plotdata$measure) %in% c("APGAR5", "TEARS"),
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
      legendrank = 1200,
      showlegend = ~ include_trend_shift_legend,
      hovertext = "",
      hoverinfo = "none"
    ) %>% 
    add_trace(
      y = ~ measure_value,
      mode = "lines+markers",
      line = list(
        color = "black", # black lines
        width = 1),
      marker = list(
        color = "black", # black dots
        size = 5),
      name = ~ case_match(
        first(plotdata$measure),
        "TYPE OF BIRTH" ~ "percentage of births (%)",
        "GESTATION AT BIRTH" ~ "percentage of births (%)",
        "ADMISSIONS TO NEOCARE BY LEVEL OF CARE" ~ "percentage of babies (%)",
        .default = str_to_lower(var_label(measure_value))
      ),
      legendgroup = "measure_value",
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
      legendgroup = "pre_pandemic_median",
      legendrank = 200,
      showlegend = ~ include_legend,
      hoverinfo = "y",
      yhoverformat = hoverinfo_format
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
      legendgroup = "extended_pre_pandemic_median",
      legendrank = 300,
      showlegend = ~ include_legend,
      hoverinfo = "y",
      yhoverformat = hoverinfo_format
    ) %>%
    add_trace(
      y = ~ shift, # orange lines
      mode = "lines",
      line = list(
        color = "orange", # orange lines (prevents missing data warning)
        width = 2),
      marker = NULL,
      name = orig_shift_label,
      legendgroup = "shift",
      legendrank = 1300,
      showlegend = ~ include_trend_shift_legend,
      hovertext = "",
      hoverinfo = "none"
    ) %>%
    layout(
      font = plotly_global_font,
      xaxis = xaxis_plots,
      yaxis = yaxis_plots,
      legend = list(title = list(text = paste0(legend_board_name, "<br>")),
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
  
  if(first(plotdata$measure_cat) %in% c("spontaneous vaginal births",
                                        "between 32 and 36 weeks (inclusive)",
                                        "special care")) {
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
        name = orig_trend_label, # retrieves label of variable
        legendgroup = "trend",
        legendrank = 800,
        showlegend = TRUE,
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
        name = orig_shift_label, # retrieves label of variable
        legendgroup = "shift",
        legendrank = 900,
        showlegend = TRUE,
        hovertext = "",
        hoverinfo = "none"
      )
  }
  
  # post-pandemic traces for the GESTATION AT BOOKING and GESTATION AT TERMINATION measures
  
  if(first(plotdata$measure) %in% c("GESTATION AT BOOKING", "GESTATION AT TERMINATION")) {
    
    runcharts <- runcharts %>%
      add_trace(
        data = filter(plotdata,!is.na(post_pandemic_median)),
        y = ~ post_pandemic_median, # magenta line
        type = "scatter",
        mode = "lines",
        line = list(
          color = phs_colours("phs-magenta"),
          width = 1
        ),
        marker = NULL,
        name = ~ paste0(var_label(post_pandemic_median)
                        ),
        legendrank = 1000,
        showlegend = include_legend,
        legendgroup = "post-pandemic median",
        hoverinfo = "y",
        yhoverformat = hoverinfo_format
      ) %>%
      add_trace(
        data = filter(plotdata,!is.na(extended_post_pandemic_median)),
        y = ~ extended_post_pandemic_median, # dotted magenta line
        type = "scatter",
        mode = "lines",
        line = list(
          color = phs_colours("phs-magenta"),
          width = 1,
          dash = "4"
        ),
        marker = NULL,
        name = ~ paste0(var_label(extended_post_pandemic_median)
                        ),
        legendrank = 1100,
        showlegend = include_legend,
        legendgroup = "extended post-pandemic median",
        hoverinfo = "y",
        yhoverformat = hoverinfo_format
      )
  }
  
  # additional traces for the "special" Boards in GESTATION AT BOOKING measure
  
  if(first(plotdata$measure) == "GESTATION AT BOOKING" &
     first(plotdata$hbname) %in% c("NHS Forth Valley", "NHS Tayside")) {
    
    runcharts <- runcharts %>%
      add_trace(
        data = filter(plotdata,!is.na(revised_median)),
        y = ~ revised_median, # green line
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
        legendrank = 400,
        showlegend = include_legend,
        legendgroup = "revised median",
        hoverinfo = "y",
        yhoverformat = hoverinfo_format
      ) %>%
      add_trace(
        data = filter(plotdata,!is.na(extended_revised_median)),
        y = ~ extended_revised_median, # dotted green line
        type = "scatter",
        mode = "lines",
        line = list(
          color = phs_colours("phs-green"),
          width = 1,
          dash = "4"
        ),
        marker = NULL,
        name = ~ paste0(case_when(
          hbname == "NHS Forth Valley" ~ 
            paste0("projected average gestation from Mar 2022", "<br>", "to end Jun 2022"),
          hbname == "NHS Tayside" ~ 
            paste0("projected average gestation from Aug 2021", "<br>", "to end Jun 2022"),
          TRUE ~ ""
        )
        ),
        legendrank = 500,
        showlegend = include_legend,
        legendgroup = "extended_revised_median",
        hoverinfo = "y",
        yhoverformat = hoverinfo_format
      )
  }
  
  return(runcharts)
}

# Function to create the context charts (overall numbers relevant to the measure)
# Parameters:
# plotdata: dataframe with data to be plotted
# date: name of the "date" variable (may be "QUARTER" rather than "date")
# num: main measure variable to be plotted as line (e.g. number of Apgar5 scores < 7)
# num_hover: hovertext for the num
# den: "total" measure variable to be plotted as a line (e.g. the total number of Apgar5 scores recorded)
# den_hover: hovertext for the den
# yaxislabel: text to appear on y axis

creates_context_charts <- function(plotdata,
                                   date,
                                   num,
                                   num_hover = "mytext1",
                                   den,
                                   den_hover = "mytext2",
                                   yaxislabel = "Number of births"){
  
  plotdata <- droplevels(plotdata) # drop unused factor levels
  
  y_max <- max(plotdata$den, na.rm = TRUE) # allows a margin to be set around y-axis
  
  # include_legend = TRUE for ONE of multiple runcharts (otherwise the legends get repeated) 
  # need to see if a different method can utilise the subgroup function (will need to reformat the
  # dataframe fed into plotly)
  
  include_legend <- case_when(
    first(plotdata$measure) == "TYPE OF BIRTH" &
      first(plotdata$measure_cat) != "spontaneous vaginal births" ~ FALSE,
    first(plotdata$measure) == "GESTATION AT BIRTH" &
      first(plotdata$measure_cat) != "between 32 and 36 weeks (inclusive)" ~ FALSE,
    TRUE ~ TRUE)
  
  # ensures ticks and tick labels correspond (different for ABC, TERMINATIONS, SMR02)
  
  select_date_tickvals <- switch( # tells plotly where ticks will show
    first(plotdata$measure), 
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
    first(plotdata$measure), 
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
  
  # adds an asterisk to these Board names when there is a related footnote to show
  
  legend_board_name <- if_else(
    (first(plotdata$measure == "TYPE OF BIRTH") &
       first(plotdata$hbname == "NHS Borders")
    ),
    paste0(first(plotdata$hbname), "*"),
    first(plotdata$hbname)
  )
  
  xaxis_plots <- orig_xaxis_plots
  xaxis_plots[["tickmode"]] <- "array"
  xaxis_plots[["tickvals"]] <- select_date_tickvals
  xaxis_plots[["ticktext"]] <- select_date_ticktext
  
  yaxis_plots <- orig_yaxis_plots
  yaxis_plots[["range"]] <- list(0, y_max * 1.05) # expands the y-axis range to prevent cut-offs
  yaxis_plots[["title"]] <- list(
    text = ~ case_match(
      first(plotdata$measure),
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
        first(plotdata$measure),
        c("TYPE OF BIRTH", "GESTATION AT BIRTH") ~ "number of births",
        "APGAR5" ~ "babies that had an Apgar5 score less than 7",
        "EXTREMELY PRE-TERM BIRTHS" ~ "births at 22-26 weeks in a hospital with a NICU",
        .default = str_to_lower(var_label(num))
      ),
      #legendgroup = "measure_value"
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
        first(plotdata$measure),
        "APGAR5" ~ "babies that had a known Apgar5 score",
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
        title = list(text = paste0(legend_board_name, "<br>")),
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

# # Function to build download data
# # Parameter: 
# # measure: dataframe to be downloaded
# 
# builds_download_data <- function(measure) {
#   
#   downloaddata <- download_dataframe[[{{measure}}]] 
#     
#   return(downloaddata)
#   
# }

# Function to select Excel download file
# Parameter: 
# this_excel_measure_name: name of the measure (as seen in the Excel filenames)

download_excel_file <- function(this_excel_measure_name) {
  
  this_excel_filename <- excel_filenames[excel_filenames %like% this_excel_measure_name]
  
  this_excel_filepath <- excel_filepaths[excel_filepaths %like% this_excel_measure_name]
  
  downloadHandler(
    
    filename = this_excel_filename,
    
    content = function(file) {
      file.copy(this_excel_filepath, file)
    }
  )
}
