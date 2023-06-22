# load packages - needs to be reviewed

library(labelled)
#library(Hmisc)
library(data.table)
library(DT)
library(shiny)
library(shinymanager)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
# library(ggrepel)
# library(gginnards)
library(stringr)
library(phsstyles)
library(phsmethods)
library(purrr)
library(fresh)
#library(fontawesome)

# username and password for PRA

credentials <- readRDS("admin/credentials.rds")

# tells PRA dashboard where to pick up the latest data from (change each month)

extract_date <- as.Date("2023-05-18")
NRS_extract_date <- as.Date("2023-05-18")

nice_extract_date <- format(extract_date, "%d %B %Y") # used on each dashboard chart page - autopopulates them

NRS_published_date <- "14 March 2023"

# load latest mateneo data (from Wider Impacts Deliveries folder / local folder)

# load(paste0("/PHI_conf/MaternityBirths/Topics/MaternityHospitalSubmissions/Projects/20201028-WiderImpactsDashboardDeliveries/WI deliveries/data/output/",
#                       extract_date, " extract/matneodata.RData")
#      )

load("data/matneodata.RData") # for SPBAND dashboard - cannot connect to server, needs self-contained dataset

# load latest NRS data (from Wider Impacts Deliveries folder / local folder)

# load(paste0("/PHI_conf/MaternityBirths/Topics/MaternityHospitalSubmissions/Projects/20201028-WiderImpactsDashboardDeliveries/WI deliveries/data/output/",
#                       extract_date, " extract/NRS_data.rds")
# )

load("data/NRS_data.rds") # for SPBAND dashboard - cannot connect to server, needs self-contained dataset

# split runchart_dataframe into individual indicator dataframes

bookings_data <- filter(runchart_dataframe, INDICATOR == "BOOKINGS")
gest_at_booking_data <- filter(runchart_dataframe, INDICATOR == "GESTATION AT BOOKING")
terminations_data <- filter(runchart_dataframe, INDICATOR == "TERMINATIONS")
gest_at_termination_data <- filter(runchart_dataframe, INDICATOR == "GESTATION AT TERMINATION")
inductions_data <- filter(runchart_dataframe, INDICATOR == "INDUCTIONS")
type_of_birth_data <- filter(runchart_dataframe, INDICATOR == "TYPE OF BIRTH")
tears_data <- filter(runchart_dataframe, INDICATOR == "TEARS")
gest_at_birth_data <- filter(runchart_dataframe, INDICATOR == "GESTATION AT BIRTH")
apgar5_data <- filter(runchart_dataframe, INDICATOR == "APGAR5")

# set up x-axis chart labels

bookings_date_range <- unique(bookings_data$DATE)
bookings_date_tickvals <- bookings_date_range[seq(1, length(bookings_date_range), 2)]
bookings_date_ticktext <- format(bookings_date_tickvals,"%b %Y")

terminations_date_range <- unique(terminations_data$DATE)
terminations_date_tickvals <- terminations_date_range[seq(1, length(terminations_date_range), 3)]
terminations_date_ticktext <- format(terminations_date_tickvals, "%b %Y")

SMR02_date_range <- unique(inductions_data$DATE)
SMR02_date_tickvals <- SMR02_date_range[seq(1, length(SMR02_date_range), 2)]
SMR02_date_ticktext <- qtr(SMR02_date_tickvals, format = "short")

SMR02_multiples_date_tickvals <- SMR02_date_range[seq(1, length(SMR02_date_range), 3)]
SMR02_multiples_date_ticktext <- qtr(SMR02_multiples_date_tickvals, format = "short")

y_max_type_of_birth <- max(type_of_birth_data$MEASURE, na.rm = TRUE) # not sure this is still needed

date_range_NRS <- as.character(unique(NRS_timeseries$DATE))

NRS_date_tickvals <- c(date_range_NRS[seq(1, 16, 2)], " ", "2020", " ", " ",
                       date_range_NRS[seq(21, length(date_range_NRS), 2)])

NRS_date_ticktext <- NRS_date_tickvals

stillbirths_download <- NRS_timeseries %>% 
  select("DATASET", "HBTYPE", "HBNAME", "PERIOD", "DATE", "INDICATOR", "INDICATOR_CAT", 
         "MEASURE", "MEAN", "EXTENDED", "SUFFIX", "MEASURE_DESCRIPTION")

y_max_NRS <- max(NRS_timeseries$MEASURE, na.rm = TRUE) # allows a margin to be set around y-axis

# create static labels for the runchart legends

orig_trend_label <-  
  paste0("trends: 5 or more consistently increasing", "<br>", "or decreasing points")
orig_shift_label <-  
  paste0("shifts: 6 or more consecutive points", "<br>", "above or below average")

# defining functions
source("functions.R")

# useful groupings for telling Shiny when to show the different drop-down filters

tabnames <- 1:13

names(tabnames) <- c("home", "multi_indicator_overview", "pregnancies_booked", "terminations", 
                     "gestation_at_booking", "gestation_at_termination",
                     "location_of_ex_pre_term", "inductions", "type_of_birth",
                     "perineal_tears", "gestation_at_birth", "stillbirths",
                     "apgar_scores")

tabpanels <- 1:2

names(tabpanels) <- c("Board comparison", "Runcharts")

tabsets <- 1:13

names(tabsets) <- c("input$tabset00", "input$tabset01", "input$tabset10", 
                    "input$tabset11", "input$tabset12", "input$tabset13",
                    "input$tabset20", "input$tabset21", "input$tabset22",
                    "input$tabset23", "input$tabset24", "input$tabset25",
                    "input$tabset26")

show_org <- names(tabnames[!tabnames %in% c(1, 7, 12)]) # don't show organisation selection in "home",
                                                     # "location_of_ex_pre_term", "stillbirths" 

show_HBname <- names(tabnames[tabnames %in% c(2, 3, 4)]) # show HB selection in "multi_indicator_overview",
                                                        # "pregnancies_booked", "terminations"

island_names <- c("NHS Orkney", "NHS Shetland", "NHS Western Isles"
                  )

# order for HB dropdown filter

HBnames <- c("Scotland", "NHS Ayrshire & Arran", "NHS Borders", "NHS Dumfries & Galloway",
             "NHS Fife", "NHS Forth Valley", "NHS Grampian", "NHS Greater Glasgow & Clyde",
             "NHS Highland", "NHS Lanarkshire", "NHS Lothian", "NHS Tayside", "NHS Orkney",
             "NHS Shetland", "NHS Western Isles"
             )

# order for multiple charts

HBnames2 <- c("Scotland", "NHS Ayrshire & Arran", "NHS Borders", "NHS Dumfries & Galloway",
             "NHS Fife", "NHS Forth Valley", "NHS Grampian", "NHS Greater Glasgow & Clyde", 
             "NHS Highland", "NHS Lanarkshire", "NHS Lothian", "NHS Tayside"
             )

# not sure if this will be used

selected_colours <-
  as.character(c(phs_colours()[1:8],
                 phs_colours(str_subset(
                   names(phs_colours()), "-80"
                 ))))

# sets "clean" theme for charts

new = theme_minimal() + 
  theme(text = element_text(size = 14),
        plot.title = element_text(size = 16),
        plot.caption = element_text(size = 14, hjust = -0, vjust = 0),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 14))

theme_set(new)

# overwrites "Shiny" set dashboard colours with PHS colours - may need to change for accessibility 
# reasons

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#3F3685" # header bar = PHS-purple
    ),
  adminlte_sidebar( # sidebar colours
    width = "290px",
    dark_bg = "#9F9BC2", # background colour (not selected) = PHS-purple-50
    dark_hover_bg = "#655E9D", # background colour (when hovering) = PHS-purple-80
    dark_color = "#3F3685", # text colour (not selected) = PHS-purple
    dark_submenu_color = "#3F3685", # sub-menu text colour (not selected) = PHS-purple
    dark_submenu_hover_color = "#FFFFFF" # text colour (when hovering) = white
    ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#FFF",
    info_box_bg = "#FFF"
    )
  #adminlte_vars(
    #box_border_color = "#FFF"
    #)
)

# buttons to remove (from plotly menu)

bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                     'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                     'hoverClosestCartesian', 'zoom2d', 'pan2d', 'resetScale2d')

# sets default style of x and y axes

orig_xaxis_plots <- list(title = "", tickfont = list(size = 12),
                         titlefont = list(size = 16), showline = FALSE,
                         fixedrange = FALSE,  rangemode = "tozero", zeroline = TRUE, 
                         zerolinecolor = "#F2F2F2", tickangle = -45, standoff = 30,
                         #tickmode = "linear", 
                         #type = "date", dtick = "M3", #tick0 = "2017-01-01",
                         showticklabels = TRUE) #ticklabelstep = 4, 

orig_yaxis_plots <- list(title = "", rangemode = "tozero", fixedrange = FALSE,
                         size = 4, tickfont = list(size = 12), standoff = 30,
                         titlefont = list(size = 14), zeroline = TRUE,
                         zerolinecolor = "#F2F2F2")

### END OF SCRIPT ###
