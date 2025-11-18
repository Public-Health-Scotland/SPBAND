# load packages ----

library(labelled)
library(data.table)
library(DT)
library(shiny)
library(shinymanager)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(phsstyles)
library(phsmethods)
library(purrr)
library(fresh)

# define functions ----
source("functions.R")

# username and password for PRA ----

credentials <- readRDS("admin/credentials.rds")

# change each quarter: ----
# refresh_date =  the date the SMR02, ABC, Terminations data are refreshed, used on each dashboard chart page - autopopulates them

refresh_date <- as.Date("2025-09-16") 

pretty_refresh_date <- format(refresh_date,"%d %B %Y")

# latest NRS publication date

NRS_published_date <- "09 September 2025"

# initialise folders and filenames for Excel downloads ----

# folder containing Excel downloads

excel_downloads_folder <- "data/excel downloads/"

# get Excel filenames

excel_filenames <- list.files(excel_downloads_folder, pattern = ".xlsx")

# set Excel measure names

excel_measure_names <- str_remove(excel_filenames, paste0("_", refresh_date, ".xlsx"))

# set individual Excel filepaths

excel_filepaths <- paste0(excel_downloads_folder, excel_filenames)

# load latest dataframes ----

# load latest SMR02 ABC Terminations data

load("data/SMR02-ABC-Terminations.RData") # for SPBAND dashboard - cannot connect to server, needs self-contained dataset

# load latest extreme pre-term data

# for SPBAND dashboard - cannot connect to server, needs self-contained dataset

extremely_preterm_data <- readRDS("data/extremely-preterm-data.rds") 

# load latest NRS stillbirths & infant deaths data

NRS_timeseries <- readRDS("data/stillbirths-infant-deaths-data.rds") # for SPBAND dashboard - cannot connect to server, needs self-contained dataset##

# load latest NeoCareIn+ gestation by highest BAPM level of care data

# gest_by_BAPM_LOC_data <- readRDS("data/gestation-by-BAPM-level-of-care.rds")

# load latest NeoCareIn+ babies born at 30-32 weeks admitted to neonatal unit

# babies_30_32_discharged_from_neocare_data <- readRDS("data/babies-30-32-discharged-from-neocare.rds")

# split runchart_dataframe into individual measure dataframes ----

bookings_data <- load_and_split_dataframe("BOOKINGS")
gest_at_booking_data <- load_and_split_dataframe("GESTATION AT BOOKING")
terminations_data <- load_and_split_dataframe("TERMINATIONS")
gest_at_termination_data <- load_and_split_dataframe("GESTATION AT TERMINATION")
inductions_data <- load_and_split_dataframe("INDUCTIONS")
type_of_birth_data <- load_and_split_dataframe("TYPE OF BIRTH")
tears_data <- load_and_split_dataframe("TEARS")
gest_at_birth_data <- load_and_split_dataframe("GESTATION AT BIRTH")
apgar5_data <- load_and_split_dataframe("APGAR5")

# set up x-axis chart labels ----

bookings_date_range <- unique(bookings_data$date)
bookings_date_tickvals <- bookings_date_range[seq(1, length(bookings_date_range), 3)]
bookings_date_ticktext <- format(bookings_date_tickvals,"%b %Y")

terminations_date_range <- unique(terminations_data$date)
terminations_date_tickvals <- terminations_date_range[seq(1, length(terminations_date_range), 4)]
terminations_date_ticktext <- format(terminations_date_tickvals, "%b %Y")

SMR02_date_range <- unique(inductions_data$date)
SMR02_date_tickvals <- SMR02_date_range[seq(1, length(SMR02_date_range), 2)]
SMR02_date_ticktext <- qtr(SMR02_date_tickvals, format = "short")

SMR02_multiples_date_tickvals <- SMR02_date_range[seq(1, length(SMR02_date_range), 4)]
SMR02_multiples_date_ticktext <- qtr(SMR02_multiples_date_tickvals, format = "short")

# NeoCare_date_range <- unique(gest_by_BAPM_LOC_data$date)
# NeoCare_date_tickvals <- NeoCare_date_range[seq(1, length(NeoCare_date_range), 2)]
# NeoCare_date_ticktext <- qtr(NeoCare_date_tickvals, format = "short")

# STLLBIRTHS SPECIFIC ----

NRS_timeseries <- filter(NRS_timeseries, date_label != "Apr-Jun 2020") %>%
  mutate(date = if_else(date_label == "2020", as.Date("2020-04-01"), as.Date(date)))
  
date_range_NRS = as.character(unique(NRS_timeseries$date))

date_label_range_NRS = as.character(unique(NRS_timeseries$date_label))

# NRS_date_tickvals <- c(date_range_NRS[seq(1, 16, 2)], "2020", " ", " ", # balances x-axis dates
#                        date_range_NRS[seq(22, length(date_range_NRS), 2)])

NRS_date_tickvals <- c(date_range_NRS[seq(1, 16, 2)], date_range_NRS[18], # only mark "Apr-Jun 2020" which is actually the annual figure
                       date_range_NRS[seq(21, length(date_range_NRS), 2)])

NRS_date_ticktext <- c(date_label_range_NRS[seq(1, 16, 2)], "2020", # labels to match marks
                       date_label_range_NRS[seq(21, length(date_label_range_NRS), 2)])

y_max_NRS <- max(NRS_timeseries$measure_value, na.rm = TRUE) # allows a margin to be set around y-axis

# GESTATION AT BIRTH SPECIFIC ----

measure_cat_order <- c("between 18 and 44 weeks (inclusive)",
                       "between 37 and 41 weeks (inclusive)",
                       "between 32 and 36 weeks (inclusive)",
                       "42 weeks and over (inclusive)",
                       "under 32 weeks",
                       "under 37 weeks"
)

# measure_cat_label feeds the multiple runchart titles

measure_cat_label <- c(paste0("all known gestations (18", "<sup>+0</sup>", " to 44", "<sup>+6</sup>", " weeks)"),
                       paste0("37", "<sup>+0</sup>", " to 41", "<sup>+6</sup>", " weeks gestation"),
                       paste0("32", "<sup>+0</sup>", " to 36", "<sup>+6</sup>", " weeks gestation"),
                       paste0("42", "<sup>+0</sup>", " weeks gestation and over"),
                       "under 32 weeks gestation",
                       "under 37 weeks gestation"
)

# create a tibble with "nice" (superscript text) gestations for gestation at birth measure

nicename <- tibble(measure_cat_order, measure_cat_label)

#nicename$measure_cat_label <- factor(nicename$measure_cat_label,
                                    # levels = measure_cat_label)

gest_at_birth_data <- left_join(gest_at_birth_data,
                                nicename,
                                by = c("measure_cat" = "measure_cat_order")
                                )

# puts runcharts in correct order

gest_at_birth_data$measure_cat <- factor(gest_at_birth_data$measure_cat, levels = measure_cat_order) 

# puts context chart lines in correct order

gest_at_birth_data$measure_cat_label <- factor(gest_at_birth_data$measure_cat_label,
                                               levels = measure_cat_label) 

# tidy up 

rm(measure_cat_order, measure_cat_label, nicename)

# TYPE OF BIRTH SPECIFIC ----

y_max_type_of_birth <- max(type_of_birth_data$measure_value, na.rm = TRUE) # used to set common y-axis scale

# measure_cat_order

measure_cat_order <- c("all caesarean births", "planned caesarean births", "unplanned caesarean births", 
                         "assisted vaginal births", "spontaneous vaginal births")

# measure_cat_label feeds the multiple runchart titles

measure_cat_label <- c("caesarean births", "planned caesarean births", "unplanned caesarean births", 
                       str_wrap("assisted vaginal births (includes forceps, ventouse and vaginal breech births)", 50), "spontaneous vaginal births")

# create a tibble with "nice" categories for type of birth measure

nicename <- tibble(measure_cat_order, measure_cat_label)

type_of_birth_data <- left_join(type_of_birth_data,
                                nicename,
                                by = c("measure_cat" = "measure_cat_order")
                                )

# Puts an asterisk next to subtitles when NHS Borders is selected # temporary till issue fixed

# type_of_birth_data$measure_cat_label = 
#   if_else(type_of_birth_data$hbname == "NHS Borders" & type_of_birth_data$measure_cat_label %like% "planned",
#           str_replace(type_of_birth_data$measure_cat_label, "births", "births*"),
#           type_of_birth_data$measure_cat_label)

# puts runcharts in correct order

type_of_birth_data$measure_cat <- factor(type_of_birth_data$measure_cat, levels = measure_cat_order) 

# tidy up 

rm(measure_cat_order, measure_cat_label, nicename)

# create static labels for the runchart legends ----

orig_trend_label <-  
  paste0("trends: 5 or more consistently increasing", "<br>", "or decreasing points")
orig_shift_label <-  
  paste0("shifts: 6 or more consecutive points", "<br>", "above or below average")

# useful groupings for telling Shiny when to show the different drop-down filters ----

tabnames <- 1:16

names(tabnames) <- 
  c("home", "multi_indicator_overview", "pregnancies_booked",
    "terminations", "gestation_at_booking", "gestation_at_termination",
    "location_of_ex_pre_term", "inductions", "type_of_birth",
    "perineal_tears", "gestation_at_birth", "stillbirths",
    "apgar_scores", "infant_feeding", "median_cga_30_32",
    "gestation_by_BAPM_LOC")

show_org <- names(tabnames[!tabnames %in% c(1, 7, 12, 14, 15, 16)]) # don't show organisation selection in "home",
                                                     # "location_of_ex_pre_term", "stillbirths", "infant_feeding", 
                                                     # "median_cga_30_32", "gestation_by_BAPM_LOC"

show_HBname <- names(tabnames[tabnames %in% c(2, 3, 4)]) # show HB selection in "multi_indicator_overview",
                                                        # "pregnancies_booked", "terminations"

show_HBname2 <- names(tabnames[!tabnames %in% c(1, 2, 3, 4, 7, 12, 14, 15, 16)]) # the remaining measures

island_names <- c("NHS Orkney", "NHS Shetland", "NHS Western Isles"
                  )

# order for small multiple charts ----

HBnames <- c("Scotland", "NHS Ayrshire & Arran", "NHS Borders", "NHS Borders*", "NHS Dumfries & Galloway",
             "NHS Fife", "NHS Forth Valley", "NHS Grampian", "NHS Greater Glasgow & Clyde",
             "NHS Highland", "NHS Lanarkshire", "NHS Lothian", "NHS Tayside", "NHS Orkney",
             "NHS Shetland", "NHS Western Isles", "NHS Orkney, NHS Shetland <br> and NHS Western Isles*")

# sets colour palette to the PHS colour scheme ----

selected_colours <-
  as.character(c(phs_colours()[1:8],
                 phs_colours(str_subset(
                   names(phs_colours()), "-80"
                 ))))

#3F3685
#8c3585
#0057ae
#296600


# overwrites "Shiny" set dashboard colours with PHS colours - may need to change for accessibility 
# reasons

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#3F3685" # header bar = PHS-purple
  ),
  adminlte_sidebar( # sidebar colours
    width = "290px",
    dark_bg = "#57518f", # background colour (not selected) = close to PHS-purple-80
    dark_hover_bg = "#3F3685", # background colour (when hovering) = PHS-purple
    dark_color = "white", # text colour (not selected) = white
    dark_submenu_bg = "#8C3585", # sub-menu background colour = close to PHS-magenta
    dark_submenu_color = "white", # sub-menu text colour (not selected) = white
    dark_submenu_hover_color = "white", # text colour (when hovering) = white
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#FFF",
    info_box_bg = "#FFF"
  )
)

# buttons to remove (from plotly menu) ----

bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                     'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                     'hoverClosestCartesian', 'zoom2d', 'pan2d', 'resetScale2d')

# sets default style of x and y axes ----

orig_xaxis_plots <- list(
  title = "",
  showticklabels = TRUE,
  tickfont = list(size = 12),                         
  fixedrange = FALSE, # allows zoom
  rangemode = "tozero", # show all non-negative values
  zeroline = FALSE, 
  tickangle = -45 # angles the tick labels
  )
                         
orig_yaxis_plots <- list(
  title = list(font = list(size = 14)),
  showticklabels = TRUE,
  tickfont = list(size = 12),
  tickformat = ",d", # formats numbers with thousand separator if needed
  fixedrange = FALSE, # allows zoom
  rangemode = "tozero", # show all non-negative values
  zeroline = FALSE  
  )

plotly_global_font <- list(
  color = "#3F3685" # phs-purple
  )

# customise features and interactivity of DT table: ----
# create a list of options used to format the DT table

my.options <- list(
  dom = "t",
  scrollY = TRUE,
  scrollX = FALSE,
  autoWidth = TRUE, # smart width handling
  searching = FALSE, # search box above table
  ordering = FALSE, # whether columns can be sorted
  lengthChange = FALSE, # ability to change number rows shown on page in table
  lengthMenu = FALSE, # options lengthChange can be changed to
  pageLength = 10, # initial number of rows per page of table
  paging = FALSE, # whether to do pagination
  info = FALSE) # notes whether or not table is filtered

# create HTML formatting code for header and overall table HTML container
# create header style HTML code

header.style <- "th { font-family: 'Arial'; font-weight: bold; color: #3F3685; background-color: white;}"

### END OF SCRIPT ###
