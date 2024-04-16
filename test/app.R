#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        #uiOutput("hbnameControl"), 
        uiOutput("organisationControl"),
        #uiOutput("measureControl")
      ),

        # Show a plot of the generated distribution
        mainPanel(
          #DTOutput("table")
          plotlyOutput("inductions_small_multiples_mainland",
                       height = "35em"
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  Selected <- reactiveValues(HBType = "RESIDENCE",
                             HBName = "NHS Ayrshire & Arran",
                             Measure = "BOOKINGS",
                             Date = "Jun 2019"
  )
  
  #observeEvent(input$hbname, Selected$HBName <- input$hbname)
  observeEvent(input$organisation, Selected$HBType <- input$organisation)
  #observeEvent(input$measure, Selected$Measure <- input$measure)
  
      # select ORGANISATION (RESIDENCE or TREATMENT)

  output$organisationControl <-renderUI({ 
      radioButtons(
      inputId = "organisation",
      label = "View analyses by Board of",
      choiceNames = list("Residence", "Treatment"),
      choiceValues = list("RESIDENCE", "TREATMENT"),
      selected = "RESIDENCE",
      inline = FALSE
    )
    })
  
  inductions_small_multiples_data <- reactive({
  # selects data
  
  req(input$organisation)
  
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
        HBGROUP = if_else(hbname %in% island_names, "island", "mainland")
        #xshowticklabels = if_else(hbname %in% island_names, TRUE, FALSE)
  ) %>% 
  group_by(HBGROUP, hbtype) %>% 
  mutate(y_max = max(measure_value),
         hbname2 = factor(hbname, levels = HBnames)
         ) #%>% 
         #y_max = plyr::round_any(y_max, 10, f = ceiling)) %>%
  # ungroup() 

  
  if (is.null(data()))
  {
  return()
  }

  else {
    data
  }
  
  })
  
  output$table <- renderDT(server = FALSE,
                             datatable(inductions_small_multiples_data(),
                                       extensions = "Buttons",
                                       options = list(
                                         dom = "Bfrtip",
                                         buttons = "copyHtml5"
                                         )
                                       )
    )

creates_overview_charts_without_median <- function(plotdata,
                                                   measure_value,
                                                   hover = "mytext",
                                                   yaxislabel = "Percentage of births (%)"){

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
  
  plot_ly(
    plotdata,
    x = ~ date,
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
}

all_plots <- reactive({
  creates_overview_charts_without_median(data)
    plotdata = inductions_small_multiples_data()
  )
})

# df_plots <- reactive({
#   inductions_small_multiples_data() %>% 
#     nest_by(.$HBGROUP, .$hbname2) %>%
#     mutate(plot = list(creates_overview_charts_without_median(inductions_small_multiples_data())))
# })
# 
# #output$plotly_inductions_overview_M <- renderPlotly({
# 
# mainland <- reactive({
#   df_plots %>%
#     filter(., `.$HBGROUP` == "mainland") %>%
#     subplot(nrows = 4,
#             shareX = TRUE,
#             shareY = TRUE) %>%
#     layout(
#       annotations = list(
#         x = 0,
#         y = 0.4,
#         text = "Percentage of births (%)",
#         xshift = -50,
#         textangle = 270,
#         showarrow = FALSE,
#         xref = "paper",
#         yref = "paper"
#       )
#     )
#   
# })

# # output$plotly_inductions_overview_I <- renderPlotly({
# 
# inductions2 <- 
#   df_plots %>%
#     filter(., `.$HBGROUP` == "island") %>%
#     subplot(nrows = 1,
#             shareX = FALSE,
#             shareY = TRUE)
# 
# inductions_overview <- 
#   subplot(inductions1,
#           inductions2,
#           heights = c(0.8, 0.2),
#           # margin = c(0.01, 0.01, 0.05, 0.02), 
#           nrows = 2) %>%
#   config(displaylogo = F, displayModeBar = FALSE)
  
#return(mainland)

  
output$inductions_small_multiples_mainland <- renderPlotly({
  
  all_plots()

})
}


# Run the application 
shinyApp(ui = ui, server = server)
