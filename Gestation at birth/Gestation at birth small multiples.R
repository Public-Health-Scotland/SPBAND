# a) data ----

# map input$gestation to visually nicer gestation group names for title

# initialise Selected$Nicename

Selected$Nicename <- "under 32 weeks gestation"
Selected$Gestation <- "under 32 weeks"

observeEvent(input$gestation, Selected$Nicename <- case_when(
  input$gestation == "under 32 weeks" ~ "under 32 weeks gestation",
  input$gestation == "between 32 and 36 weeks (inclusive)" ~ paste0("32", "<sup>+0</sup>",
                                                         " to 36", "<sup>+6</sup>", " weeks gestation"),
  input$gestation == "under 37 weeks" ~ "under 37 weeks gestation",
  input$gestation == "42 weeks and over (inclusive)" ~ paste0("42", "<sup>+0</sup>", " weeks gestation and over")
  )
)

gest_at_birth_small_multiples_data <- reactive({
  # selects data
  
  req(input$organisation)
  
  data <- gest_at_birth_data %>%
    filter(hbtype == Selected$HBType &
             measure_cat == Selected$Gestation) %>%
    mutate(mytext = paste0(hbname,
                           ": ",
                           formatted_name,
                           "<br>",
                           "Quarter: ",
                           quarter_label,
                           "<br>",
                           "Percentage of births",
                           ": ",
                           format(measure_value,
                                  digits = 1,
                                  nsmall = 2),
                           "%"),
           date = quarter_label,
           hbgroup = factor(if_else(hbname %in% island_names, "island", "mainland"),
                            levels = c("mainland", "island"), ordered = TRUE)
    ) %>% 
    group_by(hbgroup, hbtype) %>% 
    mutate(y_max = max(measure_value)
    ) %>%
    ungroup()
  
  if (is.null(data))
  {
    return()
  }
  
  else {
    data
  }
  
})

# b) chart ---- 

output$gest_at_birth_small_multiples <- renderPlotly({

subplot_mainland_island_small_multiples(
  plotdata = gest_at_birth_small_multiples_data()
  )
})

# c) chart title ----

output$gest_at_birth_small_multiples_title <- renderText({
  paste0("Board of ",
         str_to_sentence(input$organisation)
  )
})

output$gest_at_birth_small_multiples_sub_title <- renderText({
  HTML(paste0("Percentage of singleton live births that were at ", Selected$Nicename))
})

