library(shinydashboard)
library(dplyr)
library(shinya11y)

# function to make the sidebar menu accessible 

accessible_menu1 = function(bad_menu) {
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

rem_aria_label1 <- function(icon) {
  icon[["attribs"]][["aria-label"]] = NULL
  return(icon)
  }

rem_menu_aria_label1 <- function(menu) {
  menu[["children"]][[1]][["children"]][[3]][["attribs"]][["aria-label"]] = NULL
  return(menu)
}

home <- tabItem(
  tabName = "home",

  fluidRow(

    h1("Welcome to my dashboard",
       style = "text-align: center;"
    ),

    hr(),

    tabBox(title = "Home",

           # The id lets us use input$tabset00 on the server to find the current tab
           id = "tabset00",
           width = 12
    )
  )
)

overview <- tabItem(
  tabName = "overview",
  
  fluidRow(
    tabBox(title = "Overview",
           
           # The id lets us use input$tabset01 on the server to find the current tab
           id = "tabset01",
           width = 12
    )
  )
)

bookings <- tabItem(
  tabName = "bookings",
  
  fluidRow(
    tabBox(title = "Number of bookings",
           
           # The id lets us use input$tabset10 on the server to find the current tab
           id = "tabset10",
           width = 12
    )
  )
)

cancellations <- tabItem(
  tabName = "cancellations",
  
  fluidRow(
    tabBox(title = "Number of cancellations",
           
           # The id lets us use input$tabset11 on the server to find the current tab
           id = "tabset11",
           width = 12
           )
  )
)


x = sidebarMenu(
  id = "sidebarMenu",
  menuItem("Home",
           tabName = "home",
           icon = icon("info-circle", verify_fa = FALSE) %>% rem_aria_label1()
           ),
  menuItem("Overview",
           tabName = "overview",
           icon = icon("tachometer-alt", verify_fa = FALSE) %>% rem_aria_label1()
           ),
  menuItem("Information",
           icon = icon("person-pregnant", verify_fa = FALSE) %>% rem_aria_label1(),
           menuSubItem("Number of bookings", 
                       tabName = "bookings",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label1()
                       ),
           menuSubItem("Number of cancellations",
                       tabName = "cancellations",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label1()
                       )
           ) %>% rem_menu_aria_label1()
  )

ui =
dashboardPage(
  #use_tota11y(),
  skin = "purple",
  header = dashboardHeader(title = "My App"),
  sidebar = dashboardSidebar(accessible_menu(x), br(), textOutput("mytext")),
  body = dashboardBody(
    id = "dashboardBody",
    tabItems(
        home,
        overview,
        bookings,
        cancellations
    )
  )
)

server = function(input, output, session){
  
  output$mytext <- renderText({
    paste0("Topic = ", input$sidebarMenu)
  })
}

shiny::shinyApp(ui, server)
