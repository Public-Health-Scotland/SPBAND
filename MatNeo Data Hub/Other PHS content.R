# MatNeo Data Hub Other PHS content

output$hub_other_phs <- renderUI({
  
  fluidRow(
    
    column(12,
           
           p("Public Health Scotland publishes a series of annual Official Statistics on pregnancy, childbirth and the early care of babies born in Scotland.",
             
             tags$ul(
               
               tags$li(class = "bullet-points",
                       
                       tags$a(
                         href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                         tags$u("Births in Scotland"),
                         class = "externallink",
                         target = "_blank",
                         rel = "noopener noreferrer"
                       ),
                       
                       " - data on mother’s age, ethnicity, deprivation, BMI, maternal diabetes, method of birth, gestation at birth and birthweight."
               ),
               
               tags$li(class = "bullet-points",
                       
                       tags$a(
                         href = "https://www.publichealthscotland.scot/publications/antenatal-booking-in-scotland",
                         tags$u("Antenatal Booking in Scotland"),
                         class = "externallink",
                         target = "_blank",
                         rel = "noopener noreferrer"
                       ),
                       
                       " - data on numbers of pregnancies booked, gestation at booking, and smoking status (by mother’s age, deprivation and ethnicity, for all three topics)."
               ),
               
               tags$li(class = "bullet-points",
                       
                       tags$a(
                         href = "https://publichealthscotland.scot/publications/termination-of-pregnancy-statistics/",
                         tags$u("Termination of pregnancy"),
                         class = "externallink",
                         target = "_blank",
                         rel = "noopener noreferrer"
                       ),
                       
                       " - information on age, gestation, method of termination, deprivation, ethnicity, previous terminations, and grounds for termination."
               ),
               
               tags$li(class = "bullet-points",
                       
                       tags$a(
                         href = "https://publichealthscotland.scot/publications/teenage-pregnancies/",
                         tags$u("Teenage pregnancies"),
                         class = "externallink",
                         target = "_blank",
                         rel = "noopener noreferrer"
                       ),
                       
                       " - information is provided on age group (<16, <18, <20), deprivation, and whether the pregnancy was terminated or led to a birth."
               ),
               
               tags$li(class = "bullet-points",
                       
                       tags$a(
                         href = "https://publichealthscotland.scot/publications/congenital-conditions-in-scotland/",
                         tags$u("Congenital Conditions in Scotland"),
                         class = "externallink",
                         target = "_blank",
                         rel = "noopener noreferrer"
                       ),
                       
                       " - current best estimates of the number of babies with serious congenital conditions."
               ),
               
               
               tags$li(class = "bullet-points",
                       
                       tags$a(
                         href = "https://publichealthscotland.scot/publications/infant-feeding-statistics",
                         tags$u("Infant feeding"),
                         class = "externallink",
                         target = "_blank", 
                         rel = "noopener noreferrer"
                       ),
                       
                       " - information on exclusive breastfeeding, mixed, and formula feeding (initiation; first visit; 6-8 week; 13 to 15 month; maternal age; deprivation, ethnicity; looked-after status; maternal smoking status)."
               ),
               
               tags$li(class = "bullet-points",
                       
                       tags$a(
                         href = "https://publichealthscotland.scot/publications/pregnancy-screening-for-down-s-syndrome-edwards-syndrome-and-patau-s-syndrome-in-scotland/",
                         tags$u("Pregnancy Screening for Down’s Syndrome, Edward’s Syndrome, and Patau’s Syndrome in Scotland"),
                         class = "externallink",
                         target = "_blank", 
                         rel = "noopener noreferrer"
                       ),
                       
                       " - information on the coverage of screening (including by maternal age group, deprivation quintile and ethnic group), screening results, laboratory activity, invasive diagnostic procedures following screening, and pregnancy outcomes."
               )
               
             ) # tags$ul
             
           ) # p
           
    ) # column
    
  ) # fluidRow
  
})
