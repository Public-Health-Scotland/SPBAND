# a) data ----

Date <-  "Jun 2019"
HBType <- "RESIDENCE"
HBName <- "NHS Tayside"

Selected <- data.frame(Date, HBType, HBName)

plotdata <- 
  data <- bookings_data %>%
  filter(hbname == Selected$HBName &
           hbtype == Selected$HBType) %>%
    set_variable_labels(
    measure = "Number of pregnancies booked",
    median = " average to end Feb 2020",
    extended = " projected average from Mar 2020"
  ) %>% 
  mutate(mytext = paste0("Month: ", 
                         format(date, "%b %Y"),
                         "<br>",
                         var_label(measure),
                         ": ",
                         measure),
         trend = NA, # to prevent this line being plotted
         shift = NA # ditto
         )

# b) chart ----

bookings_runcharts <- 
  
creates_runcharts(plotdata = plotdata,
                  yaxislabel = "Number of pregnancies booked"
                  )


# d) download data

bookings_download <- builds_download_data("BOOKINGS")

bookings_download_data <- downloadHandler(

  filename = function() {
      paste0(first(bookings_download$indicator), "_", refresh_date, ".csv", sep = "")
    },

  content = function(file) {
    write.csv(bookings_download, file, row.names = FALSE)
    }
  )
  
