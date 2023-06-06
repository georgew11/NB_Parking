#R version 4.1.1
#load libraries
library(dplyr)         #dplyr          v1.0.10
library(ggplot2)       #ggplot2        v3.3.6
library(lubridate)     #lubridate      v1.8.0
library(ggrepel)       #ggrepel        v0.9.1
library(zoo)           #zoo            v1.8.10
library(scales)        #scales         v1.2.0
library(todor)         #todor          v0.1.2 - run todor() to find to do items or go to Addins at top
library(stringr)       #stringr        v1.4.0
library(shiny)         #shiny          v1.7.2
library(shinythemes)   #shinythemes    v1.2.0
library(shinydashboard)#shinydashboard v.0.7.2
library(leaflet)       #leaflet        v.2.1.1
library(DT)            #DT             v.0.26

#no scientific notation
options(scipen = 100)
options(shiny.maxRequestSize=500*1024^2)
## global function ----------------------------

inbetween_integers <- function(a, b) {
  u <- sort(c(a, b))
  res <- setdiff(ceiling(u[1]):floor(u[2]), c(a, b))
  if (!length(res)) {
    NULL
  } else {
    res
  }
}

time_list <- c("7am", "8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm",
               "4pm", "5pm", "6pm", "7pm", "8pm", "9pm", "10pm", "11pm")

hour_char <- data.frame(Hour=7:23, AMPM= time_list)

hour_char$AMPM <- as.factor(hour_char$AMPM)
hour_char$AMPM <- factor(hour_char$AMPM , levels = c("7am", "8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm",
                                                     "4pm", "5pm", "6pm", "7pm", "8pm", "9pm", "10pm", "11pm"))
## ui elements ----------------------------



ui <- fluidPage(
    navbarPage("Neptune Beach Parking Tracker", theme = shinytheme("yeti"),
                sidebarPanel(
                  fileInput('datafile', 'Choose CSV file',
                            accept=c('csv', 'comma-separated-values','.csv'))
                ),
                 tabPanel("Total Parking",
                          fluidRow(
                             column(8,plotOutput('totalplot')),
                             column(12,dataTableOutput("totaltable")))
                          ),
                 tabPanel("Parking by Kiosk",
                          fluidRow(
                             column(8, plotOutput('kioskplot')),
                             column(12, plotOutput('kioskplot_visits')))
                          ),
                 tabPanel("Peak Dates and Times",
                          fluidRow(
                             column(8,tableOutput('visitortable')),
                             column(12,tableOutput('busydays')))
                  )
               )
    )


## server -----------------------------

server <- function(input, output, session) {
  
  
  master_df<-reactive({
    if (is.null(input$datafile))
      return(NULL)                
    master<-read.csv(input$datafile$datapath)
    master <- master %>% rename("Payment_Type" = "Payment.Mean",
                                "Kiosk_ID" = "Terminal.Code",
                                "Transaction_Amount" = "Amount",
                                "Total_Time_Min" = "Total.Duration.in.mins",
                                "Paid_Time_Min" = "Paid.Duration.in.mins",
                                "Card_Type" = "Card.Type",
                                "System_ID" = "System.ID",
                                "Printed_ID" = "Printed.ID",
                                "Park_Code" = "Park.Code",
                                "Terminal_Description" = "Terminal.Description",
                                "End_Date" = "End.Date",
                                "Free_Duration_Min" = "Free.Duration.in.mins",
                                "Transaction_Type" = "Type",
                                "Plate_Number" = "Plate..",
                                "Terminal_Date" = "Terminal.Date")
    a1 <- dmy_hm(master$Terminal_Date)
    datetime <- data.frame(Date=format(a1, '%m/%d/%Y'), Time=format(a1, '%H:%M'))
    master <- cbind(master, datetime)
    master$Date <- as.POSIXct(master$Date, format = "%m/%d/%Y")
    master$Week_Day <- wday(master$Date, label = TRUE)
    master$Month <- format(master$Date, "%B")
    master$Time2 <- as.POSIXct(master$Time, format = "%H:%M")
    master$Hour <- hour(master$Time2)
    master$Minute <- minute(master$Time2)
    master <- master %>% 
      subset(select=-c(Time2))
    
    master <- master %>% arrange(Date, Time)
    
    #relocating Date, Time columns
    master <- master %>% 
      relocate(Date, Month, Week_Day, Time, Hour, Minute, .after = Terminal_Date) 
    
    
    #changing variable type of columns to factor
    factor_cols <- c("Payment_Type", "Origin", "Kiosk_ID", "Card_Type",
                     "Terminal_Description", "User.Type.", "Plate_Number")
    master[factor_cols] <- lapply(master[factor_cols], factor)
    
    #Drop extraneous columns
    clean_master <- master %>% 
      subset(select=-c(Server.Time, Product.amount, Reload.Amount, Total.Duration, Paid.Duration, Space.., 
                       Zone.Desc, Circuit.Desc, Address, Free.Duration., Currency))
    
    #splitting dataset to parking transactions and fines
    master_park <- clean_master %>% filter(User.Type. == 1)
    master_park <- master_park %>%  subset(select=-c(Transaction_Type, User.Type.))
    
    
    #adding column to master_park  -> Total time in minutes to hours
    master_park <- master_park %>% 
      mutate(Total_Time_Hours = Total_Time_Min/60) %>% 
      relocate(Total_Time_Hours, .after = Minute)
    return(master_park)
  })
  day_count<-reactive({
    if (is.null(input$datafile))
      return(NULL)   
    day_count <- master_df() %>% 
      group_by(Week_Day) %>% 
      summarise(count = n_distinct(Date))
    return(day_count)
  })
  
  final_occupy<-reactive({
    if (is.null(input$datafile))
      return(NULL)
    #Changing date column to POSIXct date time variable type and adding week day, month, time, hour columns
    
    
    #hour dataframe
    new <- data.frame(matrix(0, nrow = nrow(master_df()), ncol = 17))
    names(new) <- c(paste0("Hour_", 7:23))
    
    
    #Hours df joined with master_park df
    master_tally <- cbind(master_df(), new)
    
    #Presence tally
    
    #list of Hour_X column numbers
    column_index <- match("Hour_7",names(master_tally)):match("Hour_23",names(master_tally))
    
    #column number difference
    difference <- match("Hour_7",names(master_tally)) - 7
    
    #hours tracked
    hour_index <- 7:23
    
    
    #tallying car presence by hour
    withProgress(message = 'Processing data', value = 0, {
      for (i in 1:nrow(master_tally)) {
        beghour <- master_tally[i,]$Hour
        endhour <- master_tally[i,]$Hour + master_tally[i,]$Total_Time_Hours + master_tally[i,]$Minute/60
        hours_present <- append(beghour, inbetween_integers(beghour, endhour),1)
        for (h in hours_present) {
          for (k in hour_index){
            if (h == k) {
              for (j in column_index){
                if (h == k & h == (j-difference)) {
                  master_tally[i,j] <- 1
                  break
                }
                else {}
              }
            }
            else {}
          }
        }
        incProgress(1/(1:nrow(master_tally)), detail = paste(round(100 * i/nrow(master_tally)), "% Complete"))
      }
    })
    
    #save as csv
    #write.csv(master_tally, "may_sep_park.csv")
    #uploaded_park <- read.csv("may_sep_park.csv")
    
    
    #sum of cars parked by hour, week day, and kiosk id
    names_index <- colnames(master_tally[24:40])
    occupied <-
      master_tally %>% 
      group_by(Week_Day, Kiosk_ID) %>% 
      summarise(across(c(names_index), sum))
    
    
    
    avg_occupy <- left_join(occupied, day_count(), by = c("Week_Day" = "Week_Day"))
    
    
    #Average cars parked
    occupied_avg <- cbind(avg_occupy[1:2], avg_occupy[3:19]/avg_occupy$count)
    
    #Deleting 36330100 kiosk id bc it is missing most days values or 0
    occupied_avg3 <- occupied_avg %>% 
      filter(Kiosk_ID != 36330100)
    #_____________________________________________________________________________________________________
    #making table with average occupied parking spots by day and kiosk
    
    #count of unique kiosk id's
    n_kiosk <- length(unique(occupied_avg3$Kiosk_ID))
    #11
    days_of_week <- length(unique(occupied_avg3$Week_Day))
    #7
    n_hours <- length(hour_index)
    #17, 7am-11pm
    
    final_occupy <- data.frame(Week_Day = NA, Kiosk_ID = NA, Hour = rep(hour_index, n_kiosk * days_of_week), Avg_Car = NA)
    
    #week day column
    days <- c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")
    begin <- 1
    ending <- n_kiosk * n_hours
    for (i in 1:days_of_week){
      final_occupy[begin:ending, 1] <- rep(days[i], n_kiosk * n_hours)
      begin = begin + n_kiosk * n_hours
      ending = ending + n_kiosk * n_hours
    }
    
    #adding kiosk id column
    weekend <- master_df() %>% 
      group_by(Week_Day, Kiosk_ID) %>% 
      filter(Week_Day == "Sat" | Week_Day == "Sun") %>% 
      summarise(Weekend_Time = sum(Total_Time_Min))
    
    weekend_sums <- weekend %>% 
      group_by(Kiosk_ID) %>% 
      summarise(Weekend_Sum = sum(Weekend_Time))
    #adding kiosk id column
    kiosks <- c(as.character(weekend_sums$Kiosk_ID)[-n_kiosk])
    begin <- 1
    ending <- n_hours
    for (j in 1:days_of_week){
      for (i in 1:n_kiosk){
        final_occupy[begin:ending, 2] <- rep(kiosks[i], n_hours)
        begin = begin + n_hours
        ending = ending + n_hours
      }
    }
    
    
    occ_column_index <- match("Hour_7",names(occupied_avg3)):match("Hour_23",names(occupied_avg3))
    
    occ_rownum <- 1
    first_row <- 1
    final_row <- n_hours
    for (i in 1:nrow(occupied_avg3)) {
      final_occupy[first_row:final_row,4] <- t(occupied_avg3[occ_rownum,occ_column_index])
      first_row = first_row + n_hours
      final_row = final_row + n_hours
      occ_rownum = occ_rownum + 1
    }
    
    day_list <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")
    
    final_occupy$Week_Day <- as.factor(final_occupy$Week_Day)
    final_occupy$Week_Day <- factor(final_occupy$Week_Day, levels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
    
    return(final_occupy)
    
  })
  
  
  totalchart<-reactive({
    if (is.null(input$datafile))
      return(NULL)
    
    occupy_chart_new <- final_occupy()
    
    occupy_chart_new2 <- left_join(occupy_chart_new, hour_char, by = "Hour")
    
    #total average cars by hour and week day
    total_occ <- occupy_chart_new2 %>% 
      group_by(Week_Day, AMPM) %>% 
      summarize(Cars = sum(Avg_Car)) 
    
    
    csvname <- as.character(input$datafile$name)
    
    
    #charting
    total_occ %>% 
      ggplot(aes(x = AMPM, y = Cars, group = Week_Day, color = Week_Day)) +
      geom_line()+
      #scale_x_continuous(breaks = seq(0, 24, by = 1))+
      scale_y_continuous(breaks = seq(0, 300, by = 25))+
      geom_label_repel(aes(label = Week_Day), 
                       data = total_occ %>% filter(AMPM == "3pm"),
                       hjust=-.1,vjust=0, max.overlaps = 10) +
      ggtitle("Neptune Beach Cars Parked")+
      labs(x = "Hour", y = "Average \nCars Parked", 
           caption = paste("City of Neptune Beach Parking Data from ", 
                           csvname, sep = ""))+
      theme(plot.title = element_text(size = 20, face = "bold", hjust = .5),
            axis.title.y = element_text(size = 14, angle = 0, vjust =.5, face = "bold"),
            axis.title.x = element_text(size = 14, face = "bold"),
            plot.caption = element_text(size = 12))
  })
  kioskchart<-reactive({
    if (is.null(input$datafile))
      return(NULL) 
    csvname <- as.character(input$datafile$name)
    occupy_chart <- final_occupy()
    
    occupy_chart2 <- left_join(occupy_chart, hour_char, by = "Hour")
    
    plotting_occ <- occupy_chart2 %>% 
      ggplot(aes(x = AMPM, y = Avg_Car, group = Week_Day, color = Week_Day)) +
      geom_line()#+
    #scale_x_continuous(breaks = seq(0, 24, by = 1))
    #TODO - save and clean up
    #Visitor Frequency by Week Day and Hour for each Kiosk (Terminal)
    plotting_occ + facet_wrap(~Kiosk_ID, ncol = 3) +
      scale_x_discrete(breaks = c("7am", "9am", "11am", "1pm", "3pm", "5pm", "7pm", "9pm", "11pm"))+
      ggtitle("Average Cars Parked by Kiosks")+
      labs(x = "Hour", y = "Average \nCars Parked", 
           caption = paste("City of Neptune Beach Parking Data from ", 
                           csvname, sep = ""))+
      theme(plot.title = element_text(size = 20, face = "bold", hjust = .5),
            axis.title.y = element_text(size = 14, angle = 0, vjust =.5, face = "bold"),
            axis.title.x = element_text(size = 14, face = "bold"),
            plot.caption = element_text(size = 12))
  })
  kiosk_visits <- reactive({
    if (is.null(input$datafile))
      return(NULL) 
    
    park <- master_df()
    ndays <- as.numeric(park$Date[nrow(park)]-park$Date[1])
    
    csvname <- as.character(input$datafile$name)
    
    kiosk_count <- park %>% 
      group_by(Kiosk_ID) %>% 
      summarise(count = n()/ndays)
    
    kiosk_count %>% 
      ggplot(aes(x = reorder(Kiosk_ID, -count), y = count, fill = Kiosk_ID))+
      geom_bar(stat = "identity")+
      labs(
        x = "Kiosk ID",
        y = "Average Daily\n Car Visits",
        colour = "Kiosk ID",
        title = "Kiosk Car Visits", 
        caption = paste("City of Neptune Beach Parking Data from ", 
                        csvname, sep = "")
      ) +
      theme(plot.title = element_text(size = 20, face = "bold", hjust = .5),
            axis.title.y = element_text(size = 14, angle = 0, vjust =.5, face = "bold"),
            axis.title.x = element_text(size = 14, face = "bold"),
            plot.caption = element_text(size = 12))+
      geom_text(aes(label=round(kiosk_count$count)), vjust=1.3, 
                color = "white", face = "bold", size = 6)
  })
  
  top_days <- reactive({
    if (is.null(input$datafile))
      return(NULL)
    
    np <- master_df()
    
    
    big_days <- np %>% 
      group_by(Date) %>% 
      mutate(count = n()) %>% 
      arrange(desc(count))
    
    big_days2 <- big_days %>% 
      group_by(Date, Week_Day) %>% 
      summarise("Visit Count" = count)
    
    big_days3 <- big_days2 %>% 
      arrange(desc(big_days2$`Visit Count`)) %>% 
      distinct(Date, .keep_all = TRUE) %>% 
      rename("Week Day" = "Week_Day")
    big_days3$Date <- as.character(big_days3$Date)
    
    return(big_days3[1:10,])
    
  })  
  
  visitor_count<-reactive({
    if (is.null(input$datafile))
      return(NULL)
    gw <- master_df()
    
    gw2 <- left_join(gw, hour_char, by = "Hour")
    
    day_count2 <- day_count()
    
    time_hour <- gw2 %>%
      group_by(Week_Day, AMPM) %>% 
      summarize(Frequency = n()) 
    
    time_hour_dc <- left_join(time_hour, day_count2, by = c("Week_Day" = "Week_Day"))
    
    new_hour <- time_hour_dc %>%
      group_by(Week_Day, AMPM) %>%
      mutate(Average_Visitors = round(Frequency/count)) %>% 
      arrange(desc(Average_Visitors)) 
    
    new_hour <- new_hour %>% 
      rename("Week Day" = "Week_Day",
             "Hour" = "AMPM",
             "Average Car Visits" = "Average_Visitors")
    
    return(new_hour[1:10,c(1,2,5)])
    
  })
  
  alltable <- reactive({
    if (is.null(input$datafile))
      return(NULL)
    
    occupy_chart_new <- final_occupy()
    
    occupy_chart_new2 <- left_join(occupy_chart_new, hour_char, by = "Hour")
    
    #total average cars by hour and week day
    occupying <- occupy_chart_new2 %>% 
      group_by(Week_Day, AMPM) %>% 
      summarize(Cars = sum(Avg_Car))
    
  })
 
  ## Output #################################################################
  output$kioskplot <- renderPlot({
    kioskchart()
  })
  output$totalplot <- renderPlot({
    totalchart()
  })
  output$kioskplot_visits <- renderPlot ({
    kiosk_visits()
  })
  output$busydays <- renderTable ({
    top_days()
  })
  output$visitortable <- renderTable ({
    visitor_count()
  })
  output$totaltable <- renderDataTable({
    alltable()
  })
}



shinyApp(ui = ui, server = server)


