# Load required packages
library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)




# Load telemetry data
lap_times <- read.csv("lap_times.csv")

ham_ad_2021_lap1 <- read.csv("ham_data_lap_2021_ad_1.csv")
ver_ad_2021_lap1 <- read.csv("ver_data_lap_2021_ad_1.csv")

ham_ad_2021_lap2 <- read.csv("ham_data_lap_2021_ad_2.csv")
ver_ad_2021_lap2 <- read.csv("ver_data_lap_2021_ad_2.csv")

ham_ad_2021_lap3 <- read.csv("ham_data_lap_2021_ad_3.csv")
ver_ad_2021_lap3 <- read.csv("ver_data_lap_2021_ad_3.csv")

ham_ad_2021_lap4 <- read.csv("ham_data_lap_2021_ad_4.csv")
ver_ad_2021_lap4 <- read.csv("ver_data_lap_2021_ad_4.csv")

ham_ad_2021_lap5 <- read.csv("ham_data_lap_2021_ad_5.csv")
ver_ad_2021_lap5 <- read.csv("ver_data_lap_2021_ad_5.csv")

ham_ad_2021_lap6 <- read.csv("ham_data_lap_2021_ad_6.csv")
ver_ad_2021_lap6 <- read.csv("ver_data_lap_2021_ad_6.csv")

ham_ad_2021_lap7 <- read.csv("ham_data_lap_2021_ad_7.csv")
ver_ad_2021_lap7 <- read.csv("ver_data_lap_2021_ad_7.csv")

ham_ad_2021_lap8 <- read.csv("ham_data_lap_2021_ad_8.csv")
ver_ad_2021_lap8 <- read.csv("ver_data_lap_2021_ad_8.csv")

ham_ad_2021_lap9 <- read.csv("ham_data_lap_2021_ad_9.csv")
ver_ad_2021_lap9 <- read.csv("ver_data_lap_2021_ad_9.csv")

ham_ad_2021_lap10 <- read.csv("ham_data_lap_2021_ad_10.csv")
ver_ad_2021_lap10 <- read.csv("ver_data_lap_2021_ad_10.csv")

ham_ad_2021_lap11 <- read.csv("ham_data_lap_2021_ad_11.csv")
ver_ad_2021_lap11 <- read.csv("ver_data_lap_2021_ad_11.csv")

ham_ad_2021_lap12 <- read.csv("ham_data_lap_2021_ad_12.csv")
ver_ad_2021_lap12 <- read.csv("ver_data_lap_2021_ad_12.csv")

ham_ad_2021_lap13 <- read.csv("ham_data_lap_2021_ad_13.csv")
ver_ad_2021_lap13 <- read.csv("ver_data_lap_2021_ad_13.csv")

ham_ad_2021_lap14 <- read.csv("ham_data_lap_2021_ad_14.csv")
ver_ad_2021_lap14 <- read.csv("ver_data_lap_2021_ad_14.csv")

ham_ad_2021_lap15 <- read.csv("ham_data_lap_2021_ad_15.csv")
ver_ad_2021_lap15 <- read.csv("ver_data_lap_2021_ad_15.csv")

ham_ad_2021_lap16 <- read.csv("ham_data_lap_2021_ad_16.csv")
ver_ad_2021_lap16 <- read.csv("ver_data_lap_2021_ad_16.csv")

ham_ad_2021_lap17 <- read.csv("ham_data_lap_2021_ad_17.csv")
ver_ad_2021_lap17 <- read.csv("ver_data_lap_2021_ad_17.csv")

ham_ad_2021_lap18 <- read.csv("ham_data_lap_2021_ad_18.csv")
ver_ad_2021_lap18 <- read.csv("ver_data_lap_2021_ad_18.csv")

ham_ad_2021_lap19 <- read.csv("ham_data_lap_2021_ad_19.csv")
ver_ad_2021_lap19 <- read.csv("ver_data_lap_2021_ad_19.csv")

ham_ad_2021_lap20 <- read.csv("ham_data_lap_2021_ad_20.csv")
ver_ad_2021_lap20 <- read.csv("ver_data_lap_2021_ad_20.csv")

ham_ad_2021_lap21 <- read.csv("ham_data_lap_2021_ad_21.csv")
ver_ad_2021_lap21 <- read.csv("ver_data_lap_2021_ad_21.csv")

ham_ad_2021_lap22 <- read.csv("ham_data_lap_2021_ad_22.csv")
ver_ad_2021_lap22 <- read.csv("ver_data_lap_2021_ad_22.csv")

ham_ad_2021_lap23 <- read.csv("ham_data_lap_2021_ad_23.csv")
ver_ad_2021_lap23 <- read.csv("ver_data_lap_2021_ad_23.csv")

ham_ad_2021_lap24 <- read.csv("ham_data_lap_2021_ad_24.csv")
ver_ad_2021_lap24 <- read.csv("ver_data_lap_2021_ad_24.csv")

ham_ad_2021_lap25 <- read.csv("ham_data_lap_2021_ad_25.csv")
ver_ad_2021_lap25 <- read.csv("ver_data_lap_2021_ad_25.csv")

ham_ad_2021_lap26 <- read.csv("ham_data_lap_2021_ad_26.csv")
ver_ad_2021_lap26 <- read.csv("ver_data_lap_2021_ad_26.csv")

ham_ad_2021_lap27 <- read.csv("ham_data_lap_2021_ad_27.csv")
ver_ad_2021_lap27 <- read.csv("ver_data_lap_2021_ad_27.csv")

ham_ad_2021_lap28 <- read.csv("ham_data_lap_2021_ad_28.csv")
ver_ad_2021_lap28 <- read.csv("ver_data_lap_2021_ad_28.csv")

ham_ad_2021_lap29 <- read.csv("ham_data_lap_2021_ad_29.csv")
ver_ad_2021_lap29 <- read.csv("ver_data_lap_2021_ad_29.csv")

ham_ad_2021_lap30 <- read.csv("ham_data_lap_2021_ad_30.csv")
ver_ad_2021_lap30 <- read.csv("ver_data_lap_2021_ad_30.csv")

ham_ad_2021_lap31 <- read.csv("ham_data_lap_2021_ad_31.csv")
ver_ad_2021_lap31 <- read.csv("ver_data_lap_2021_ad_31.csv")

ham_ad_2021_lap32 <- read.csv("ham_data_lap_2021_ad_32.csv")
ver_ad_2021_lap32 <- read.csv("ver_data_lap_2021_ad_32.csv")

ham_ad_2021_lap33 <- read.csv("ham_data_lap_2021_ad_33.csv")
ver_ad_2021_lap33 <- read.csv("ver_data_lap_2021_ad_33.csv")

ham_ad_2021_lap34 <- read.csv("ham_data_lap_2021_ad_34.csv")
ver_ad_2021_lap34 <- read.csv("ver_data_lap_2021_ad_34.csv")

ham_ad_2021_lap35 <- read.csv("ham_data_lap_2021_ad_35.csv")
ver_ad_2021_lap35 <- read.csv("ver_data_lap_2021_ad_35.csv")

ham_ad_2021_lap36 <- read.csv("ham_data_lap_2021_ad_36.csv")
##ver_ad_2021_lap36 <- read.csv("ver_data_lap_2021_ad_36.csv")

ham_ad_2021_lap37 <- read.csv("ham_data_lap_2021_ad_37.csv")
ver_ad_2021_lap37 <- read.csv("ver_data_lap_2021_ad_37.csv")

ham_ad_2021_lap38 <- read.csv("ham_data_lap_2021_ad_38.csv")
ver_ad_2021_lap38 <- read.csv("ver_data_lap_2021_ad_38.csv")

ham_ad_2021_lap39 <- read.csv("ham_data_lap_2021_ad_39.csv")
ver_ad_2021_lap39 <- read.csv("ver_data_lap_2021_ad_39.csv")

ham_ad_2021_lap40 <- read.csv("ham_data_lap_2021_ad_40.csv")
ver_ad_2021_lap40 <- read.csv("ver_data_lap_2021_ad_40.csv")

ham_ad_2021_lap41 <- read.csv("ham_data_lap_2021_ad_41.csv")
ver_ad_2021_lap41 <- read.csv("ver_data_lap_2021_ad_41.csv")

ham_ad_2021_lap42 <- read.csv("ham_data_lap_2021_ad_42.csv")
ver_ad_2021_lap42 <- read.csv("ver_data_lap_2021_ad_42.csv")

ham_ad_2021_lap43 <- read.csv("ham_data_lap_2021_ad_43.csv")
ver_ad_2021_lap43 <- read.csv("ver_data_lap_2021_ad_43.csv")

ham_ad_2021_lap44 <- read.csv("ham_data_lap_2021_ad_44.csv")
ver_ad_2021_lap44 <- read.csv("ver_data_lap_2021_ad_44.csv")

ham_ad_2021_lap45 <- read.csv("ham_data_lap_2021_ad_45.csv")
ver_ad_2021_lap45 <- read.csv("ver_data_lap_2021_ad_45.csv")

ham_ad_2021_lap46 <- read.csv("ham_data_lap_2021_ad_46.csv")
ver_ad_2021_lap46 <- read.csv("ver_data_lap_2021_ad_46.csv")

ham_ad_2021_lap47 <- read.csv("ham_data_lap_2021_ad_47.csv")
ver_ad_2021_lap47 <- read.csv("ver_data_lap_2021_ad_47.csv")

ham_ad_2021_lap48 <- read.csv("ham_data_lap_2021_ad_48.csv")
ver_ad_2021_lap48 <- read.csv("ver_data_lap_2021_ad_48.csv")

ham_ad_2021_lap49 <- read.csv("ham_data_lap_2021_ad_49.csv")
ver_ad_2021_lap49 <- read.csv("ver_data_lap_2021_ad_49.csv")

ham_ad_2021_lap50 <- read.csv("ham_data_lap_2021_ad_50.csv")
ver_ad_2021_lap50 <- read.csv("ver_data_lap_2021_ad_50.csv")

ham_ad_2021_lap51 <- read.csv("ham_data_lap_2021_ad_51.csv")
ver_ad_2021_lap51 <- read.csv("ver_data_lap_2021_ad_51.csv")

ham_ad_2021_lap52 <- read.csv("ham_data_lap_2021_ad_52.csv")
ver_ad_2021_lap52 <- read.csv("ver_data_lap_2021_ad_52.csv")

ham_ad_2021_lap53 <- read.csv("ham_data_lap_2021_ad_53.csv")
##ver_ad_2021_lap53 <- read.csv("ver_data_lap_2021_ad_53.csv")

ham_ad_2021_lap54 <- read.csv("ham_data_lap_2021_ad_54.csv")
ver_ad_2021_lap54 <- read.csv("ver_data_lap_2021_ad_54.csv")

ham_ad_2021_lap55 <- read.csv("ham_data_lap_2021_ad_55.csv")
ver_ad_2021_lap55 <- read.csv("ver_data_lap_2021_ad_55.csv")

ham_ad_2021_lap56 <- read.csv("ham_data_lap_2021_ad_56.csv")
ver_ad_2021_lap56 <- read.csv("ver_data_lap_2021_ad_56.csv")

ham_ad_2021_lap57 <- read.csv("ham_data_lap_2021_ad_57.csv")
ver_ad_2021_lap57 <- read.csv("ver_data_lap_2021_ad_57.csv")

ham_ad_2021_lap58 <- read.csv("ham_data_lap_2021_ad_58.csv")
ver_ad_2021_lap58 <- read.csv("ver_data_lap_2021_ad_58.csv")

# Load lap time data
lap_subset <- subset(lap_times, raceId == 1073 & (driverId == 1 | driverId == 830))

# Define UI
ui <- fluidPage(
  
  # App title
  titlePanel("Abu Dhabi 2021 Lap Times & Telemetry"),
  
  # Sidebar with dropdown to select lap
  sidebarLayout(
    sidebarPanel(
      selectInput("lap_select", "Select Lap:", choices = unique(lap_subset$lap)),
      br(),
      tags$em("Note: Telemetry data is only available for laps 2-58.")
    ),
    
    # Display lap times and telemetry data for selected lap
    mainPanel(
      tabsetPanel(
        tabPanel("Lap Times",
                 plotOutput("lap_times_plot")
        ),
        tabPanel("Telemetry Data",
                 plotOutput("telemetry_table")
        )
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Generate lap times plot
  output$lap_times_plot <- renderPlot({
    time_sec <- unlist(lapply(strsplit(lap_times$time, split = ":", fixed = TRUE)
                              , function(xx){ as.numeric(xx[1])*60 + as.numeric(xx[2]) }))
    
    lap_times$time_sec <- time_sec
    
    
    lap_times$driverId_fac <- as.factor(lap_times$driverId)
    lap_times_subset <- subset(lap_times, raceId == 1073 & (driverId == 1 | driverId == 830))
    ggplot(lap_times_subset, aes(x = lap, y = time_sec, colour = driverId_fac)) +
      geom_line() + 
      scale_colour_manual(values = c("steelblue", "darkred") 
                          , name = "Driver", breaks = c("1", "830") 
                          , labels = c("Lewis Hamilton", "Max Verstappen")) + 
      labs(y = "Lap Time (sec)", x = "Lap") + theme(legend.position = "top")
  })
  
  # Generate telemetry table
  output$telemetry_table <- renderPlot({
if (input$lap_select == 2) {
    p1 <- ggplot(data = ver_ad_2021_lap2, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 2")) +
    geom_line() +
    geom_line(data = ham_ad_2021_lap2, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 2")) +
    labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
    scale_color_manual(values = c("darkred", "steelblue"), 
                       name = "Max Verstappen Vs Lewis Hamilton",
                       breaks = c("Max Verstappen Lap 2", "Lewis Hamilton Lap 2"),
                       labels = c("Lap 2", "Lap 2"))
      
    p2 <- ggplot(data = ver_ad_2021_lap2, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 2")) +
    geom_line() +
    geom_line(data = ham_ad_2021_lap2, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 2")) +
    abs(y = "Throttle(%)", x = "Distance(in meters)") + 
    scale_color_manual(values = c("darkred", "steelblue"), 
                       name = "Max Verstappen vs Lewis Hamilton",
                           breaks = c("Max Verstappen Lap 2", "Lewis Hamilton Lap 2"),
                           labels = c("Lap 2", "Lap 2")) +
        guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
      
      
      p3 <- ggplot(data = ver_ad_2021_lap2, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 2")) +
        geom_line() +
        geom_line(data = ham_ad_2021_lap2, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 2")) +
        labs(y = "Brake", x = "Distance(in meters)") + 
        scale_color_manual(values = c("darkred", "steelblue"), 
                           name = "Max Verstappen vs Lewis Hamilton",
                           breaks = c("Max Verstappen Lap 2", "Lewis Hamilton Lap 2"),
                           labels = c("Lap 2", "Lap 2")) +
        guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
      
      p4 <- ggplot(data = ver_ad_2021_lap2, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 2")) +
        geom_line() +
        geom_line(data = ham_ad_2021_lap2, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 2")) +
        labs(y = "Gear Number", x = "Distance(in meters)") + 
        scale_color_manual(values = c("darkred", "steelblue"), 
                           name = "Max Verstappen vs Lewis Hamilton",
                           breaks = c("Max Verstappen Lap 2", "Lewis Hamilton Lap 2"),
                           labels = c("Lap 2", "Lap 2")) +
        guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
      
      p5 <- ggplot(data = ver_ad_2021_lap2, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 2")) +
        geom_line() +
        geom_line(data = ham_ad_2021_lap2, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 2")) +
        labs(y = "RPM", x = "Distance(in meters)") + 
        scale_color_manual(values = c("darkred", "steelblue"), 
                           name = "Max Verstappen vs Lewis Hamilton",
                           breaks = c("Max Verstappen Lap 2", "Lewis Hamilton Lap 2"),
                           labels = c("Lap 2", "Lap 2")) +
        guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
      
      grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
      
    }
    
    else
    {
      if (input$lap_select == 1) {
        p1 <- ggplot(data = ver_ad_2021_lap1, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 1")) +
          geom_line() +
          geom_line(data = ham_ad_2021_lap1, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 1")) +
          labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
          scale_color_manual(values = c("darkred", "steelblue"), 
                             name = "Max Verstappen Vs Lewis Hamilton",
                             breaks = c("Max Verstappen Lap 1", "Lewis Hamilton Lap 1"),
                             labels = c("Lap 1", "Lap 1"))
        
        p2 <- ggplot(data = ver_ad_2021_lap1, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 1")) +
          geom_line() +
          geom_line(data = ham_ad_2021_lap1, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 1")) +
          labs(y = "Throttle(%)", x = "Distance(in meters)") + 
          scale_color_manual(values = c("darkred", "steelblue"), 
                             name = "Max Verstappen vs Lewis Hamilton",
                             breaks = c("Max Verstappen Lap 1", "Lewis Hamilton Lap 1"),
                             labels = c("Lap 1", "Lap 1")) +
          guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
        
        ver_ad_2021_lap1$Brake <- ifelse(ver_ad_2021_lap1$Brake, 1, 0)
        ham_ad_2021_lap1$Brake <- ifelse(ham_ad_2021_lap1$Brake, 1, 0)
        
        p3 <- ggplot(data = ver_ad_2021_lap1, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 1")) +
          geom_line() +
          geom_line(data = ham_ad_2021_lap1, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 1")) +
          labs(y = "Brake", x = "Distance(in meters)") + 
          scale_color_manual(values = c("darkred", "steelblue"), 
                             name = "Max Verstappen vs Lewis Hamilton",
                             breaks = c("Max Verstappen Lap 1", "Lewis Hamilton Lap 1"),
                             labels = c("Lap 1", "Lap 1")) +
          guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
        
        p4 <- ggplot(data = ver_ad_2021_lap1, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 1")) +
          geom_line() +
          geom_line(data = ham_ad_2021_lap1, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 1")) +
          labs(y = "Gear Number", x = "Distance(in meters)") + 
          scale_color_manual(values = c("darkred", "steelblue"), 
                             name = "Max Verstappen vs Lewis Hamilton",
                             breaks = c("Max Verstappen Lap 1", "Lewis Hamilton Lap 1"),
                             labels = c("Lap 1", "Lap 1")) +
          guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
        
        p5 <- ggplot(data = ver_ad_2021_lap1, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 1")) +
          geom_line() +
          geom_line(data = ham_ad_2021_lap1, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 1")) +
          labs(y = "RPM", x = "Distance(in meters)") + 
          scale_color_manual(values = c("darkred", "steelblue"), 
                             name = "Max Verstappen vs Lewis Hamilton",
                             breaks = c("Max Verstappen Lap 1", "Lewis Hamilton Lap 1"),
                             labels = c("Lap 1", "Lap 1")) +
          guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
        
        grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
        
      }
   else
   {
     if (input$lap_select == 3) {
       p1 <- ggplot(data = ver_ad_2021_lap3, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 3")) +
         geom_line() +
         geom_line(data = ham_ad_2021_lap3, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 3")) +
         labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
         scale_color_manual(values = c("darkred", "steelblue"), 
                            name = "Max Verstappen Vs Lewis Hamilton",
                            breaks = c("Max Verstappen Lap 3", "Lewis Hamilton Lap 3"),
                            labels = c("Lap 3", "Lap 3"))
       
       p2 <- ggplot(data = ver_ad_2021_lap3, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 3")) +
         geom_line() +
         geom_line(data = ham_ad_2021_lap3, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 3")) +
         labs(y = "Throttle(%)", x = "Distance(in meters)") + 
         scale_color_manual(values = c("darkred", "steelblue"), 
                            name = "Max Verstappen vs Lewis Hamilton",
                            breaks = c("Max Verstappen Lap 3", "Lewis Hamilton Lap 3"),
                            labels = c("Lap 3", "Lap 3")) +
         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
       
       
       p3 <- ggplot(data = ver_ad_2021_lap3, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 3")) +
         geom_line() +
         geom_line(data = ham_ad_2021_lap3, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 3")) +
         labs(y = "Brake", x = "Distance(in meters)") + 
         scale_color_manual(values = c("darkred", "steelblue"), 
                            name = "Max Verstappen vs Lewis Hamilton",
                            breaks = c("Max Verstappen Lap 3", "Lewis Hamilton Lap 3"),
                            labels = c("Lap 3", "Lap 3")) +
         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
       
       p4 <- ggplot(data = ver_ad_2021_lap3, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 3")) +
         geom_line() +
         geom_line(data = ham_ad_2021_lap3, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 3")) +
         labs(y = "Gear Number", x = "Distance(in meters)") + 
         scale_color_manual(values = c("darkred", "steelblue"), 
                            name = "Max Verstappen vs Lewis Hamilton",
                            breaks = c("Max Verstappen Lap 3", "Lewis Hamilton Lap 3"),
                            labels = c("Lap 3", "Lap 3")) +
         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
       
       p5 <- ggplot(data = ver_ad_2021_lap3, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 3")) +
         geom_line() +
         geom_line(data = ham_ad_2021_lap3, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 3")) +
         labs(y = "RPM", x = "Distance(in meters)") + 
         scale_color_manual(values = c("darkred", "steelblue"), 
                            name = "Max Verstappen vs Lewis Hamilton",
                            breaks = c("Max Verstappen Lap 3", "Lewis Hamilton Lap 3"),
                            labels = c("Lap 3", "Lap 3")) +
         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
       
       grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
       
     }
     
     else
     {
       if (input$lap_select == 4) {
         p1 <- ggplot(data = ver_ad_2021_lap4, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 4")) +
           geom_line() +
           geom_line(data = ham_ad_2021_lap4, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 4")) +
           labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
           scale_color_manual(values = c("darkred", "steelblue"), 
                              name = "Max Verstappen Vs Lewis Hamilton",
                              breaks = c("Max Verstappen Lap 4", "Lewis Hamilton Lap 4"),
                              labels = c("Lap 4", "Lap 4"))
         
         p2 <- ggplot(data = ver_ad_2021_lap4, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 4")) +
           geom_line() +
           geom_line(data = ham_ad_2021_lap4, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 4")) +
           labs(y = "Throttle(%)", x = "Distance(in meters)") + 
           scale_color_manual(values = c("darkred", "steelblue"), 
                              name = "Max Verstappen vs Lewis Hamilton",
                              breaks = c("Max Verstappen Lap 4", "Lewis Hamilton Lap 4"),
                              labels = c("Lap 4", "Lap 4")) +
           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
         
         
         p3 <- ggplot(data = ver_ad_2021_lap4, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 4")) +
           geom_line() +
           geom_line(data = ham_ad_2021_lap4, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 4")) +
           labs(y = "Brake", x = "Distance(in meters)") + 
           scale_color_manual(values = c("darkred", "steelblue"), 
                              name = "Max Verstappen vs Lewis Hamilton",
                              breaks = c("Max Verstappen Lap 4", "Lewis Hamilton Lap 4"),
                              labels = c("Lap 4", "Lap 4")) +
           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
         
         p4 <- ggplot(data = ver_ad_2021_lap4, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 4")) +
           geom_line() +
           geom_line(data = ham_ad_2021_lap4, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 4")) +
           labs(y = "Gear Number", x = "Distance(in meters)") + 
           scale_color_manual(values = c("darkred", "steelblue"), 
                              name = "Max Verstappen vs Lewis Hamilton",
                              breaks = c("Max Verstappen Lap 4", "Lewis Hamilton Lap 4"),
                              labels = c("Lap 4", "Lap 4")) +
           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
         
         p5 <- ggplot(data = ver_ad_2021_lap4, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 4")) +
           geom_line() +
           geom_line(data = ham_ad_2021_lap4, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 4")) +
           labs(y = "RPM", x = "Distance(in meters)") + 
           scale_color_manual(values = c("darkred", "steelblue"), 
                              name = "Max Verstappen vs Lewis Hamilton",
                              breaks = c("Max Verstappen Lap 4", "Lewis Hamilton Lap 4"),
                              labels = c("Lap 4", "Lap 4")) +
           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
         
         grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
         
       }
       else
       {
         if (input$lap_select == 5) {
           p1 <- ggplot(data = ver_ad_2021_lap5, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 5")) +
             geom_line() +
             geom_line(data = ham_ad_2021_lap5, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 5")) +
             labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
             scale_color_manual(values = c("darkred", "steelblue"), 
                                name = "Max Verstappen Vs Lewis Hamilton",
                                breaks = c("Max Verstappen Lap 5", "Lewis Hamilton Lap 5"),
                                labels = c("Lap 5", "Lap 5"))
           
           p2 <- ggplot(data = ver_ad_2021_lap5, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 5")) +
             geom_line() +
             geom_line(data = ham_ad_2021_lap5, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 5")) +
             labs(y = "Throttle(%)", x = "Distance(in meters)") + 
             scale_color_manual(values = c("darkred", "steelblue"), 
                                name = "Max Verstappen vs Lewis Hamilton",
                                breaks = c("Max Verstappen Lap 5", "Lewis Hamilton Lap 5"),
                                labels = c("Lap 5", "Lap 5")) +
             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
           
           
           p3 <- ggplot(data = ver_ad_2021_lap5, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 5")) +
             geom_line() +
             geom_line(data = ham_ad_2021_lap5, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 5")) +
             labs(y = "Brake", x = "Distance(in meters)") + 
             scale_color_manual(values = c("darkred", "steelblue"), 
                                name = "Max Verstappen vs Lewis Hamilton",
                                breaks = c("Max Verstappen Lap 5", "Lewis Hamilton Lap 5"),
                                labels = c("Lap 5", "Lap 5")) +
             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
           
           p4 <- ggplot(data = ver_ad_2021_lap5, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 5")) +
             geom_line() +
             geom_line(data = ham_ad_2021_lap5, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 5")) +
             labs(y = "Gear Number", x = "Distance(in meters)") + 
             scale_color_manual(values = c("darkred", "steelblue"), 
                                name = "Max Verstappen vs Lewis Hamilton",
                                breaks = c("Max Verstappen Lap 5", "Lewis Hamilton Lap 5"),
                                labels = c("Lap 5", "Lap 5")) +
             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
           
           p5 <- ggplot(data = ver_ad_2021_lap5, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 5")) +
             geom_line() +
             geom_line(data = ham_ad_2021_lap5, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 5")) +
             labs(y = "RPM", x = "Distance(in meters)") + 
             scale_color_manual(values = c("darkred", "steelblue"), 
                                name = "Max Verstappen vs Lewis Hamilton",
                                breaks = c("Max Verstappen Lap 5", "Lewis Hamilton Lap 5"),
                                labels = c("Lap 5", "Lap 5")) +
             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
           
           grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
           
         }
     
         else
         {
           if (input$lap_select == 6) {
             p1 <- ggplot(data = ver_ad_2021_lap6, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 6")) +
               geom_line() +
               geom_line(data = ham_ad_2021_lap6, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 6")) +
               labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
               scale_color_manual(values = c("darkred", "steelblue"), 
                                  name = "Max Verstappen Vs Lewis Hamilton",
                                  breaks = c("Max Verstappen Lap 6", "Lewis Hamilton Lap 6"),
                                  labels = c("Lap 6", "Lap 6"))
             
             p2 <- ggplot(data = ver_ad_2021_lap6, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 6")) +
               geom_line() +
               geom_line(data = ham_ad_2021_lap6, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 6")) +
               labs(y = "Throttle(%)", x = "Distance(in meters)") + 
               scale_color_manual(values = c("darkred", "steelblue"), 
                                  name = "Max Verstappen vs Lewis Hamilton",
                                  breaks = c("Max Verstappen Lap 6", "Lewis Hamilton Lap 6"),
                                  labels = c("Lap 6", "Lap 6")) +
               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
             
             
             p3 <- ggplot(data = ver_ad_2021_lap6, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 6")) +
               geom_line() +
               geom_line(data = ham_ad_2021_lap6, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 6")) +
               labs(y = "Brake", x = "Distance(in meters)") + 
               scale_color_manual(values = c("darkred", "steelblue"), 
                                  name = "Max Verstappen vs Lewis Hamilton",
                                  breaks = c("Max Verstappen Lap 6", "Lewis Hamilton Lap 6"),
                                  labels = c("Lap 6", "Lap 6")) +
               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
             
             p4 <- ggplot(data = ver_ad_2021_lap6, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 6")) +
               geom_line() +
               geom_line(data = ham_ad_2021_lap6, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 6")) +
               labs(y = "Gear Number", x = "Distance(in meters)") + 
               scale_color_manual(values = c("darkred", "steelblue"), 
                                  name = "Max Verstappen vs Lewis Hamilton",
                                  breaks = c("Max Verstappen Lap 6", "Lewis Hamilton Lap 6"),
                                  labels = c("Lap 6", "Lap 6")) +
               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
             
             p5 <- ggplot(data = ver_ad_2021_lap6, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 6")) +
               geom_line() +
               geom_line(data = ham_ad_2021_lap6, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 6")) +
               labs(y = "RPM", x = "Distance(in meters)") + 
               scale_color_manual(values = c("darkred", "steelblue"), 
                                  name = "Max Verstappen vs Lewis Hamilton",
                                  breaks = c("Max Verstappen Lap 6", "Lewis Hamilton Lap 6"),
                                  labels = c("Lap 6", "Lap 6")) +
               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
             
             grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
             
           }
           
           else
           {
             if (input$lap_select == 7) {
               p1 <- ggplot(data = ver_ad_2021_lap7, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 7")) +
                 geom_line() +
                 geom_line(data = ham_ad_2021_lap7, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 7")) +
                 labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                 scale_color_manual(values = c("darkred", "steelblue"), 
                                    name = "Max Verstappen Vs Lewis Hamilton",
                                    breaks = c("Max Verstappen Lap 7", "Lewis Hamilton Lap 7"),
                                    labels = c("Lap 7", "Lap 7"))
               
               p2 <- ggplot(data = ver_ad_2021_lap7, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 7")) +
                 geom_line() +
                 geom_line(data = ham_ad_2021_lap7, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 7")) +
                 labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                 scale_color_manual(values = c("darkred", "steelblue"), 
                                    name = "Max Verstappen vs Lewis Hamilton",
                                    breaks = c("Max Verstappen Lap 7", "Lewis Hamilton Lap 7"),
                                    labels = c("Lap 7", "Lap 7")) +
                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
               
               
               p3 <- ggplot(data = ver_ad_2021_lap7, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 7")) +
                 geom_line() +
                 geom_line(data = ham_ad_2021_lap7, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 7")) +
                 labs(y = "Brake", x = "Distance(in meters)") + 
                 scale_color_manual(values = c("darkred", "steelblue"), 
                                    name = "Max Verstappen vs Lewis Hamilton",
                                    breaks = c("Max Verstappen Lap 7", "Lewis Hamilton Lap 7"),
                                    labels = c("Lap 7", "Lap 7")) +
                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
               
               p4 <- ggplot(data = ver_ad_2021_lap7, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 7")) +
                 geom_line() +
                 geom_line(data = ham_ad_2021_lap7, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 7")) +
                 labs(y = "Gear Number", x = "Distance(in meters)") + 
                 scale_color_manual(values = c("darkred", "steelblue"), 
                                    name = "Max Verstappen vs Lewis Hamilton",
                                    breaks = c("Max Verstappen Lap 7", "Lewis Hamilton Lap 7"),
                                    labels = c("Lap 7", "Lap 7")) +
                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
               
               p5 <- ggplot(data = ver_ad_2021_lap7, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 7")) +
                 geom_line() +
                 geom_line(data = ham_ad_2021_lap7, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 7")) +
                 labs(y = "RPM", x = "Distance(in meters)") + 
                 scale_color_manual(values = c("darkred", "steelblue"), 
                                    name = "Max Verstappen vs Lewis Hamilton",
                                    breaks = c("Max Verstappen Lap 7", "Lewis Hamilton Lap 7"),
                                    labels = c("Lap 7", "Lap 7")) +
                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
               
               grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
               
             }
             else
             {
               if (input$lap_select == 8) {
                 p1 <- ggplot(data = ver_ad_2021_lap8, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 8")) +
                   geom_line() +
                   geom_line(data = ham_ad_2021_lap8, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 8")) +
                   labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                   scale_color_manual(values = c("darkred", "steelblue"), 
                                      name = "Max Verstappen Vs Lewis Hamilton",
                                      breaks = c("Max Verstappen Lap 8", "Lewis Hamilton Lap 8"),
                                      labels = c("Lap 8", "Lap 8"))
                 
                 p2 <- ggplot(data = ver_ad_2021_lap8, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 8")) +
                   geom_line() +
                   geom_line(data = ham_ad_2021_lap8, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 8")) +
                   labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                   scale_color_manual(values = c("darkred", "steelblue"), 
                                      name = "Max Verstappen vs Lewis Hamilton",
                                      breaks = c("Max Verstappen Lap 8", "Lewis Hamilton Lap 8"),
                                      labels = c("Lap 8", "Lap 8")) +
                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                 
                 
                 p3 <- ggplot(data = ver_ad_2021_lap8, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 8")) +
                   geom_line() +
                   geom_line(data = ham_ad_2021_lap8, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 8")) +
                   labs(y = "Brake", x = "Distance(in meters)") + 
                   scale_color_manual(values = c("darkred", "steelblue"), 
                                      name = "Max Verstappen vs Lewis Hamilton",
                                      breaks = c("Max Verstappen Lap 8", "Lewis Hamilton Lap 8"),
                                      labels = c("Lap 8", "Lap 8")) +
                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                 
                 p4 <- ggplot(data = ver_ad_2021_lap8, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 8")) +
                   geom_line() +
                   geom_line(data = ham_ad_2021_lap8, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 8")) +
                   labs(y = "Gear Number", x = "Distance(in meters)") + 
                   scale_color_manual(values = c("darkred", "steelblue"), 
                                      name = "Max Verstappen vs Lewis Hamilton",
                                      breaks = c("Max Verstappen Lap 8", "Lewis Hamilton Lap 8"),
                                      labels = c("Lap 8", "Lap 8")) +
                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                 
                 p5 <- ggplot(data = ver_ad_2021_lap8, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 8")) +
                   geom_line() +
                   geom_line(data = ham_ad_2021_lap8, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 8")) +
                   labs(y = "RPM", x = "Distance(in meters)") + 
                   scale_color_manual(values = c("darkred", "steelblue"), 
                                      name = "Max Verstappen vs Lewis Hamilton",
                                      breaks = c("Max Verstappen Lap 8", "Lewis Hamilton Lap 8"),
                                      labels = c("Lap 8", "Lap 8")) +
                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                 
                 grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                 
               }
    
               else
               {
                 if (input$lap_select == 9) {
                   p1 <- ggplot(data = ver_ad_2021_lap9, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 9")) +
                     geom_line() +
                     geom_line(data = ham_ad_2021_lap9, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 9")) +
                     labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                     scale_color_manual(values = c("darkred", "steelblue"), 
                                        name = "Max Verstappen Vs Lewis Hamilton",
                                        breaks = c("Max Verstappen Lap 9", "Lewis Hamilton Lap 9"),
                                        labels = c("Lap 9", "Lap 9"))
                   
                   p2 <- ggplot(data = ver_ad_2021_lap9, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 9")) +
                     geom_line() +
                     geom_line(data = ham_ad_2021_lap9, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 9")) +
                     labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                     scale_color_manual(values = c("darkred", "steelblue"), 
                                        name = "Max Verstappen vs Lewis Hamilton",
                                        breaks = c("Max Verstappen Lap 9", "Lewis Hamilton Lap 9"),
                                        labels = c("Lap 9", "Lap 9")) +
                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                   
                   
                   p3 <- ggplot(data = ver_ad_2021_lap9, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 9")) +
                     geom_line() +
                     geom_line(data = ham_ad_2021_lap9, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 9")) +
                     labs(y = "Brake", x = "Distance(in meters)") + 
                     scale_color_manual(values = c("darkred", "steelblue"), 
                                        name = "Max Verstappen vs Lewis Hamilton",
                                        breaks = c("Max Verstappen Lap 9", "Lewis Hamilton Lap 9"),
                                        labels = c("Lap 9", "Lap 9")) +
                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                   
                   p4 <- ggplot(data = ver_ad_2021_lap9, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 9")) +
                     geom_line() +
                     geom_line(data = ham_ad_2021_lap9, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 9")) +
                     labs(y = "Gear Number", x = "Distance(in meters)") + 
                     scale_color_manual(values = c("darkred", "steelblue"), 
                                        name = "Max Verstappen vs Lewis Hamilton",
                                        breaks = c("Max Verstappen Lap 9", "Lewis Hamilton Lap 9"),
                                        labels = c("Lap 9", "Lap 9")) +
                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                   
                   p5 <- ggplot(data = ver_ad_2021_lap9, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 9")) +
                     geom_line() +
                     geom_line(data = ham_ad_2021_lap9, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 9")) +
                     labs(y = "RPM", x = "Distance(in meters)") + 
                     scale_color_manual(values = c("darkred", "steelblue"), 
                                        name = "Max Verstappen vs Lewis Hamilton",
                                        breaks = c("Max Verstappen Lap 9", "Lewis Hamilton Lap 9"),
                                        labels = c("Lap 9", "Lap 9")) +
                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                   
                   grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                   
                 }
      
                 else
                 {
                   if (input$lap_select == 10) {
                     p1 <- ggplot(data = ver_ad_2021_lap10, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 10")) +
                       geom_line() +
                       geom_line(data = ham_ad_2021_lap10, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 10")) +
                       labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                       scale_color_manual(values = c("darkred", "steelblue"), 
                                          name = "Max Verstappen Vs Lewis Hamilton",
                                          breaks = c("Max Verstappen Lap 10", "Lewis Hamilton Lap 10"),
                                          labels = c("Lap 10", "Lap 10"))
                     
                     p2 <- ggplot(data = ver_ad_2021_lap10, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 10")) +
                       geom_line() +
                       geom_line(data = ham_ad_2021_lap10, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 10")) +
                       labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                       scale_color_manual(values = c("darkred", "steelblue"), 
                                          name = "Max Verstappen vs Lewis Hamilton",
                                          breaks = c("Max Verstappen Lap 10", "Lewis Hamilton Lap 10"),
                                          labels = c("Lap 10", "Lap 10")) +
                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                     
                     
                     p3 <- ggplot(data = ver_ad_2021_lap10, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 10")) +
                       geom_line() +
                       geom_line(data = ham_ad_2021_lap10, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 10")) +
                       labs(y = "Brake", x = "Distance(in meters)") + 
                       scale_color_manual(values = c("darkred", "steelblue"), 
                                          name = "Max Verstappen vs Lewis Hamilton",
                                          breaks = c("Max Verstappen Lap 10", "Lewis Hamilton Lap 10"),
                                          labels = c("Lap 10", "Lap 10")) +
                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                     
                     p4 <- ggplot(data = ver_ad_2021_lap10, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 10")) +
                       geom_line() +
                       geom_line(data = ham_ad_2021_lap10, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 10")) +
                       labs(y = "Gear Number", x = "Distance(in meters)") + 
                       scale_color_manual(values = c("darkred", "steelblue"), 
                                          name = "Max Verstappen vs Lewis Hamilton",
                                          breaks = c("Max Verstappen Lap 10", "Lewis Hamilton Lap 10"),
                                          labels = c("Lap 10", "Lap 10")) +
                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                     
                     p5 <- ggplot(data = ver_ad_2021_lap10, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 10")) +
                       geom_line() +
                       geom_line(data = ham_ad_2021_lap10, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 10")) +
                       labs(y = "RPM", x = "Distance(in meters)") + 
                       scale_color_manual(values = c("darkred", "steelblue"), 
                                          name = "Max Verstappen vs Lewis Hamilton",
                                          breaks = c("Max Verstappen Lap 10", "Lewis Hamilton Lap 10"),
                                          labels = c("Lap 10", "Lap 10")) +
                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                     
                     grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                     
                   }
         
                   else
                   {
                     if (input$lap_select == 11) {
                       p1 <- ggplot(data = ver_ad_2021_lap11, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 11")) +
                         geom_line() +
                         geom_line(data = ham_ad_2021_lap11, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 11")) +
                         labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                         scale_color_manual(values = c("darkred", "steelblue"), 
                                            name = "Max Verstappen Vs Lewis Hamilton",
                                            breaks = c("Max Verstappen Lap 11", "Lewis Hamilton Lap 11"),
                                            labels = c("Lap 11", "Lap 11"))
                       
                       p2 <- ggplot(data = ver_ad_2021_lap11, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 11")) +
                         geom_line() +
                         geom_line(data = ham_ad_2021_lap11, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 11")) +
                         labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                         scale_color_manual(values = c("darkred", "steelblue"), 
                                            name = "Max Verstappen vs Lewis Hamilton",
                                            breaks = c("Max Verstappen Lap 11", "Lewis Hamilton Lap 11"),
                                            labels = c("Lap 11", "Lap 11")) +
                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                       
                       
                       p3 <- ggplot(data = ver_ad_2021_lap11, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 11")) +
                         geom_line() +
                         geom_line(data = ham_ad_2021_lap11, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 11")) +
                         labs(y = "Brake", x = "Distance(in meters)") + 
                         scale_color_manual(values = c("darkred", "steelblue"), 
                                            name = "Max Verstappen vs Lewis Hamilton",
                                            breaks = c("Max Verstappen Lap 11", "Lewis Hamilton Lap 11"),
                                            labels = c("Lap 11", "Lap 11")) +
                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                       
                       p4 <- ggplot(data = ver_ad_2021_lap11, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 11")) +
                         geom_line() +
                         geom_line(data = ham_ad_2021_lap11, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 11")) +
                         labs(y = "Gear Number", x = "Distance(in meters)") + 
                         scale_color_manual(values = c("darkred", "steelblue"), 
                                            name = "Max Verstappen vs Lewis Hamilton",
                                            breaks = c("Max Verstappen Lap 11", "Lewis Hamilton Lap 11"),
                                            labels = c("Lap 11", "Lap 11")) +
                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                       
                       p5 <- ggplot(data = ver_ad_2021_lap11, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 11")) +
                         geom_line() +
                         geom_line(data = ham_ad_2021_lap11, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 11")) +
                         labs(y = "RPM", x = "Distance(in meters)") + 
                         scale_color_manual(values = c("darkred", "steelblue"), 
                                            name = "Max Verstappen vs Lewis Hamilton",
                                            breaks = c("Max Verstappen Lap 11", "Lewis Hamilton Lap 11"),
                                            labels = c("Lap 11", "Lap 11")) +
                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                       
                       grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                       
                     }
                   
                     else
                     {
                       if (input$lap_select == 12) {
                         p1 <- ggplot(data = ver_ad_2021_lap12, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 12")) +
                           geom_line() +
                           geom_line(data = ham_ad_2021_lap12, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 12")) +
                           labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                           scale_color_manual(values = c("darkred", "steelblue"), 
                                              name = "Max Verstappen Vs Lewis Hamilton",
                                              breaks = c("Max Verstappen Lap 12", "Lewis Hamilton Lap 12"),
                                              labels = c("Lap 12", "Lap 12"))
                         
                         p2 <- ggplot(data = ver_ad_2021_lap12, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 12")) +
                           geom_line() +
                           geom_line(data = ham_ad_2021_lap12, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 12")) +
                           labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                           scale_color_manual(values = c("darkred", "steelblue"), 
                                              name = "Max Verstappen vs Lewis Hamilton",
                                              breaks = c("Max Verstappen Lap 12", "Lewis Hamilton Lap 12"),
                                              labels = c("Lap 12", "Lap 12")) +
                           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                         
                         
                         p3 <- ggplot(data = ver_ad_2021_lap12, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 12")) +
                           geom_line() +
                           geom_line(data = ham_ad_2021_lap12, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 12")) +
                           labs(y = "Brake", x = "Distance(in meters)") + 
                           scale_color_manual(values = c("darkred", "steelblue"), 
                                              name = "Max Verstappen vs Lewis Hamilton",
                                              breaks = c("Max Verstappen Lap 12", "Lewis Hamilton Lap 12"),
                                              labels = c("Lap 12", "Lap 12")) +
                           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                         
                         p4 <- ggplot(data = ver_ad_2021_lap12, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 12")) +
                           geom_line() +
                           geom_line(data = ham_ad_2021_lap12, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 12")) +
                           labs(y = "Gear Number", x = "Distance(in meters)") + 
                           scale_color_manual(values = c("darkred", "steelblue"), 
                                              name = "Max Verstappen vs Lewis Hamilton",
                                              breaks = c("Max Verstappen Lap 12", "Lewis Hamilton Lap 12"),
                                              labels = c("Lap 12", "Lap 12")) +
                           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                         
                         p5 <- ggplot(data = ver_ad_2021_lap12, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 12")) +
                           geom_line() +
                           geom_line(data = ham_ad_2021_lap12, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 12")) +
                           labs(y = "RPM", x = "Distance(in meters)") + 
                           scale_color_manual(values = c("darkred", "steelblue"), 
                                              name = "Max Verstappen vs Lewis Hamilton",
                                              breaks = c("Max Verstappen Lap 12", "Lewis Hamilton Lap 12"),
                                              labels = c("Lap 12", "Lap 12")) +
                           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                         
                         grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                         
                       }
                       
                       else
                       {
                         if (input$lap_select == 13) {
                           p1 <- ggplot(data = ver_ad_2021_lap13, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 13")) +
                             geom_line() +
                             geom_line(data = ham_ad_2021_lap13, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 13")) +
                             labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                name = "Max Verstappen Vs Lewis Hamilton",
                                                breaks = c("Max Verstappen Lap 13", "Lewis Hamilton Lap 13"),
                                                labels = c("Lap 13", "Lap 13"))
                           
                           p2 <- ggplot(data = ver_ad_2021_lap13, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 13")) +
                             geom_line() +
                             geom_line(data = ham_ad_2021_lap13, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 13")) +
                             labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                name = "Max Verstappen vs Lewis Hamilton",
                                                breaks = c("Max Verstappen Lap 13", "Lewis Hamilton Lap 13"),
                                                labels = c("Lap 13", "Lap 13")) +
                             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                           
                           
                           p3 <- ggplot(data = ver_ad_2021_lap13, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 13")) +
                             geom_line() +
                             geom_line(data = ham_ad_2021_lap13, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 13")) +
                             labs(y = "Brake", x = "Distance(in meters)") + 
                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                name = "Max Verstappen vs Lewis Hamilton",
                                                breaks = c("Max Verstappen Lap 13", "Lewis Hamilton Lap 13"),
                                                labels = c("Lap 13", "Lap 13")) +
                             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                           
                           p4 <- ggplot(data = ver_ad_2021_lap13, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 13")) +
                             geom_line() +
                             geom_line(data = ham_ad_2021_lap13, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 13")) +
                             labs(y = "Gear Number", x = "Distance(in meters)") + 
                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                name = "Max Verstappen vs Lewis Hamilton",
                                                breaks = c("Max Verstappen Lap 13", "Lewis Hamilton Lap 13"),
                                                labels = c("Lap 13", "Lap 13")) +
                             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                           
                           p5 <- ggplot(data = ver_ad_2021_lap13, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 13")) +
                             geom_line() +
                             geom_line(data = ham_ad_2021_lap13, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 13")) +
                             labs(y = "RPM", x = "Distance(in meters)") + 
                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                name = "Max Verstappen vs Lewis Hamilton",
                                                breaks = c("Max Verstappen Lap 13", "Lewis Hamilton Lap 13"),
                                                labels = c("Lap 13", "Lap 13")) +
                             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                           
                           grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                           
                         }
                         
                         else
                         {
                           if (input$lap_select == 14) {
                             p1 <- ggplot(data = ver_ad_2021_lap14, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 14")) +
                               geom_line() +
                               geom_line(data = ham_ad_2021_lap14, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 14")) +
                               labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                  name = "Max Verstappen Vs Lewis Hamilton",
                                                  breaks = c("Max Verstappen Lap 14", "Lewis Hamilton Lap 14"),
                                                  labels = c("Lap 14", "Lap 14"))
                             
                             p2 <- ggplot(data = ver_ad_2021_lap14, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 14")) +
                               geom_line() +
                               geom_line(data = ham_ad_2021_lap14, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 14")) +
                               labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                  name = "Max Verstappen vs Lewis Hamilton",
                                                  breaks = c("Max Verstappen Lap 14", "Lewis Hamilton Lap 14"),
                                                  labels = c("Lap 14", "Lap 14")) +
                               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                             
                             
                             p3 <- ggplot(data = ver_ad_2021_lap14, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 14")) +
                               geom_line() +
                               geom_line(data = ham_ad_2021_lap14, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 14")) +
                               labs(y = "Brake", x = "Distance(in meters)") + 
                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                  name = "Max Verstappen vs Lewis Hamilton",
                                                  breaks = c("Max Verstappen Lap 14", "Lewis Hamilton Lap 14"),
                                                  labels = c("Lap 14", "Lap 14")) +
                               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                             
                             p4 <- ggplot(data = ver_ad_2021_lap14, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 14")) +
                               geom_line() +
                               geom_line(data = ham_ad_2021_lap14, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 14")) +
                               labs(y = "Gear Number", x = "Distance(in meters)") + 
                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                  name = "Max Verstappen vs Lewis Hamilton",
                                                  breaks = c("Max Verstappen Lap 14", "Lewis Hamilton Lap 14"),
                                                  labels = c("Lap 14", "Lap 14")) +
                               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                             
                             p5 <- ggplot(data = ver_ad_2021_lap14, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 14")) +
                               geom_line() +
                               geom_line(data = ham_ad_2021_lap14, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 14")) +
                               labs(y = "RPM", x = "Distance(in meters)") + 
                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                  name = "Max Verstappen vs Lewis Hamilton",
                                                  breaks = c("Max Verstappen Lap 14", "Lewis Hamilton Lap 14"),
                                                  labels = c("Lap 14", "Lap 14")) +
                               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                             
                             grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                             
                           }
                           
                           else
                           {
                             if (input$lap_select == 15) {
                               p1 <- ggplot(data = ver_ad_2021_lap15, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 15")) +
                                 geom_line() +
                                 geom_line(data = ham_ad_2021_lap15, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 15")) +
                                 labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                    name = "Max Verstappen Vs Lewis Hamilton",
                                                    breaks = c("Max Verstappen Lap 15", "Lewis Hamilton Lap 15"),
                                                    labels = c("Lap 15", "Lap 15"))
                               
                               p2 <- ggplot(data = ver_ad_2021_lap15, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 15")) +
                                 geom_line() +
                                 geom_line(data = ham_ad_2021_lap15, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 15")) +
                                 labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                    name = "Max Verstappen vs Lewis Hamilton",
                                                    breaks = c("Max Verstappen Lap 15", "Lewis Hamilton Lap 15"),
                                                    labels = c("Lap 15", "Lap 15")) +
                                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                               
                               
                               p3 <- ggplot(data = ver_ad_2021_lap15, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 15")) +
                                 geom_line() +
                                 geom_line(data = ham_ad_2021_lap15, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 15")) +
                                 labs(y = "Brake", x = "Distance(in meters)") + 
                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                    name = "Max Verstappen vs Lewis Hamilton",
                                                    breaks = c("Max Verstappen Lap 15", "Lewis Hamilton Lap 15"),
                                                    labels = c("Lap 15", "Lap 15")) +
                                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                               
                               p4 <- ggplot(data = ver_ad_2021_lap15, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 15")) +
                                 geom_line() +
                                 geom_line(data = ham_ad_2021_lap15, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 15")) +
                                 labs(y = "Gear Number", x = "Distance(in meters)") + 
                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                    name = "Max Verstappen vs Lewis Hamilton",
                                                    breaks = c("Max Verstappen Lap 15", "Lewis Hamilton Lap 15"),
                                                    labels = c("Lap 15", "Lap 15")) +
                                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                               
                               p5 <- ggplot(data = ver_ad_2021_lap15, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 15")) +
                                 geom_line() +
                                 geom_line(data = ham_ad_2021_lap15, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 15")) +
                                 labs(y = "RPM", x = "Distance(in meters)") + 
                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                    name = "Max Verstappen vs Lewis Hamilton",
                                                    breaks = c("Max Verstappen Lap 15", "Lewis Hamilton Lap 15"),
                                                    labels = c("Lap 15", "Lap 15")) +
                                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                               
                               grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                               
                             }
                             
                             else
                             {
                               if (input$lap_select == 16) {
                                 p1 <- ggplot(data = ver_ad_2021_lap16, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 16")) +
                                   geom_line() +
                                   geom_line(data = ham_ad_2021_lap16, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 16")) +
                                   labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                      name = "Max Verstappen Vs Lewis Hamilton",
                                                      breaks = c("Max Verstappen Lap 16", "Lewis Hamilton Lap 16"),
                                                      labels = c("Lap 16", "Lap 16"))
                                 
                                 p2 <- ggplot(data = ver_ad_2021_lap16, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 16")) +
                                   geom_line() +
                                   geom_line(data = ham_ad_2021_lap16, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 16")) +
                                   labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                      name = "Max Verstappen vs Lewis Hamilton",
                                                      breaks = c("Max Verstappen Lap 16", "Lewis Hamilton Lap 16"),
                                                      labels = c("Lap 16", "Lap 16")) +
                                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                 
                                 ver_ad_2021_lap16$Brake <- ifelse(ver_ad_2021_lap16$Brake, 1, 0)
                                 ham_ad_2021_lap16$Brake <- ifelse(ham_ad_2021_lap16$Brake, 1, 0)
                                 
                                 p3 <- ggplot(data = ver_ad_2021_lap16, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 16")) +
                                   geom_line() +
                                   geom_line(data = ham_ad_2021_lap16, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 16")) +
                                   labs(y = "Brake", x = "Distance(in meters)") + 
                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                      name = "Max Verstappen vs Lewis Hamilton",
                                                      breaks = c("Max Verstappen Lap 16", "Lewis Hamilton Lap 16"),
                                                      labels = c("Lap 16", "Lap 16")) +
                                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                 
                                 p4 <- ggplot(data = ver_ad_2021_lap16, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 16")) +
                                   geom_line() +
                                   geom_line(data = ham_ad_2021_lap16, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 16")) +
                                   labs(y = "Gear Number", x = "Distance(in meters)") + 
                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                      name = "Max Verstappen vs Lewis Hamilton",
                                                      breaks = c("Max Verstappen Lap 16", "Lewis Hamilton Lap 16"),
                                                      labels = c("Lap 16", "Lap 16")) +
                                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                 
                                 p5 <- ggplot(data = ver_ad_2021_lap16, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 16")) +
                                   geom_line() +
                                   geom_line(data = ham_ad_2021_lap16, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 16")) +
                                   labs(y = "RPM", x = "Distance(in meters)") + 
                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                      name = "Max Verstappen vs Lewis Hamilton",
                                                      breaks = c("Max Verstappen Lap 16", "Lewis Hamilton Lap 16"),
                                                      labels = c("Lap 16", "Lap 16")) +
                                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                 
                                 grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                 
                               }
                               

                             
                               else
                               {
                                 if (input$lap_select == 17) {
                                   p1 <- ggplot(data = ver_ad_2021_lap17, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 17")) +
                                     geom_line() +
                                     geom_line(data = ham_ad_2021_lap17, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 17")) +
                                     labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                        name = "Max Verstappen Vs Lewis Hamilton",
                                                        breaks = c("Max Verstappen Lap 17", "Lewis Hamilton Lap 17"),
                                                        labels = c("Lap 17", "Lap 17"))
                                   
                                   p2 <- ggplot(data = ver_ad_2021_lap17, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 17")) +
                                     geom_line() +
                                     geom_line(data = ham_ad_2021_lap17, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 17")) +
                                     labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                        name = "Max Verstappen vs Lewis Hamilton",
                                                        breaks = c("Max Verstappen Lap 17", "Lewis Hamilton Lap 17"),
                                                        labels = c("Lap 17", "Lap 17")) +
                                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                   
                                   ver_ad_2021_lap17$Brake <- ifelse(ver_ad_2021_lap17$Brake, 1, 0)
                                   ham_ad_2021_lap17$Brake <- ifelse(ham_ad_2021_lap17$Brake, 1, 0)
                                   
                                   p3 <- ggplot(data = ver_ad_2021_lap17, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 17")) +
                                     geom_line() +
                                     geom_line(data = ham_ad_2021_lap17, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 17")) +
                                     labs(y = "Brake", x = "Distance(in meters)") + 
                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                        name = "Max Verstappen vs Lewis Hamilton",
                                                        breaks = c("Max Verstappen Lap 17", "Lewis Hamilton Lap 17"),
                                                        labels = c("Lap 17", "Lap 17")) +
                                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                   
                                   p4 <- ggplot(data = ver_ad_2021_lap17, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 17")) +
                                     geom_line() +
                                     geom_line(data = ham_ad_2021_lap17, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 17")) +
                                     labs(y = "Gear Number", x = "Distance(in meters)") + 
                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                        name = "Max Verstappen vs Lewis Hamilton",
                                                        breaks = c("Max Verstappen Lap 17", "Lewis Hamilton Lap 17"),
                                                        labels = c("Lap 17", "Lap 17")) +
                                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                   
                                   p5 <- ggplot(data = ver_ad_2021_lap17, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 17")) +
                                     geom_line() +
                                     geom_line(data = ham_ad_2021_lap17, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 17")) +
                                     labs(y = "RPM", x = "Distance(in meters)") + 
                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                        name = "Max Verstappen vs Lewis Hamilton",
                                                        breaks = c("Max Verstappen Lap 17", "Lewis Hamilton Lap 17"),
                                                        labels = c("Lap 17", "Lap 17")) +
                                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                   
                                   grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                   
                                 }
                                 
                                 else
                                 {
                                   if (input$lap_select == 18) {
                                     p1 <- ggplot(data = ver_ad_2021_lap18, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 18")) +
                                       geom_line() +
                                       geom_line(data = ham_ad_2021_lap18, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 18")) +
                                       labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                          name = "Max Verstappen Vs Lewis Hamilton",
                                                          breaks = c("Max Verstappen Lap 18", "Lewis Hamilton Lap 18"),
                                                          labels = c("Lap 18", "Lap 18"))
                                     
                                     p2 <- ggplot(data = ver_ad_2021_lap18, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 18")) +
                                       geom_line() +
                                       geom_line(data = ham_ad_2021_lap18, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 18")) +
                                       labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                          name = "Max Verstappen vs Lewis Hamilton",
                                                          breaks = c("Max Verstappen Lap 18", "Lewis Hamilton Lap 18"),
                                                          labels = c("Lap 18", "Lap 18")) +
                                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                     
                                     ver_ad_2021_lap18$Brake <- ifelse(ver_ad_2021_lap18$Brake, 1, 0)
                                     ham_ad_2021_lap18$Brake <- ifelse(ham_ad_2021_lap18$Brake, 1, 0)
                                     
                                     p3 <- ggplot(data = ver_ad_2021_lap18, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 18")) +
                                       geom_line() +
                                       geom_line(data = ham_ad_2021_lap18, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 18")) +
                                       labs(y = "Brake", x = "Distance(in meters)") + 
                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                          name = "Max Verstappen vs Lewis Hamilton",
                                                          breaks = c("Max Verstappen Lap 18", "Lewis Hamilton Lap 18"),
                                                          labels = c("Lap 18", "Lap 18")) +
                                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                     
                                     p4 <- ggplot(data = ver_ad_2021_lap18, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 18")) +
                                       geom_line() +
                                       geom_line(data = ham_ad_2021_lap18, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 18")) +
                                       labs(y = "Gear Number", x = "Distance(in meters)") + 
                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                          name = "Max Verstappen vs Lewis Hamilton",
                                                          breaks = c("Max Verstappen Lap 18", "Lewis Hamilton Lap 18"),
                                                          labels = c("Lap 18", "Lap 18")) +
                                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                     
                                     p5 <- ggplot(data = ver_ad_2021_lap18, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 18")) +
                                       geom_line() +
                                       geom_line(data = ham_ad_2021_lap18, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 18")) +
                                       labs(y = "RPM", x = "Distance(in meters)") + 
                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                          name = "Max Verstappen vs Lewis Hamilton",
                                                          breaks = c("Max Verstappen Lap 18", "Lewis Hamilton Lap 18"),
                                                          labels = c("Lap 18", "Lap 18")) +
                                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                     
                                     grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                     
                                   }
                                   
                                   else
                                   {
                                     if (input$lap_select == 19) {
                                       p1 <- ggplot(data = ver_ad_2021_lap19, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 19")) +
                                         geom_line() +
                                         geom_line(data = ham_ad_2021_lap19, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 19")) +
                                         labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                            name = "Max Verstappen Vs Lewis Hamilton",
                                                            breaks = c("Max Verstappen Lap 19", "Lewis Hamilton Lap 19"),
                                                            labels = c("Lap 19", "Lap 19"))
                                       
                                       p2 <- ggplot(data = ver_ad_2021_lap19, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 19")) +
                                         geom_line() +
                                         geom_line(data = ham_ad_2021_lap19, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 19")) +
                                         labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                            name = "Max Verstappen vs Lewis Hamilton",
                                                            breaks = c("Max Verstappen Lap 19", "Lewis Hamilton Lap 19"),
                                                            labels = c("Lap 19", "Lap 19")) +
                                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                       
                                       ver_ad_2021_lap19$Brake <- ifelse(ver_ad_2021_lap19$Brake, 1, 0)
                                       ham_ad_2021_lap19$Brake <- ifelse(ham_ad_2021_lap19$Brake, 1, 0)
                                       
                                       p3 <- ggplot(data = ver_ad_2021_lap19, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 19")) +
                                         geom_line() +
                                         geom_line(data = ham_ad_2021_lap19, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 19")) +
                                         labs(y = "Brake", x = "Distance(in meters)") + 
                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                            name = "Max Verstappen vs Lewis Hamilton",
                                                            breaks = c("Max Verstappen Lap 19", "Lewis Hamilton Lap 19"),
                                                            labels = c("Lap 19", "Lap 19")) +
                                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                       
                                       p4 <- ggplot(data = ver_ad_2021_lap19, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 19")) +
                                         geom_line() +
                                         geom_line(data = ham_ad_2021_lap19, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 19")) +
                                         labs(y = "Gear Number", x = "Distance(in meters)") + 
                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                            name = "Max Verstappen vs Lewis Hamilton",
                                                            breaks = c("Max Verstappen Lap 19", "Lewis Hamilton Lap 19"),
                                                            labels = c("Lap 19", "Lap 19")) +
                                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                       
                                       p5 <- ggplot(data = ver_ad_2021_lap19, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 19")) +
                                         geom_line() +
                                         geom_line(data = ham_ad_2021_lap19, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 19")) +
                                         labs(y = "RPM", x = "Distance(in meters)") + 
                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                            name = "Max Verstappen vs Lewis Hamilton",
                                                            breaks = c("Max Verstappen Lap 19", "Lewis Hamilton Lap 19"),
                                                            labels = c("Lap 19", "Lap 19")) +
                                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                       
                                       grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                       
                                     }
                                     
                                     else
                                     {
                                       if (input$lap_select == 20) {
                                         p1 <- ggplot(data = ver_ad_2021_lap20, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 20")) +
                                           geom_line() +
                                           geom_line(data = ham_ad_2021_lap20, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 20")) +
                                           labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                           scale_color_manual(values = c("darkred", "steelblue"), 
                                                              name = "Max Verstappen Vs Lewis Hamilton",
                                                              breaks = c("Max Verstappen Lap 20", "Lewis Hamilton Lap 20"),
                                                              labels = c("Lap 20", "Lap 20"))
                                         
                                         p2 <- ggplot(data = ver_ad_2021_lap20, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 20")) +
                                           geom_line() +
                                           geom_line(data = ham_ad_2021_lap20, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 20")) +
                                           labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                           scale_color_manual(values = c("darkred", "steelblue"), 
                                                              name = "Max Verstappen vs Lewis Hamilton",
                                                              breaks = c("Max Verstappen Lap 20", "Lewis Hamilton Lap 20"),
                                                              labels = c("Lap 20", "Lap 20")) +
                                           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                         
                                         ver_ad_2021_lap20$Brake <- ifelse(ver_ad_2021_lap20$Brake, 1, 0)
                                         ham_ad_2021_lap20$Brake <- ifelse(ham_ad_2021_lap20$Brake, 1, 0)
                                         
                                         p3 <- ggplot(data = ver_ad_2021_lap20, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 20")) +
                                           geom_line() +
                                           geom_line(data = ham_ad_2021_lap20, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 20")) +
                                           labs(y = "Brake", x = "Distance(in meters)") + 
                                           scale_color_manual(values = c("darkred", "steelblue"), 
                                                              name = "Max Verstappen vs Lewis Hamilton",
                                                              breaks = c("Max Verstappen Lap 20", "Lewis Hamilton Lap 20"),
                                                              labels = c("Lap 20", "Lap 20")) +
                                           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                         
                                         p4 <- ggplot(data = ver_ad_2021_lap20, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 20")) +
                                           geom_line() +
                                           geom_line(data = ham_ad_2021_lap20, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 20")) +
                                           labs(y = "Gear Number", x = "Distance(in meters)") + 
                                           scale_color_manual(values = c("darkred", "steelblue"), 
                                                              name = "Max Verstappen vs Lewis Hamilton",
                                                              breaks = c("Max Verstappen Lap 20", "Lewis Hamilton Lap 20"),
                                                              labels = c("Lap 20", "Lap 20")) +
                                           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                         
                                         p5 <- ggplot(data = ver_ad_2021_lap20, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 20")) +
                                           geom_line() +
                                           geom_line(data = ham_ad_2021_lap20, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 20")) +
                                           labs(y = "RPM", x = "Distance(in meters)") + 
                                           scale_color_manual(values = c("darkred", "steelblue"), 
                                                              name = "Max Verstappen vs Lewis Hamilton",
                                                              breaks = c("Max Verstappen Lap 20", "Lewis Hamilton Lap 20"),
                                                              labels = c("Lap 20", "Lap 20")) +
                                           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                         
                                         grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                         
                                       }
                                       
                                       else
                                       {
                                         if (input$lap_select == 21) {
                                           p1 <- ggplot(data = ver_ad_2021_lap21, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 21")) +
                                             geom_line() +
                                             geom_line(data = ham_ad_2021_lap21, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 21")) +
                                             labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                                name = "Max Verstappen Vs Lewis Hamilton",
                                                                breaks = c("Max Verstappen Lap 21", "Lewis Hamilton Lap 21"),
                                                                labels = c("Lap 21", "Lap 21"))
                                           
                                           p2 <- ggplot(data = ver_ad_2021_lap21, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 21")) +
                                             geom_line() +
                                             geom_line(data = ham_ad_2021_lap21, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 21")) +
                                             labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                                name = "Max Verstappen vs Lewis Hamilton",
                                                                breaks = c("Max Verstappen Lap 21", "Lewis Hamilton Lap 21"),
                                                                labels = c("Lap 21", "Lap 21")) +
                                             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                           
                                           ver_ad_2021_lap21$Brake <- ifelse(ver_ad_2021_lap21$Brake, 1, 0)
                                           ham_ad_2021_lap21$Brake <- ifelse(ham_ad_2021_lap21$Brake, 1, 0)
                                           
                                           p3 <- ggplot(data = ver_ad_2021_lap21, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 21")) +
                                             geom_line() +
                                             geom_line(data = ham_ad_2021_lap21, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 21")) +
                                             labs(y = "Brake", x = "Distance(in meters)") + 
                                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                                name = "Max Verstappen vs Lewis Hamilton",
                                                                breaks = c("Max Verstappen Lap 21", "Lewis Hamilton Lap 21"),
                                                                labels = c("Lap 21", "Lap 21")) +
                                             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                           
                                           p4 <- ggplot(data = ver_ad_2021_lap21, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 21")) +
                                             geom_line() +
                                             geom_line(data = ham_ad_2021_lap21, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 21")) +
                                             labs(y = "Gear Number", x = "Distance(in meters)") + 
                                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                                name = "Max Verstappen vs Lewis Hamilton",
                                                                breaks = c("Max Verstappen Lap 21", "Lewis Hamilton Lap 21"),
                                                                labels = c("Lap 21", "Lap 21")) +
                                             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                           
                                           p5 <- ggplot(data = ver_ad_2021_lap21, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 21")) +
                                             geom_line() +
                                             geom_line(data = ham_ad_2021_lap21, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 21")) +
                                             labs(y = "RPM", x = "Distance(in meters)") + 
                                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                                name = "Max Verstappen vs Lewis Hamilton",
                                                                breaks = c("Max Verstappen Lap 21", "Lewis Hamilton Lap 21"),
                                                                labels = c("Lap 21", "Lap 21")) +
                                             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                           
                                           grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                           
                                         }
                                         
                                         else
                                         {
                                           if (input$lap_select == 22) {
                                             p1 <- ggplot(data = ver_ad_2021_lap22, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 22")) +
                                               geom_line() +
                                               geom_line(data = ham_ad_2021_lap22, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 22")) +
                                               labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                                  name = "Max Verstappen Vs Lewis Hamilton",
                                                                  breaks = c("Max Verstappen Lap 22", "Lewis Hamilton Lap 22"),
                                                                  labels = c("Lap 22", "Lap 22"))
                                             
                                             p2 <- ggplot(data = ver_ad_2021_lap22, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 22")) +
                                               geom_line() +
                                               geom_line(data = ham_ad_2021_lap22, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 22")) +
                                               labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                                  name = "Max Verstappen vs Lewis Hamilton",
                                                                  breaks = c("Max Verstappen Lap 22", "Lewis Hamilton Lap 22"),
                                                                  labels = c("Lap 22", "Lap 22")) +
                                               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                             
                                             ver_ad_2021_lap22$Brake <- ifelse(ver_ad_2021_lap22$Brake, 1, 0)
                                             ham_ad_2021_lap22$Brake <- ifelse(ham_ad_2021_lap22$Brake, 1, 0)
                                             
                                             p3 <- ggplot(data = ver_ad_2021_lap22, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 22")) +
                                               geom_line() +
                                               geom_line(data = ham_ad_2021_lap22, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 22")) +
                                               labs(y = "Brake", x = "Distance(in meters)") + 
                                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                                  name = "Max Verstappen vs Lewis Hamilton",
                                                                  breaks = c("Max Verstappen Lap 22", "Lewis Hamilton Lap 22"),
                                                                  labels = c("Lap 22", "Lap 22")) +
                                               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                             
                                             p4 <- ggplot(data = ver_ad_2021_lap22, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 22")) +
                                               geom_line() +
                                               geom_line(data = ham_ad_2021_lap22, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 22")) +
                                               labs(y = "Gear Number", x = "Distance(in meters)") + 
                                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                                  name = "Max Verstappen vs Lewis Hamilton",
                                                                  breaks = c("Max Verstappen Lap 22", "Lewis Hamilton Lap 22"),
                                                                  labels = c("Lap 22", "Lap 22")) +
                                               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                             
                                             p5 <- ggplot(data = ver_ad_2021_lap22, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 22")) +
                                               geom_line() +
                                               geom_line(data = ham_ad_2021_lap22, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 22")) +
                                               labs(y = "RPM", x = "Distance(in meters)") + 
                                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                                  name = "Max Verstappen vs Lewis Hamilton",
                                                                  breaks = c("Max Verstappen Lap 22", "Lewis Hamilton Lap 22"),
                                                                  labels = c("Lap 22", "Lap 22")) +
                                               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                             
                                             grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                             
                                           }
                    
                                           else
                                           {
                                             if (input$lap_select == 23) {
                                               p1 <- ggplot(data = ver_ad_2021_lap23, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 23")) +
                                                 geom_line() +
                                                 geom_line(data = ham_ad_2021_lap23, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 23")) +
                                                 labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                                    name = "Max Verstappen Vs Lewis Hamilton",
                                                                    breaks = c("Max Verstappen Lap 23", "Lewis Hamilton Lap 23"),
                                                                    labels = c("Lap 23", "Lap 23"))
                                               
                                               p2 <- ggplot(data = ver_ad_2021_lap23, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 23")) +
                                                 geom_line() +
                                                 geom_line(data = ham_ad_2021_lap23, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 23")) +
                                                 labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                                    name = "Max Verstappen vs Lewis Hamilton",
                                                                    breaks = c("Max Verstappen Lap 23", "Lewis Hamilton Lap 23"),
                                                                    labels = c("Lap 23", "Lap 23")) +
                                                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                               
                                               ver_ad_2021_lap23$Brake <- ifelse(ver_ad_2021_lap23$Brake, 1, 0)
                                               ham_ad_2021_lap23$Brake <- ifelse(ham_ad_2021_lap23$Brake, 1, 0)
                                               
                                               p3 <- ggplot(data = ver_ad_2021_lap23, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 23")) +
                                                 geom_line() +
                                                 geom_line(data = ham_ad_2021_lap23, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 23")) +
                                                 labs(y = "Brake", x = "Distance(in meters)") + 
                                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                                    name = "Max Verstappen vs Lewis Hamilton",
                                                                    breaks = c("Max Verstappen Lap 23", "Lewis Hamilton Lap 23"),
                                                                    labels = c("Lap 23", "Lap 23")) +
                                                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                               
                                               p4 <- ggplot(data = ver_ad_2021_lap23, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 23")) +
                                                 geom_line() +
                                                 geom_line(data = ham_ad_2021_lap23, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 23")) +
                                                 labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                                    name = "Max Verstappen vs Lewis Hamilton",
                                                                    breaks = c("Max Verstappen Lap 23", "Lewis Hamilton Lap 23"),
                                                                    labels = c("Lap 23", "Lap 23")) +
                                                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                               
                                               p5 <- ggplot(data = ver_ad_2021_lap23, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 23")) +
                                                 geom_line() +
                                                 geom_line(data = ham_ad_2021_lap23, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 23")) +
                                                 labs(y = "RPM", x = "Distance(in meters)") + 
                                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                                    name = "Max Verstappen vs Lewis Hamilton",
                                                                    breaks = c("Max Verstappen Lap 23", "Lewis Hamilton Lap 23"),
                                                                    labels = c("Lap 23", "Lap 23")) +
                                                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                               
                                               grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                               
                                             }
                                             
                                             else
                                             {
                                               if (input$lap_select == 24) {
                                                 p1 <- ggplot(data = ver_ad_2021_lap24, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 24")) +
                                                   geom_line() +
                                                   geom_line(data = ham_ad_2021_lap24, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 24")) +
                                                   labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                                      name = "Max Verstappen Vs Lewis Hamilton",
                                                                      breaks = c("Max Verstappen Lap 24", "Lewis Hamilton Lap 24"),
                                                                      labels = c("Lap 24", "Lap 24"))
                                                 
                                                 p2 <- ggplot(data = ver_ad_2021_lap24, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 24")) +
                                                   geom_line() +
                                                   geom_line(data = ham_ad_2021_lap24, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 24")) +
                                                   labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                                      name = "Max Verstappen vs Lewis Hamilton",
                                                                      breaks = c("Max Verstappen Lap 24", "Lewis Hamilton Lap 24"),
                                                                      labels = c("Lap 24", "Lap 24")) +
                                                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                 
                                                 ver_ad_2021_lap24$Brake <- ifelse(ver_ad_2021_lap24$Brake, 1, 0)
                                                 ham_ad_2021_lap24$Brake <- ifelse(ham_ad_2021_lap24$Brake, 1, 0)
                                                 
                                                 p3 <- ggplot(data = ver_ad_2021_lap24, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 24")) +
                                                   geom_line() +
                                                   geom_line(data = ham_ad_2021_lap24, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 24")) +
                                                   labs(y = "Brake", x = "Distance(in meters)") + 
                                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                                      name = "Max Verstappen vs Lewis Hamilton",
                                                                      breaks = c("Max Verstappen Lap 24", "Lewis Hamilton Lap 24"),
                                                                      labels = c("Lap 24", "Lap 24")) +
                                                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                 
                                                 p4 <- ggplot(data = ver_ad_2021_lap24, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 24")) +
                                                   geom_line() +
                                                   geom_line(data = ham_ad_2021_lap24, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 24")) +
                                                   labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                                      name = "Max Verstappen vs Lewis Hamilton",
                                                                      breaks = c("Max Verstappen Lap 24", "Lewis Hamilton Lap 24"),
                                                                      labels = c("Lap 24", "Lap 24")) +
                                                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                 
                                                 p5 <- ggplot(data = ver_ad_2021_lap24, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 24")) +
                                                   geom_line() +
                                                   geom_line(data = ham_ad_2021_lap24, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 24")) +
                                                   labs(y = "RPM", x = "Distance(in meters)") + 
                                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                                      name = "Max Verstappen vs Lewis Hamilton",
                                                                      breaks = c("Max Verstappen Lap 24", "Lewis Hamilton Lap 24"),
                                                                      labels = c("Lap 24", "Lap 24")) +
                                                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                 
                                                 grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                 
                                               }
                                               
                                               else
                                               {
                                                 if (input$lap_select == 25) {
                                                   p1 <- ggplot(data = ver_ad_2021_lap25, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 25")) +
                                                     geom_line() +
                                                     geom_line(data = ham_ad_2021_lap25, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 25")) +
                                                     labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                                        name = "Max Verstappen Vs Lewis Hamilton",
                                                                        breaks = c("Max Verstappen Lap 25", "Lewis Hamilton Lap 25"),
                                                                        labels = c("Lap 25", "Lap 25"))
                                                   
                                                   p2 <- ggplot(data = ver_ad_2021_lap25, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 25")) +
                                                     geom_line() +
                                                     geom_line(data = ham_ad_2021_lap25, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 25")) +
                                                     labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                                        name = "Max Verstappen vs Lewis Hamilton",
                                                                        breaks = c("Max Verstappen Lap 25", "Lewis Hamilton Lap 25"),
                                                                        labels = c("Lap 25", "Lap 25")) +
                                                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                   
                                                   ver_ad_2021_lap25$Brake <- ifelse(ver_ad_2021_lap25$Brake, 1, 0)
                                                   ham_ad_2021_lap25$Brake <- ifelse(ham_ad_2021_lap25$Brake, 1, 0)
                                                   
                                                   p3 <- ggplot(data = ver_ad_2021_lap25, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 25")) +
                                                     geom_line() +
                                                     geom_line(data = ham_ad_2021_lap25, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 25")) +
                                                     labs(y = "Brake", x = "Distance(in meters)") + 
                                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                                        name = "Max Verstappen vs Lewis Hamilton",
                                                                        breaks = c("Max Verstappen Lap 25", "Lewis Hamilton Lap 25"),
                                                                        labels = c("Lap 25", "Lap 25")) +
                                                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                   
                                                   p4 <- ggplot(data = ver_ad_2021_lap25, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 25")) +
                                                     geom_line() +
                                                     geom_line(data = ham_ad_2021_lap25, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 25")) +
                                                     labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                                        name = "Max Verstappen vs Lewis Hamilton",
                                                                        breaks = c("Max Verstappen Lap 25", "Lewis Hamilton Lap 25"),
                                                                        labels = c("Lap 25", "Lap 25")) +
                                                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                   
                                                   p5 <- ggplot(data = ver_ad_2021_lap25, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 25")) +
                                                     geom_line() +
                                                     geom_line(data = ham_ad_2021_lap25, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 25")) +
                                                     labs(y = "RPM", x = "Distance(in meters)") + 
                                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                                        name = "Max Verstappen vs Lewis Hamilton",
                                                                        breaks = c("Max Verstappen Lap 25", "Lewis Hamilton Lap 25"),
                                                                        labels = c("Lap 25", "Lap 25")) +
                                                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                   
                                                   grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                   
                                                 }
     
                                                 else
                                                 {
                                                   if (input$lap_select == 26) {
                                                     p1 <- ggplot(data = ver_ad_2021_lap26, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 26")) +
                                                       geom_line() +
                                                       geom_line(data = ham_ad_2021_lap26, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 26")) +
                                                       labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                                          name = "Max Verstappen Vs Lewis Hamilton",
                                                                          breaks = c("Max Verstappen Lap 26", "Lewis Hamilton Lap 26"),
                                                                          labels = c("Lap 26", "Lap 26"))
                                                     
                                                     p2 <- ggplot(data = ver_ad_2021_lap26, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 26")) +
                                                       geom_line() +
                                                       geom_line(data = ham_ad_2021_lap26, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 26")) +
                                                       labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                                          name = "Max Verstappen vs Lewis Hamilton",
                                                                          breaks = c("Max Verstappen Lap 26", "Lewis Hamilton Lap 26"),
                                                                          labels = c("Lap 26", "Lap 26")) +
                                                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                     
                                                     ver_ad_2021_lap26$Brake <- ifelse(ver_ad_2021_lap26$Brake, 1, 0)
                                                     ham_ad_2021_lap26$Brake <- ifelse(ham_ad_2021_lap26$Brake, 1, 0)
                                                     
                                                     p3 <- ggplot(data = ver_ad_2021_lap26, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 26")) +
                                                       geom_line() +
                                                       geom_line(data = ham_ad_2021_lap26, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 26")) +
                                                       labs(y = "Brake", x = "Distance(in meters)") + 
                                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                                          name = "Max Verstappen vs Lewis Hamilton",
                                                                          breaks = c("Max Verstappen Lap 26", "Lewis Hamilton Lap 26"),
                                                                          labels = c("Lap 26", "Lap 26")) +
                                                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                     
                                                     p4 <- ggplot(data = ver_ad_2021_lap26, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 26")) +
                                                       geom_line() +
                                                       geom_line(data = ham_ad_2021_lap26, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 26")) +
                                                       labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                                          name = "Max Verstappen vs Lewis Hamilton",
                                                                          breaks = c("Max Verstappen Lap 26", "Lewis Hamilton Lap 26"),
                                                                          labels = c("Lap 26", "Lap 26")) +
                                                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                     
                                                     p5 <- ggplot(data = ver_ad_2021_lap26, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 26")) +
                                                       geom_line() +
                                                       geom_line(data = ham_ad_2021_lap26, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 26")) +
                                                       labs(y = "RPM", x = "Distance(in meters)") + 
                                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                                          name = "Max Verstappen vs Lewis Hamilton",
                                                                          breaks = c("Max Verstappen Lap 26", "Lewis Hamilton Lap 26"),
                                                                          labels = c("Lap 26", "Lap 26")) +
                                                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                     
                                                     grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                     
                                                   }
                                                   
                                                   else
                                                   {
                                                     if (input$lap_select == 27) {
                                                       p1 <- ggplot(data = ver_ad_2021_lap27, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 27")) +
                                                         geom_line() +
                                                         geom_line(data = ham_ad_2021_lap27, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 27")) +
                                                         labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                                            name = "Max Verstappen Vs Lewis Hamilton",
                                                                            breaks = c("Max Verstappen Lap 27", "Lewis Hamilton Lap 27"),
                                                                            labels = c("Lap 27", "Lap 27"))
                                                       
                                                       p2 <- ggplot(data = ver_ad_2021_lap27, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 27")) +
                                                         geom_line() +
                                                         geom_line(data = ham_ad_2021_lap27, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 27")) +
                                                         labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                                            name = "Max Verstappen vs Lewis Hamilton",
                                                                            breaks = c("Max Verstappen Lap 27", "Lewis Hamilton Lap 27"),
                                                                            labels = c("Lap 27", "Lap 27")) +
                                                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                       
                                                       ver_ad_2021_lap27$Brake <- ifelse(ver_ad_2021_lap27$Brake, 1, 0)
                                                       ham_ad_2021_lap27$Brake <- ifelse(ham_ad_2021_lap27$Brake, 1, 0)
                                                       
                                                       p3 <- ggplot(data = ver_ad_2021_lap27, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 27")) +
                                                         geom_line() +
                                                         geom_line(data = ham_ad_2021_lap27, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 27")) +
                                                         labs(y = "Brake", x = "Distance(in meters)") + 
                                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                                            name = "Max Verstappen vs Lewis Hamilton",
                                                                            breaks = c("Max Verstappen Lap 27", "Lewis Hamilton Lap 27"),
                                                                            labels = c("Lap 27", "Lap 27")) +
                                                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                       
                                                       p4 <- ggplot(data = ver_ad_2021_lap27, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 27")) +
                                                         geom_line() +
                                                         geom_line(data = ham_ad_2021_lap27, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 27")) +
                                                         labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                                            name = "Max Verstappen vs Lewis Hamilton",
                                                                            breaks = c("Max Verstappen Lap 27", "Lewis Hamilton Lap 27"),
                                                                            labels = c("Lap 27", "Lap 27")) +
                                                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                       
                                                       p5 <- ggplot(data = ver_ad_2021_lap27, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 27")) +
                                                         geom_line() +
                                                         geom_line(data = ham_ad_2021_lap27, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 27")) +
                                                         labs(y = "RPM", x = "Distance(in meters)") + 
                                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                                            name = "Max Verstappen vs Lewis Hamilton",
                                                                            breaks = c("Max Verstappen Lap 27", "Lewis Hamilton Lap 27"),
                                                                            labels = c("Lap 27", "Lap 27")) +
                                                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                       
                                                       grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                       
                                                     }
                                                     
                                                     else
                                                     {
                                                       if (input$lap_select == 28) {
                                                         p1 <- ggplot(data = ver_ad_2021_lap28, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 28")) +
                                                           geom_line() +
                                                           geom_line(data = ham_ad_2021_lap28, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 28")) +
                                                           labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                           scale_color_manual(values = c("darkred", "steelblue"), 
                                                                              name = "Max Verstappen Vs Lewis Hamilton",
                                                                              breaks = c("Max Verstappen Lap 28", "Lewis Hamilton Lap 28"),
                                                                              labels = c("Lap 28", "Lap 28"))
                                                         
                                                         p2 <- ggplot(data = ver_ad_2021_lap28, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 28")) +
                                                           geom_line() +
                                                           geom_line(data = ham_ad_2021_lap28, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 28")) +
                                                           labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                           scale_color_manual(values = c("darkred", "steelblue"), 
                                                                              name = "Max Verstappen vs Lewis Hamilton",
                                                                              breaks = c("Max Verstappen Lap 28", "Lewis Hamilton Lap 28"),
                                                                              labels = c("Lap 28", "Lap 28")) +
                                                           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                         
                                                         ver_ad_2021_lap28$Brake <- ifelse(ver_ad_2021_lap28$Brake, 1, 0)
                                                         ham_ad_2021_lap28$Brake <- ifelse(ham_ad_2021_lap28$Brake, 1, 0)
                                                         
                                                         p3 <- ggplot(data = ver_ad_2021_lap28, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 28")) +
                                                           geom_line() +
                                                           geom_line(data = ham_ad_2021_lap28, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 28")) +
                                                           labs(y = "Brake", x = "Distance(in meters)") + 
                                                           scale_color_manual(values = c("darkred", "steelblue"), 
                                                                              name = "Max Verstappen vs Lewis Hamilton",
                                                                              breaks = c("Max Verstappen Lap 28", "Lewis Hamilton Lap 28"),
                                                                              labels = c("Lap 28", "Lap 28")) +
                                                           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                         
                                                         p4 <- ggplot(data = ver_ad_2021_lap28, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 28")) +
                                                           geom_line() +
                                                           geom_line(data = ham_ad_2021_lap28, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 28")) +
                                                           labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                           scale_color_manual(values = c("darkred", "steelblue"), 
                                                                              name = "Max Verstappen vs Lewis Hamilton",
                                                                              breaks = c("Max Verstappen Lap 28", "Lewis Hamilton Lap 28"),
                                                                              labels = c("Lap 28", "Lap 28")) +
                                                           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                         
                                                         p5 <- ggplot(data = ver_ad_2021_lap28, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 28")) +
                                                           geom_line() +
                                                           geom_line(data = ham_ad_2021_lap28, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 28")) +
                                                           labs(y = "RPM", x = "Distance(in meters)") + 
                                                           scale_color_manual(values = c("darkred", "steelblue"), 
                                                                              name = "Max Verstappen vs Lewis Hamilton",
                                                                              breaks = c("Max Verstappen Lap 28", "Lewis Hamilton Lap 28"),
                                                                              labels = c("Lap 28", "Lap 28")) +
                                                           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                         
                                                         grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                         
                                                       }
                                                       
                                                       else
                                                       {
                                                         if (input$lap_select == 29) {
                                                           p1 <- ggplot(data = ver_ad_2021_lap29, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 29")) +
                                                             geom_line() +
                                                             geom_line(data = ham_ad_2021_lap29, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 29")) +
                                                             labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                name = "Max Verstappen Vs Lewis Hamilton",
                                                                                breaks = c("Max Verstappen Lap 29", "Lewis Hamilton Lap 29"),
                                                                                labels = c("Lap 29", "Lap 29"))
                                                           
                                                           p2 <- ggplot(data = ver_ad_2021_lap29, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 29")) +
                                                             geom_line() +
                                                             geom_line(data = ham_ad_2021_lap29, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 29")) +
                                                             labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                name = "Max Verstappen vs Lewis Hamilton",
                                                                                breaks = c("Max Verstappen Lap 29", "Lewis Hamilton Lap 29"),
                                                                                labels = c("Lap 29", "Lap 29")) +
                                                             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                           
                                                           ver_ad_2021_lap29$Brake <- ifelse(ver_ad_2021_lap29$Brake, 1, 0)
                                                           ham_ad_2021_lap29$Brake <- ifelse(ham_ad_2021_lap29$Brake, 1, 0)
                                                           
                                                           p3 <- ggplot(data = ver_ad_2021_lap29, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 29")) +
                                                             geom_line() +
                                                             geom_line(data = ham_ad_2021_lap29, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 29")) +
                                                             labs(y = "Brake", x = "Distance(in meters)") + 
                                                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                name = "Max Verstappen vs Lewis Hamilton",
                                                                                breaks = c("Max Verstappen Lap 29", "Lewis Hamilton Lap 29"),
                                                                                labels = c("Lap 29", "Lap 29")) +
                                                             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                           
                                                           p4 <- ggplot(data = ver_ad_2021_lap29, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 29")) +
                                                             geom_line() +
                                                             geom_line(data = ham_ad_2021_lap29, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 29")) +
                                                             labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                name = "Max Verstappen vs Lewis Hamilton",
                                                                                breaks = c("Max Verstappen Lap 29", "Lewis Hamilton Lap 29"),
                                                                                labels = c("Lap 29", "Lap 29")) +
                                                             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                           
                                                           p5 <- ggplot(data = ver_ad_2021_lap29, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 29")) +
                                                             geom_line() +
                                                             geom_line(data = ham_ad_2021_lap29, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 29")) +
                                                             labs(y = "RPM", x = "Distance(in meters)") + 
                                                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                name = "Max Verstappen vs Lewis Hamilton",
                                                                                breaks = c("Max Verstappen Lap 29", "Lewis Hamilton Lap 29"),
                                                                                labels = c("Lap 29", "Lap 29")) +
                                                             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                           
                                                           grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                           
                                                         }
                                                         
                                                         else
                                                         {
                                                           if (input$lap_select == 30) {
                                                             p1 <- ggplot(data = ver_ad_2021_lap30, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 30")) +
                                                               geom_line() +
                                                               geom_line(data = ham_ad_2021_lap30, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 30")) +
                                                               labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                  name = "Max Verstappen Vs Lewis Hamilton",
                                                                                  breaks = c("Max Verstappen Lap 30", "Lewis Hamilton Lap 30"),
                                                                                  labels = c("Lap 30", "Lap 30"))
                                                             
                                                             p2 <- ggplot(data = ver_ad_2021_lap30, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 30")) +
                                                               geom_line() +
                                                               geom_line(data = ham_ad_2021_lap30, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 30")) +
                                                               labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                  name = "Max Verstappen vs Lewis Hamilton",
                                                                                  breaks = c("Max Verstappen Lap 30", "Lewis Hamilton Lap 30"),
                                                                                  labels = c("Lap 30", "Lap 30")) +
                                                               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                             
                                                             ver_ad_2021_lap30$Brake <- ifelse(ver_ad_2021_lap30$Brake, 1, 0)
                                                             ham_ad_2021_lap30$Brake <- ifelse(ham_ad_2021_lap30$Brake, 1, 0)
                                                             
                                                             p3 <- ggplot(data = ver_ad_2021_lap30, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 30")) +
                                                               geom_line() +
                                                               geom_line(data = ham_ad_2021_lap30, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 30")) +
                                                               labs(y = "Brake", x = "Distance(in meters)") + 
                                                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                  name = "Max Verstappen vs Lewis Hamilton",
                                                                                  breaks = c("Max Verstappen Lap 30", "Lewis Hamilton Lap 30"),
                                                                                  labels = c("Lap 30", "Lap 30")) +
                                                               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                             
                                                             p4 <- ggplot(data = ver_ad_2021_lap30, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 30")) +
                                                               geom_line() +
                                                               geom_line(data = ham_ad_2021_lap30, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 30")) +
                                                               labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                  name = "Max Verstappen vs Lewis Hamilton",
                                                                                  breaks = c("Max Verstappen Lap 30", "Lewis Hamilton Lap 30"),
                                                                                  labels = c("Lap 30", "Lap 30")) +
                                                               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                             
                                                             p5 <- ggplot(data = ver_ad_2021_lap30, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 30")) +
                                                               geom_line() +
                                                               geom_line(data = ham_ad_2021_lap30, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 30")) +
                                                               labs(y = "RPM", x = "Distance(in meters)") + 
                                                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                  name = "Max Verstappen vs Lewis Hamilton",
                                                                                  breaks = c("Max Verstappen Lap 30", "Lewis Hamilton Lap 30"),
                                                                                  labels = c("Lap 30", "Lap 30")) +
                                                               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                             
                                                             grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                             
                                                           }
                                                           else
                                                           {
                                                             if (input$lap_select == 31) {
                                                               p1 <- ggplot(data = ver_ad_2021_lap31, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 31")) +
                                                                 geom_line() +
                                                                 geom_line(data = ham_ad_2021_lap31, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 31")) +
                                                                 labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                    name = "Max Verstappen Vs Lewis Hamilton",
                                                                                    breaks = c("Max Verstappen Lap 31", "Lewis Hamilton Lap 31"),
                                                                                    labels = c("Lap 31", "Lap 31"))
                                                               
                                                               p2 <- ggplot(data = ver_ad_2021_lap31, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 31")) +
                                                                 geom_line() +
                                                                 geom_line(data = ham_ad_2021_lap31, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 31")) +
                                                                 labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                    name = "Max Verstappen vs Lewis Hamilton",
                                                                                    breaks = c("Max Verstappen Lap 31", "Lewis Hamilton Lap 31"),
                                                                                    labels = c("Lap 31", "Lap 31")) +
                                                                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                               
                                                               ver_ad_2021_lap31$Brake <- ifelse(ver_ad_2021_lap31$Brake, 1, 0)
                                                               ham_ad_2021_lap31$Brake <- ifelse(ham_ad_2021_lap31$Brake, 1, 0)
                                                               
                                                               p3 <- ggplot(data = ver_ad_2021_lap31, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 31")) +
                                                                 geom_line() +
                                                                 geom_line(data = ham_ad_2021_lap31, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 31")) +
                                                                 labs(y = "Brake", x = "Distance(in meters)") + 
                                                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                    name = "Max Verstappen vs Lewis Hamilton",
                                                                                    breaks = c("Max Verstappen Lap 31", "Lewis Hamilton Lap 31"),
                                                                                    labels = c("Lap 31", "Lap 31")) +
                                                                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                               
                                                               p4 <- ggplot(data = ver_ad_2021_lap31, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 31")) +
                                                                 geom_line() +
                                                                 geom_line(data = ham_ad_2021_lap31, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 31")) +
                                                                 labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                    name = "Max Verstappen vs Lewis Hamilton",
                                                                                    breaks = c("Max Verstappen Lap 31", "Lewis Hamilton Lap 31"),
                                                                                    labels = c("Lap 31", "Lap 31")) +
                                                                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                               
                                                               p5 <- ggplot(data = ver_ad_2021_lap31, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 31")) +
                                                                 geom_line() +
                                                                 geom_line(data = ham_ad_2021_lap31, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 31")) +
                                                                 labs(y = "RPM", x = "Distance(in meters)") + 
                                                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                    name = "Max Verstappen vs Lewis Hamilton",
                                                                                    breaks = c("Max Verstappen Lap 31", "Lewis Hamilton Lap 31"),
                                                                                    labels = c("Lap 31", "Lap 31")) +
                                                                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                               
                                                               grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                               
                                                             }
                                                             
                                                             else
                                                             {
                                                               if (input$lap_select == 32) {
                                                                 p1 <- ggplot(data = ver_ad_2021_lap32, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 32")) +
                                                                   geom_line() +
                                                                   geom_line(data = ham_ad_2021_lap32, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 32")) +
                                                                   labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                      name = "Max Verstappen Vs Lewis Hamilton",
                                                                                      breaks = c("Max Verstappen Lap 32", "Lewis Hamilton Lap 32"),
                                                                                      labels = c("Lap 32", "Lap 32"))
                                                                 
                                                                 p2 <- ggplot(data = ver_ad_2021_lap32, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 32")) +
                                                                   geom_line() +
                                                                   geom_line(data = ham_ad_2021_lap32, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 32")) +
                                                                   labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                      name = "Max Verstappen vs Lewis Hamilton",
                                                                                      breaks = c("Max Verstappen Lap 32", "Lewis Hamilton Lap 32"),
                                                                                      labels = c("Lap 32", "Lap 32")) +
                                                                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                 
                                                                 ver_ad_2021_lap32$Brake <- ifelse(ver_ad_2021_lap32$Brake, 1, 0)
                                                                 ham_ad_2021_lap32$Brake <- ifelse(ham_ad_2021_lap32$Brake, 1, 0)
                                                                 
                                                                 p3 <- ggplot(data = ver_ad_2021_lap32, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 32")) +
                                                                   geom_line() +
                                                                   geom_line(data = ham_ad_2021_lap32, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 32")) +
                                                                   labs(y = "Brake", x = "Distance(in meters)") + 
                                                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                      name = "Max Verstappen vs Lewis Hamilton",
                                                                                      breaks = c("Max Verstappen Lap 32", "Lewis Hamilton Lap 32"),
                                                                                      labels = c("Lap 32", "Lap 32")) +
                                                                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                 
                                                                 p4 <- ggplot(data = ver_ad_2021_lap32, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 32")) +
                                                                   geom_line() +
                                                                   geom_line(data = ham_ad_2021_lap32, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 32")) +
                                                                   labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                      name = "Max Verstappen vs Lewis Hamilton",
                                                                                      breaks = c("Max Verstappen Lap 32", "Lewis Hamilton Lap 32"),
                                                                                      labels = c("Lap 32", "Lap 32")) +
                                                                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                 
                                                                 p5 <- ggplot(data = ver_ad_2021_lap32, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 32")) +
                                                                   geom_line() +
                                                                   geom_line(data = ham_ad_2021_lap32, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 32")) +
                                                                   labs(y = "RPM", x = "Distance(in meters)") + 
                                                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                      name = "Max Verstappen vs Lewis Hamilton",
                                                                                      breaks = c("Max Verstappen Lap 32", "Lewis Hamilton Lap 32"),
                                                                                      labels = c("Lap 32", "Lap 32")) +
                                                                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                 
                                                                 grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                                 
                                                               }
                                                               else
                                                               {
                                                                 if (input$lap_select == 33) {
                                                                   p1 <- ggplot(data = ver_ad_2021_lap33, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 33")) +
                                                                     geom_line() +
                                                                     geom_line(data = ham_ad_2021_lap33, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 33")) +
                                                                     labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                        name = "Max Verstappen Vs Lewis Hamilton",
                                                                                        breaks = c("Max Verstappen Lap 33", "Lewis Hamilton Lap 33"),
                                                                                        labels = c("Lap 33", "Lap 33"))
                                                                   
                                                                   p2 <- ggplot(data = ver_ad_2021_lap33, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 33")) +
                                                                     geom_line() +
                                                                     geom_line(data = ham_ad_2021_lap33, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 33")) +
                                                                     labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                        name = "Max Verstappen vs Lewis Hamilton",
                                                                                        breaks = c("Max Verstappen Lap 33", "Lewis Hamilton Lap 33"),
                                                                                        labels = c("Lap 33", "Lap 33")) +
                                                                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                   
                                                                   ver_ad_2021_lap33$Brake <- ifelse(ver_ad_2021_lap33$Brake, 1, 0)
                                                                   ham_ad_2021_lap33$Brake <- ifelse(ham_ad_2021_lap33$Brake, 1, 0)
                                                                   
                                                                   p3 <- ggplot(data = ver_ad_2021_lap33, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 33")) +
                                                                     geom_line() +
                                                                     geom_line(data = ham_ad_2021_lap33, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 33")) +
                                                                     labs(y = "Brake", x = "Distance(in meters)") + 
                                                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                        name = "Max Verstappen vs Lewis Hamilton",
                                                                                        breaks = c("Max Verstappen Lap 33", "Lewis Hamilton Lap 33"),
                                                                                        labels = c("Lap 33", "Lap 33")) +
                                                                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                   
                                                                   p4 <- ggplot(data = ver_ad_2021_lap33, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 33")) +
                                                                     geom_line() +
                                                                     geom_line(data = ham_ad_2021_lap33, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 33")) +
                                                                     labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                        name = "Max Verstappen vs Lewis Hamilton",
                                                                                        breaks = c("Max Verstappen Lap 33", "Lewis Hamilton Lap 33"),
                                                                                        labels = c("Lap 33", "Lap 33")) +
                                                                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                   
                                                                   p5 <- ggplot(data = ver_ad_2021_lap33, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 33")) +
                                                                     geom_line() +
                                                                     geom_line(data = ham_ad_2021_lap33, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 33")) +
                                                                     labs(y = "RPM", x = "Distance(in meters)") + 
                                                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                        name = "Max Verstappen vs Lewis Hamilton",
                                                                                        breaks = c("Max Verstappen Lap 33", "Lewis Hamilton Lap 33"),
                                                                                        labels = c("Lap 33", "Lap 33")) +
                                                                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                   
                                                                   grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                                   
                                                                 }
                                                                 
                                                                 else
                                                                 {
                                                                   if (input$lap_select == 34) {
                                                                     p1 <- ggplot(data = ver_ad_2021_lap34, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 34")) +
                                                                       geom_line() +
                                                                       geom_line(data = ham_ad_2021_lap34, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 34")) +
                                                                       labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                          name = "Max Verstappen Vs Lewis Hamilton",
                                                                                          breaks = c("Max Verstappen Lap 34", "Lewis Hamilton Lap 34"),
                                                                                          labels = c("Lap 34", "Lap 34"))
                                                                     
                                                                     p2 <- ggplot(data = ver_ad_2021_lap34, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 34")) +
                                                                       geom_line() +
                                                                       geom_line(data = ham_ad_2021_lap34, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 34")) +
                                                                       labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                          name = "Max Verstappen vs Lewis Hamilton",
                                                                                          breaks = c("Max Verstappen Lap 34", "Lewis Hamilton Lap 34"),
                                                                                          labels = c("Lap 34", "Lap 34")) +
                                                                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                     
                                                                     ver_ad_2021_lap34$Brake <- ifelse(ver_ad_2021_lap34$Brake, 1, 0)
                                                                     ham_ad_2021_lap34$Brake <- ifelse(ham_ad_2021_lap34$Brake, 1, 0)
                                                                     
                                                                     p3 <- ggplot(data = ver_ad_2021_lap34, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 34")) +
                                                                       geom_line() +
                                                                       geom_line(data = ham_ad_2021_lap34, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 34")) +
                                                                       labs(y = "Brake", x = "Distance(in meters)") + 
                                                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                          name = "Max Verstappen vs Lewis Hamilton",
                                                                                          breaks = c("Max Verstappen Lap 34", "Lewis Hamilton Lap 34"),
                                                                                          labels = c("Lap 34", "Lap 34")) +
                                                                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                     
                                                                     p4 <- ggplot(data = ver_ad_2021_lap34, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 34")) +
                                                                       geom_line() +
                                                                       geom_line(data = ham_ad_2021_lap34, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 34")) +
                                                                       labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                          name = "Max Verstappen vs Lewis Hamilton",
                                                                                          breaks = c("Max Verstappen Lap 34", "Lewis Hamilton Lap 34"),
                                                                                          labels = c("Lap 34", "Lap 34")) +
                                                                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                     
                                                                     p5 <- ggplot(data = ver_ad_2021_lap34, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 34")) +
                                                                       geom_line() +
                                                                       geom_line(data = ham_ad_2021_lap34, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 34")) +
                                                                       labs(y = "RPM", x = "Distance(in meters)") + 
                                                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                          name = "Max Verstappen vs Lewis Hamilton",
                                                                                          breaks = c("Max Verstappen Lap 34", "Lewis Hamilton Lap 34"),
                                                                                          labels = c("Lap 34", "Lap 34")) +
                                                                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                     
                                                                     grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                                     
                                                                   }
                                                                   
                                                                   else
                                                                   {
                                                                     if (input$lap_select == 35) {
                                                                       p1 <- ggplot(data = ver_ad_2021_lap35, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 35")) +
                                                                         geom_line() +
                                                                         geom_line(data = ham_ad_2021_lap35, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 35")) +
                                                                         labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                            name = "Max Verstappen Vs Lewis Hamilton",
                                                                                            breaks = c("Max Verstappen Lap 35", "Lewis Hamilton Lap 35"),
                                                                                            labels = c("Lap 35", "Lap 35"))
                                                                       
                                                                       p2 <- ggplot(data = ver_ad_2021_lap35, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 35")) +
                                                                         geom_line() +
                                                                         geom_line(data = ham_ad_2021_lap35, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 35")) +
                                                                         labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                            name = "Max Verstappen vs Lewis Hamilton",
                                                                                            breaks = c("Max Verstappen Lap 35", "Lewis Hamilton Lap 35"),
                                                                                            labels = c("Lap 35", "Lap 35")) +
                                                                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                       
                                                                       ver_ad_2021_lap35$Brake <- ifelse(ver_ad_2021_lap35$Brake, 1, 0)
                                                                       ham_ad_2021_lap35$Brake <- ifelse(ham_ad_2021_lap35$Brake, 1, 0)
                                                                       
                                                                       p3 <- ggplot(data = ver_ad_2021_lap35, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 35")) +
                                                                         geom_line() +
                                                                         geom_line(data = ham_ad_2021_lap35, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 35")) +
                                                                         labs(y = "Brake", x = "Distance(in meters)") + 
                                                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                            name = "Max Verstappen vs Lewis Hamilton",
                                                                                            breaks = c("Max Verstappen Lap 35", "Lewis Hamilton Lap 35"),
                                                                                            labels = c("Lap 35", "Lap 35")) +
                                                                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                       
                                                                       p4 <- ggplot(data = ver_ad_2021_lap35, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 35")) +
                                                                         geom_line() +
                                                                         geom_line(data = ham_ad_2021_lap35, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 35")) +
                                                                         labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                            name = "Max Verstappen vs Lewis Hamilton",
                                                                                            breaks = c("Max Verstappen Lap 35", "Lewis Hamilton Lap 35"),
                                                                                            labels = c("Lap 35", "Lap 35")) +
                                                                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                       
                                                                       p5 <- ggplot(data = ver_ad_2021_lap35, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 35")) +
                                                                         geom_line() +
                                                                         geom_line(data = ham_ad_2021_lap35, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 35")) +
                                                                         labs(y = "RPM", x = "Distance(in meters)") + 
                                                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                            name = "Max Verstappen vs Lewis Hamilton",
                                                                                            breaks = c("Max Verstappen Lap 35", "Lewis Hamilton Lap 35"),
                                                                                            labels = c("Lap 35", "Lap 35")) +
                                                                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                       
                                                                       grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                                       
                                                                     }
                                                                     
                                                                     else
                                                                     {
                                                                       if (input$lap_select == 36) {
                                                                         p1 <- ggplot(data = ver_ad_2021_lap36, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 36")) +
                                                                           geom_line() +
                                                                           geom_line(data = ham_ad_2021_lap36, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 36")) +
                                                                           labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                                           scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                              name = "Max Verstappen Vs Lewis Hamilton",
                                                                                              breaks = c("Max Verstappen Lap 36", "Lewis Hamilton Lap 36"),
                                                                                              labels = c("Lap 36", "Lap 36"))
                                                                         
                                                                         p2 <- ggplot(data = ver_ad_2021_lap36, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 36")) +
                                                                           geom_line() +
                                                                           geom_line(data = ham_ad_2021_lap36, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 36")) +
                                                                           labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                                           scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                              name = "Max Verstappen vs Lewis Hamilton",
                                                                                              breaks = c("Max Verstappen Lap 36", "Lewis Hamilton Lap 36"),
                                                                                              labels = c("Lap 36", "Lap 36")) +
                                                                           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                         
                                                                         ver_ad_2021_lap36$Brake <- ifelse(ver_ad_2021_lap36$Brake, 1, 0)
                                                                         ham_ad_2021_lap36$Brake <- ifelse(ham_ad_2021_lap36$Brake, 1, 0)
                                                                         
                                                                         p3 <- ggplot(data = ver_ad_2021_lap36, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 36")) +
                                                                           geom_line() +
                                                                           geom_line(data = ham_ad_2021_lap36, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 36")) +
                                                                           labs(y = "Brake", x = "Distance(in meters)") + 
                                                                           scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                              name = "Max Verstappen vs Lewis Hamilton",
                                                                                              breaks = c("Max Verstappen Lap 36", "Lewis Hamilton Lap 36"),
                                                                                              labels = c("Lap 36", "Lap 36")) +
                                                                           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                         
                                                                         p4 <- ggplot(data = ver_ad_2021_lap36, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 36")) +
                                                                           geom_line() +
                                                                           geom_line(data = ham_ad_2021_lap36, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 36")) +
                                                                           labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                                           scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                              name = "Max Verstappen vs Lewis Hamilton",
                                                                                              breaks = c("Max Verstappen Lap 36", "Lewis Hamilton Lap 36"),
                                                                                              labels = c("Lap 36", "Lap 36")) +
                                                                           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                         
                                                                         p5 <- ggplot(data = ver_ad_2021_lap36, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 36")) +
                                                                           geom_line() +
                                                                           geom_line(data = ham_ad_2021_lap36, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 36")) +
                                                                           labs(y = "RPM", x = "Distance(in meters)") + 
                                                                           scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                              name = "Max Verstappen vs Lewis Hamilton",
                                                                                              breaks = c("Max Verstappen Lap 36", "Lewis Hamilton Lap 36"),
                                                                                              labels = c("Lap 36", "Lap 36")) +
                                                                           guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                         
                                                                         grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                                         
                                                                       }
                                                                       
                                                                       else
                                                                       {
                                                                         if (input$lap_select == 37) {
                                                                           p1 <- ggplot(data = ver_ad_2021_lap37, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 37")) +
                                                                             geom_line() +
                                                                             geom_line(data = ham_ad_2021_lap37, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 37")) +
                                                                             labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                name = "Max Verstappen Vs Lewis Hamilton",
                                                                                                breaks = c("Max Verstappen Lap 37", "Lewis Hamilton Lap 37"),
                                                                                                labels = c("Lap 37", "Lap 37"))
                                                                           
                                                                           p2 <- ggplot(data = ver_ad_2021_lap37, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 37")) +
                                                                             geom_line() +
                                                                             geom_line(data = ham_ad_2021_lap37, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 37")) +
                                                                             labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                name = "Max Verstappen vs Lewis Hamilton",
                                                                                                breaks = c("Max Verstappen Lap 37", "Lewis Hamilton Lap 37"),
                                                                                                labels = c("Lap 37", "Lap 37")) +
                                                                             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                           
                                                                           ver_ad_2021_lap37$Brake <- ifelse(ver_ad_2021_lap37$Brake, 1, 0)
                                                                           ham_ad_2021_lap37$Brake <- ifelse(ham_ad_2021_lap37$Brake, 1, 0)
                                                                           
                                                                           p3 <- ggplot(data = ver_ad_2021_lap37, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 37")) +
                                                                             geom_line() +
                                                                             geom_line(data = ham_ad_2021_lap37, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 37")) +
                                                                             labs(y = "Brake", x = "Distance(in meters)") + 
                                                                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                name = "Max Verstappen vs Lewis Hamilton",
                                                                                                breaks = c("Max Verstappen Lap 37", "Lewis Hamilton Lap 37"),
                                                                                                labels = c("Lap 37", "Lap 37")) +
                                                                             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                           
                                                                           p4 <- ggplot(data = ver_ad_2021_lap37, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 37")) +
                                                                             geom_line() +
                                                                             geom_line(data = ham_ad_2021_lap37, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 37")) +
                                                                             labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                name = "Max Verstappen vs Lewis Hamilton",
                                                                                                breaks = c("Max Verstappen Lap 37", "Lewis Hamilton Lap 37"),
                                                                                                labels = c("Lap 37", "Lap 37")) +
                                                                             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                           
                                                                           p5 <- ggplot(data = ver_ad_2021_lap37, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 37")) +
                                                                             geom_line() +
                                                                             geom_line(data = ham_ad_2021_lap37, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 37")) +
                                                                             labs(y = "RPM", x = "Distance(in meters)") + 
                                                                             scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                name = "Max Verstappen vs Lewis Hamilton",
                                                                                                breaks = c("Max Verstappen Lap 37", "Lewis Hamilton Lap 37"),
                                                                                                labels = c("Lap 37", "Lap 37")) +
                                                                             guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                           
                                                                           grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                                           
                                                                         }
                                                                         
                                                                         else
                                                                         {
                                                                           if (input$lap_select == 38) {
                                                                             p1 <- ggplot(data = ver_ad_2021_lap38, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 38")) +
                                                                               geom_line() +
                                                                               geom_line(data = ham_ad_2021_lap38, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 38")) +
                                                                               labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                  name = "Max Verstappen Vs Lewis Hamilton",
                                                                                                  breaks = c("Max Verstappen Lap 38", "Lewis Hamilton Lap 38"),
                                                                                                  labels = c("Lap 38", "Lap 38"))
                                                                             
                                                                             p2 <- ggplot(data = ver_ad_2021_lap38, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 38")) +
                                                                               geom_line() +
                                                                               geom_line(data = ham_ad_2021_lap38, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 38")) +
                                                                               labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                  name = "Max Verstappen vs Lewis Hamilton",
                                                                                                  breaks = c("Max Verstappen Lap 38", "Lewis Hamilton Lap 38"),
                                                                                                  labels = c("Lap 38", "Lap 38")) +
                                                                               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                             
                                                                             ver_ad_2021_lap38$Brake <- ifelse(ver_ad_2021_lap38$Brake, 1, 0)
                                                                             ham_ad_2021_lap38$Brake <- ifelse(ham_ad_2021_lap38$Brake, 1, 0)
                                                                             
                                                                             p3 <- ggplot(data = ver_ad_2021_lap38, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 38")) +
                                                                               geom_line() +
                                                                               geom_line(data = ham_ad_2021_lap38, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 38")) +
                                                                               labs(y = "Brake", x = "Distance(in meters)") + 
                                                                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                  name = "Max Verstappen vs Lewis Hamilton",
                                                                                                  breaks = c("Max Verstappen Lap 38", "Lewis Hamilton Lap 38"),
                                                                                                  labels = c("Lap 38", "Lap 38")) +
                                                                               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                             
                                                                             p4 <- ggplot(data = ver_ad_2021_lap38, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 38")) +
                                                                               geom_line() +
                                                                               geom_line(data = ham_ad_2021_lap38, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 38")) +
                                                                               labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                  name = "Max Verstappen vs Lewis Hamilton",
                                                                                                  breaks = c("Max Verstappen Lap 38", "Lewis Hamilton Lap 38"),
                                                                                                  labels = c("Lap 38", "Lap 38")) +
                                                                               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                             
                                                                             p5 <- ggplot(data = ver_ad_2021_lap38, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 38")) +
                                                                               geom_line() +
                                                                               geom_line(data = ham_ad_2021_lap38, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 38")) +
                                                                               labs(y = "RPM", x = "Distance(in meters)") + 
                                                                               scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                  name = "Max Verstappen vs Lewis Hamilton",
                                                                                                  breaks = c("Max Verstappen Lap 38", "Lewis Hamilton Lap 38"),
                                                                                                  labels = c("Lap 38", "Lap 38")) +
                                                                               guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                             
                                                                             grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                                             
                                                                           }
                                                                           
                                                                           else
                                                                           {
                                                                             if (input$lap_select == 39) {
                                                                               p1 <- ggplot(data = ver_ad_2021_lap39, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 39")) +
                                                                                 geom_line() +
                                                                                 geom_line(data = ham_ad_2021_lap39, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 39")) +
                                                                                 labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                    name = "Max Verstappen Vs Lewis Hamilton",
                                                                                                    breaks = c("Max Verstappen Lap 39", "Lewis Hamilton Lap 39"),
                                                                                                    labels = c("Lap 39", "Lap 39"))
                                                                               
                                                                               p2 <- ggplot(data = ver_ad_2021_lap39, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 39")) +
                                                                                 geom_line() +
                                                                                 geom_line(data = ham_ad_2021_lap39, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 39")) +
                                                                                 labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                    name = "Max Verstappen vs Lewis Hamilton",
                                                                                                    breaks = c("Max Verstappen Lap 39", "Lewis Hamilton Lap 39"),
                                                                                                    labels = c("Lap 39", "Lap 39")) +
                                                                                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                               
                                                                               ver_ad_2021_lap39$Brake <- ifelse(ver_ad_2021_lap39$Brake, 1, 0)
                                                                               ham_ad_2021_lap39$Brake <- ifelse(ham_ad_2021_lap39$Brake, 1, 0)
                                                                               
                                                                               p3 <- ggplot(data = ver_ad_2021_lap39, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 39")) +
                                                                                 geom_line() +
                                                                                 geom_line(data = ham_ad_2021_lap39, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 39")) +
                                                                                 labs(y = "Brake", x = "Distance(in meters)") + 
                                                                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                    name = "Max Verstappen vs Lewis Hamilton",
                                                                                                    breaks = c("Max Verstappen Lap 39", "Lewis Hamilton Lap 39"),
                                                                                                    labels = c("Lap 39", "Lap 39")) +
                                                                                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                               
                                                                               p4 <- ggplot(data = ver_ad_2021_lap39, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 39")) +
                                                                                 geom_line() +
                                                                                 geom_line(data = ham_ad_2021_lap39, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 39")) +
                                                                                 labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                    name = "Max Verstappen vs Lewis Hamilton",
                                                                                                    breaks = c("Max Verstappen Lap 39", "Lewis Hamilton Lap 39"),
                                                                                                    labels = c("Lap 39", "Lap 39")) +
                                                                                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                               
                                                                               p5 <- ggplot(data = ver_ad_2021_lap39, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 39")) +
                                                                                 geom_line() +
                                                                                 geom_line(data = ham_ad_2021_lap39, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 39")) +
                                                                                 labs(y = "RPM", x = "Distance(in meters)") + 
                                                                                 scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                    name = "Max Verstappen vs Lewis Hamilton",
                                                                                                    breaks = c("Max Verstappen Lap 39", "Lewis Hamilton Lap 39"),
                                                                                                    labels = c("Lap 39", "Lap 39")) +
                                                                                 guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                               
                                                                               grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                                               
                                                                             }
                                                                             
                                                                             else
                                                                             {
                                                                               if (input$lap_select == 40) {
                                                                                 p1 <- ggplot(data = ver_ad_2021_lap40, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 40")) +
                                                                                   geom_line() +
                                                                                   geom_line(data = ham_ad_2021_lap40, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 40")) +
                                                                                   labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                      name = "Max Verstappen Vs Lewis Hamilton",
                                                                                                      breaks = c("Max Verstappen Lap 40", "Lewis Hamilton Lap 40"),
                                                                                                      labels = c("Lap 40", "Lap 40"))
                                                                                 
                                                                                 p2 <- ggplot(data = ver_ad_2021_lap40, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 40")) +
                                                                                   geom_line() +
                                                                                   geom_line(data = ham_ad_2021_lap40, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 40")) +
                                                                                   labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                      name = "Max Verstappen vs Lewis Hamilton",
                                                                                                      breaks = c("Max Verstappen Lap 40", "Lewis Hamilton Lap 40"),
                                                                                                      labels = c("Lap 40", "Lap 40")) +
                                                                                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                                 
                                                                                 ver_ad_2021_lap40$Brake <- ifelse(ver_ad_2021_lap40$Brake, 1, 0)
                                                                                 ham_ad_2021_lap40$Brake <- ifelse(ham_ad_2021_lap40$Brake, 1, 0)
                                                                                 
                                                                                 p3 <- ggplot(data = ver_ad_2021_lap40, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 40")) +
                                                                                   geom_line() +
                                                                                   geom_line(data = ham_ad_2021_lap40, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 40")) +
                                                                                   labs(y = "Brake", x = "Distance(in meters)") + 
                                                                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                      name = "Max Verstappen vs Lewis Hamilton",
                                                                                                      breaks = c("Max Verstappen Lap 40", "Lewis Hamilton Lap 40"),
                                                                                                      labels = c("Lap 40", "Lap 40")) +
                                                                                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                                 
                                                                                 p4 <- ggplot(data = ver_ad_2021_lap40, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 40")) +
                                                                                   geom_line() +
                                                                                   geom_line(data = ham_ad_2021_lap40, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 40")) +
                                                                                   labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                      name = "Max Verstappen vs Lewis Hamilton",
                                                                                                      breaks = c("Max Verstappen Lap 40", "Lewis Hamilton Lap 40"),
                                                                                                      labels = c("Lap 40", "Lap 40")) +
                                                                                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                                 
                                                                                 p5 <- ggplot(data = ver_ad_2021_lap40, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 40")) +
                                                                                   geom_line() +
                                                                                   geom_line(data = ham_ad_2021_lap40, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 40")) +
                                                                                   labs(y = "RPM", x = "Distance(in meters)") + 
                                                                                   scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                      name = "Max Verstappen vs Lewis Hamilton",
                                                                                                      breaks = c("Max Verstappen Lap 40", "Lewis Hamilton Lap 40"),
                                                                                                      labels = c("Lap 40", "Lap 40")) +
                                                                                   guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                                 
                                                                                 grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                                                 
                                                                               }
                                                                               
                                                                               else
                                                                               {
                                                                                 if (input$lap_select == 41) {
                                                                                   p1 <- ggplot(data = ver_ad_2021_lap41, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 41")) +
                                                                                     geom_line() +
                                                                                     geom_line(data = ham_ad_2021_lap41, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 41")) +
                                                                                     labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                        name = "Max Verstappen Vs Lewis Hamilton",
                                                                                                        breaks = c("Max Verstappen Lap 41", "Lewis Hamilton Lap 41"),
                                                                                                        labels = c("Lap 41", "Lap 41"))
                                                                                   
                                                                                   p2 <- ggplot(data = ver_ad_2021_lap41, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 41")) +
                                                                                     geom_line() +
                                                                                     geom_line(data = ham_ad_2021_lap41, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 41")) +
                                                                                     labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                        name = "Max Verstappen vs Lewis Hamilton",
                                                                                                        breaks = c("Max Verstappen Lap 41", "Lewis Hamilton Lap 41"),
                                                                                                        labels = c("Lap 41", "Lap 41")) +
                                                                                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                                   
                                                                                   ver_ad_2021_lap41$Brake <- ifelse(ver_ad_2021_lap41$Brake, 1, 0)
                                                                                   ham_ad_2021_lap41$Brake <- ifelse(ham_ad_2021_lap41$Brake, 1, 0)
                                                                                   
                                                                                   p3 <- ggplot(data = ver_ad_2021_lap41, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 41")) +
                                                                                     geom_line() +
                                                                                     geom_line(data = ham_ad_2021_lap41, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 41")) +
                                                                                     labs(y = "Brake", x = "Distance(in meters)") + 
                                                                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                        name = "Max Verstappen vs Lewis Hamilton",
                                                                                                        breaks = c("Max Verstappen Lap 41", "Lewis Hamilton Lap 41"),
                                                                                                        labels = c("Lap 41", "Lap 41")) +
                                                                                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                                   
                                                                                   p4 <- ggplot(data = ver_ad_2021_lap41, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 41")) +
                                                                                     geom_line() +
                                                                                     geom_line(data = ham_ad_2021_lap41, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 41")) +
                                                                                     labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                        name = "Max Verstappen vs Lewis Hamilton",
                                                                                                        breaks = c("Max Verstappen Lap 41", "Lewis Hamilton Lap 41"),
                                                                                                        labels = c("Lap 41", "Lap 41")) +
                                                                                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                                   
                                                                                   p5 <- ggplot(data = ver_ad_2021_lap41, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 41")) +
                                                                                     geom_line() +
                                                                                     geom_line(data = ham_ad_2021_lap41, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 41")) +
                                                                                     labs(y = "RPM", x = "Distance(in meters)") + 
                                                                                     scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                        name = "Max Verstappen vs Lewis Hamilton",
                                                                                                        breaks = c("Max Verstappen Lap 41", "Lewis Hamilton Lap 41"),
                                                                                                        labels = c("Lap 41", "Lap 41")) +
                                                                                     guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                                   
                                                                                   grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                                                   
                                                                                 }
    
                                                                                 else
                                                                                 {
                                                                                   if (input$lap_select == 42) {
                                                                                     p1 <- ggplot(data = ver_ad_2021_lap42, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 42")) +
                                                                                       geom_line() +
                                                                                       geom_line(data = ham_ad_2021_lap42, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 42")) +
                                                                                       labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                          name = "Max Verstappen Vs Lewis Hamilton",
                                                                                                          breaks = c("Max Verstappen Lap 42", "Lewis Hamilton Lap 42"),
                                                                                                          labels = c("Lap 42", "Lap 42"))
                                                                                     
                                                                                     p2 <- ggplot(data = ver_ad_2021_lap42, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 42")) +
                                                                                       geom_line() +
                                                                                       geom_line(data = ham_ad_2021_lap42, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 42")) +
                                                                                       labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                          name = "Max Verstappen vs Lewis Hamilton",
                                                                                                          breaks = c("Max Verstappen Lap 42", "Lewis Hamilton Lap 42"),
                                                                                                          labels = c("Lap 42", "Lap 42")) +
                                                                                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                                     
                                                                                     ver_ad_2021_lap42$Brake <- ifelse(ver_ad_2021_lap42$Brake, 1, 0)
                                                                                     ham_ad_2021_lap42$Brake <- ifelse(ham_ad_2021_lap42$Brake, 1, 0)
                                                                                     
                                                                                     p3 <- ggplot(data = ver_ad_2021_lap42, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 42")) +
                                                                                       geom_line() +
                                                                                       geom_line(data = ham_ad_2021_lap42, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 42")) +
                                                                                       labs(y = "Brake", x = "Distance(in meters)") + 
                                                                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                          name = "Max Verstappen vs Lewis Hamilton",
                                                                                                          breaks = c("Max Verstappen Lap 42", "Lewis Hamilton Lap 42"),
                                                                                                          labels = c("Lap 42", "Lap 42")) +
                                                                                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                                     
                                                                                     p4 <- ggplot(data = ver_ad_2021_lap42, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 42")) +
                                                                                       geom_line() +
                                                                                       geom_line(data = ham_ad_2021_lap42, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 42")) +
                                                                                       labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                          name = "Max Verstappen vs Lewis Hamilton",
                                                                                                          breaks = c("Max Verstappen Lap 42", "Lewis Hamilton Lap 42"),
                                                                                                          labels = c("Lap 42", "Lap 42")) +
                                                                                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                                     
                                                                                     p5 <- ggplot(data = ver_ad_2021_lap42, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 42")) +
                                                                                       geom_line() +
                                                                                       geom_line(data = ham_ad_2021_lap42, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 42")) +
                                                                                       labs(y = "RPM", x = "Distance(in meters)") + 
                                                                                       scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                          name = "Max Verstappen vs Lewis Hamilton",
                                                                                                          breaks = c("Max Verstappen Lap 42", "Lewis Hamilton Lap 42"),
                                                                                                          labels = c("Lap 42", "Lap 42")) +
                                                                                       guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                                     
                                                                                     grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                                                     
                                                                                   }
                                                                                   else
                                                                                   {
                                                                                     if (input$lap_select == 43) {
                                                                                       p1 <- ggplot(data = ver_ad_2021_lap43, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 43")) +
                                                                                         geom_line() +
                                                                                         geom_line(data = ham_ad_2021_lap43, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 43")) +
                                                                                         labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
                                                                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                            name = "Max Verstappen Vs Lewis Hamilton",
                                                                                                            breaks = c("Max Verstappen Lap 43", "Lewis Hamilton Lap 43"),
                                                                                                            labels = c("Lap 43", "Lap 43"))
                                                                                       
                                                                                       p2 <- ggplot(data = ver_ad_2021_lap43, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 43")) +
                                                                                         geom_line() +
                                                                                         geom_line(data = ham_ad_2021_lap43, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 43")) +
                                                                                         labs(y = "Throttle(%)", x = "Distance(in meters)") + 
                                                                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                            name = "Max Verstappen vs Lewis Hamilton",
                                                                                                            breaks = c("Max Verstappen Lap 43", "Lewis Hamilton Lap 43"),
                                                                                                            labels = c("Lap 43", "Lap 43")) +
                                                                                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                                       
                                                                                       ver_ad_2021_lap43$Brake <- ifelse(ver_ad_2021_lap43$Brake, 1, 0)
                                                                                       ham_ad_2021_lap43$Brake <- ifelse(ham_ad_2021_lap43$Brake, 1, 0)
                                                                                       
                                                                                       p3 <- ggplot(data = ver_ad_2021_lap43, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 43")) +
                                                                                         geom_line() +
                                                                                         geom_line(data = ham_ad_2021_lap43, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 43")) +
                                                                                         labs(y = "Brake", x = "Distance(in meters)") + 
                                                                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                            name = "Max Verstappen vs Lewis Hamilton",
                                                                                                            breaks = c("Max Verstappen Lap 43", "Lewis Hamilton Lap 43"),
                                                                                                            labels = c("Lap 43", "Lap 43")) +
                                                                                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                                       
                                                                                       p4 <- ggplot(data = ver_ad_2021_lap43, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 43")) +
                                                                                         geom_line() +
                                                                                         geom_line(data = ham_ad_2021_lap43, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 43")) +
                                                                                         labs(y = "Gear Number", x = "Distance(in meters)") + 
                                                                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                            name = "Max Verstappen vs Lewis Hamilton",
                                                                                                            breaks = c("Max Verstappen Lap 43", "Lewis Hamilton Lap 43"),
                                                                                                            labels = c("Lap 43", "Lap 43")) +
                                                                                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                                       
                                                                                       p5 <- ggplot(data = ver_ad_2021_lap43, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 43")) +
                                                                                         geom_line() +
                                                                                         geom_line(data = ham_ad_2021_lap43, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 43")) +
                                                                                         labs(y = "RPM", x = "Distance(in meters)") + 
                                                                                         scale_color_manual(values = c("darkred", "steelblue"), 
                                                                                                            name = "Max Verstappen vs Lewis Hamilton",
                                                                                                            breaks = c("Max Verstappen Lap 43", "Lewis Hamilton Lap 43"),
                                                                                                            labels = c("Lap 43", "Lap 43")) +
                                                                                         guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
                                                                                       
                                                                                       grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
                                                                                       
                                                                                     }
                                                                                    
    else {
      NULL
    }
  }
                   } 
                 }
         }
           }
         }
           }
       }
     }
                                                                                   }
                                                                                 }
             }
               }                                                        }                  }
                                                                     }        
                                                                     }
           }
                                                             }
                                             }
                                           }
                           }
                         }
                                     }
                                   }
               }
             }
         }
                   }
                 }
                 }
                                           }
                                         }
                                       }
                                     }
                       
                               }
               }}}
                       }
                     }
                         }
                       
                     
                   

    
    })
  }

# Run the app
shinyApp(ui, server)
