# Load required packages
library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)


# Load telemetry data
lap_times <- read.csv("lap_times.csv")

# create an empty list to store the data
ham_ad_2021 <- list()
ver_ad_2021 <- list()

# loop through the lap numbers and load the data
for (i in 1:58) {
  ham_ad_2021[[i]] <- read.csv(paste0("ham_data_lap_2021_ad_", i, ".csv"))
  ver_ad_2021[[i]] <- read.csv(paste0("ver_data_lap_2021_ad_", i, ".csv"))
}


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
  
  for(i in 1:58)
  {
    if (input$lap_select == i)
    {
    ver_df <-ver_ad_2021[[i]]
    ham_df <-ham_ad_2021[[i]]
    
    ver_df$Brake <- ifelse(ver_df$Brake, 1, 0)
    ham_df$Brake <- ifelse(ham_df$Brake, 1, 0)
    
    p1 <- ggplot(data = ver_df, aes(x = Distance, y = Speed, color = "Max Verstappen")) +
      geom_line() +
      geom_line(data = ham_df, aes(x = Distance, y = Speed, color = "Lewis Hamilton")) +
      labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
      scale_color_manual(values = c("darkred", "steelblue"), 
                         name = "Max Verstappen vs Lewis Hamilton",
                         breaks = c("Max Verstappen", "Lewis Hamilton"),
                         labels = c("Max Verstappen", "Lewis Hamilton"))
    
    p2 <- ggplot(data = ver_df, aes(x = Distance, y = Throttle, color = "Max Verstappen")) +
      geom_line() +
      geom_line(data = ham_df, aes(x = Distance, y = Throttle, color = "Lewis Hamilton")) +
      labs(y = "Throttle(%)", x = "Distance(in meters)") + 
      scale_color_manual(values = c("darkred", "steelblue"), 
                         name = "Max Verstappen vs Lewis Hamilton",
                         breaks = c("Max Verstappen", "Lewis Hamilton"),
                         labels = c("Max Verstappen", "Lewis Hamilton")) +
      guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
    
    p3 <- ggplot(data = ver_df, aes(x = Distance, y = Brake, color = "Max Verstappen")) +
      geom_line() +
      geom_line(data = ham_df, aes(x = Distance, y = Brake, color = "Lewis Hamilton")) +
      labs(y = "Brake", x = "Distance(in meters)") + 
      scale_color_manual(values = c("darkred", "steelblue"), 
                         name = "Max Verstappen vs Lewis Hamilton",
                         breaks = c("Max Verstappen", "Lewis Hamilton"),
                         labels = c("Max Verstappen", "Lewis Hamilton")) +
      guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
    
    p4 <- ggplot(data = ver_df, aes(x = Distance, y = nGear, color = "Max Verstappen")) +
      geom_line() +
      geom_line(data = ham_df, aes(x = Distance, y = nGear, color = "Lewis Hamilton")) +
      labs(y = "Gear number", x = "Distance(in meters)") + 
      scale_color_manual(values = c("darkred", "steelblue"), 
                         name = "Max Verstappen vs Lewis Hamilton",
                         breaks = c("Max Verstappen", "Lewis Hamilton"),
                         labels = c("Max Verstappen", "Lewis Hamilton")) +
      guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
    
    
    p5 <- ggplot(data = ver_df, aes(x = Distance, y = RPM, color = "Max Verstappen")) +
      geom_line() +
      geom_line(data = ham_df, aes(x = Distance, y = RPM, color = "Lewis Hamilton")) +
      labs(y = "RPM", x = "Distance(in meters)") + 
      scale_color_manual(values = c("darkred", "steelblue"), 
                         name = "Max Verstappen vs Lewis Hamilton",
                         breaks = c("Max Verstappen", "Lewis Hamilton"),
                         labels = c("Max Verstappen", "Lewis Hamilton")) +
      guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))
    
    grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
}}})

}


# Run the app
shinyApp(ui, server)      
      
        
      
