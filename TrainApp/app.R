library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2)
library(leaflet)
library(plotly)
?img
load("../Data/merged.Rdata")
as_tibble(all_trains_table)

categorize_duration <- function(time_string) {
  extracted_time <- as.numeric(strsplit(time_string, "h")[[1]][1])
  if (extracted_time <= 15) {
    return("short (0-15 hrs")
  } else if (extracted_time <= 30) {
    return("medium (15-30 hrs)")
  } else {
    return("long (30-60 hrs)")
  }
}

all_trains_table <- all_trains_table %>%
  mutate(`Duration Group` = sapply(`Travel Time`, categorize_duration))

ui <- dashboardPage(
  dashboardHeader(title = "Train App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Train search engine", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Delay Comparison", tabName = "delay", icon = icon("clock")),
      menuItem("Distance vs Speed", tabName = "DistanceVsAvgSpeed",icon = icon("car") ),
      menuItem("Map Analysis", tabName = "Map", icon = icon("map")),
      menuItem("Speed Vs TrainType", tabName = "SpeedVsTrainType", icon = icon("train"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Trains", solidHeader = TRUE, 
                    "Choose a train according to your preference",
                    br(),
                    selectInput("source", "Train from: ", unique(all_trains_table$Source)),
                    selectInput("dest", "Train to: ", unique(all_trains_table$Dest)),
                    ),
              ),
              fluidRow(
                box(
                  selectInput("sort","sort by: ", c("Travel Time", "Delays"), selected = "Delays")
                )
              ),
              fluidRow(
                box(
                  DTOutput("trains_table")
                )
              ),
      ),
      
      # Second tab content
      tabItem(tabName = "delay",
              h4("Delay comparisons"),
              fluidRow(
                selectInput("duration", "Select Duration Group: ", choices=unique(all_trains_table$`Duration Group`), multiple = TRUE, selected = c("medium (15-30 hrs)"))
              ),
              fluidRow(
                box(
                    width = 6,
                    plotOutput("duration_plot", height = 500)
                    ),
                box(
                  width = 6, 
                  plotOutput("speed_plot", height = 500)
                )
              ),
              fluidRow(
                       box(tableOutput("duration_summary"), width = 5))
      ),
      # Third tab content
      tabItem(tabName ="DistanceVsAvgSpeed",
              box(
              selectInput("ZoneSelector", label = "ZoneSelector",
                          choices = all_trains_table$Zone,
                          selected = "NR",
                          multiple = TRUE
              ),
              
              ),
              plotOutput("distancespeed",height = 600)
              ),
        #Fourth Tab content
        tabItem(tabName = "Map",
                h2("Train Type Analyser"),
                fluidRow(
                  box(
                    selectInput("dropdown", label = "TrainSelected",
                                choices = all_trains_table$Train_Type,
                                selected = "Option 1",
                    ),
                    textOutput("selected_option"),
                    height = "400px", # Adjust the height as needed
                    width = 6, # Occupies full width within the row
                    leafletOutput("map") # Assuming you'll render the leaflet map in this output
                  ),
                  box(
                    h2("Train Zones"),
                    plotOutput("pieChart")
                  )
                )
        ),
      #FifthTab
      tabItem(
        tabName = "SpeedVsTrainType",
        selectInput("zone", "Select Zone:", choices = unique(all_trains_table$Zone), ),
        selectInput("train_type", "Select Train Types:", choices = unique(all_trains_table$Train_Type), multiple = TRUE, selected = c("rajdhani", "vande-bharat")),
        plotOutput("speedPlot")
      )
      )
    )
  )


server <- function(input, output,session) {
 trainSrc <- reactive({
   input$source
 })
 trainDest <- reactive({
   input$dest
 })
 
 trainSort <- reactive({
   switch(input$sort,
          "Delays" = all_trains_table$Delays,
          "Travel Time" = all_trains_table$`Travel Time`
          )
 })
 
 
 output$trains_table <- renderDT({
   
   # Filter, arrange, and select data
   filtered_data <- all_trains_table %>% 
     filter(Source == input$source, Dest == input$dest) %>% 
     arrange_(.dots = sprintf("`%s`", input$sort)) %>%
     select(`Train#`, `Train Name`, Dept., Arr., Delays, `Travel Time`)
   
   # Get the row index with the least delay
   min_delay_row <- min(filtered_data$Delays)
   
   # Render the DataTable
   datatable(
     filtered_data,
     options = list(rownames = FALSE),
     rownames = FALSE
   ) %>%
     formatStyle(
       columns = "Delays",
       valueColumns = "Delays",
       target = "row",
       backgroundColor = styleEqual(min_delay_row, "#00FF0080")
     )
 })
 
duration_data <- reactive({all_trains_table %>% filter(`Duration Group` %in% input$duration)})
 
output$duration_plot <- renderPlot({
  ggplot(duration_data(), aes(x = `Duration Group`, y = Delays, fill = `Duration Group`)) +
    geom_boxplot() +
    labs(
      title = "Boxplot of Delays Grouped by Departure Group",
      x = "Duration Group",
      y = "Delay",
      fill = "Duration Group"
    )+ theme(
      legend.position = c(0.85, 0.9)
    )
})

output$duration_summary <- renderTable({
  all_trains_table %>% 
    group_by(`Duration Group`) %>% 
    summarize(
      Avg_delay = mean(Delays, na.rm = TRUE),
      Count = n()
    )
})

output$speed_plot <- renderPlot({
  ggplot(duration_data(), aes(y = Delays, x = `Avg Speed`, color = `Duration Group`)) +
    geom_point() +
    labs(
      title = "Scatter Plot of Delays and Durations",
      x = "Speed",
      y = "Delays",
      color = "Duration Group"
    ) + theme(
      legend.position = c(0.85, 0.9)
    )
})

#Server part related to third tab
output$distancespeed <- renderPlot({
  selected_zone <- input$ZoneSelector
  
  # Filter data for the selected zone
  subset_data <- all_trains_table[all_trains_table$Zone == selected_zone, ]
  
  # Convert Zone to a factor (if not already done)
  subset_data$Zone <- factor(subset_data$Zone)
  
  # Plot the scatter plot
  plot(
    log(as.numeric(subset_data$Dist.)),
    subset_data$`Avg Speed`,
    col = subset_data$Zone,
    pch = 16,
    main = "Scatter Plot",
    xlab = "log(Distance)",
    ylab = "Average Speed",
    xlim = c(min(log(as.numeric(subset_data$Dist.))), max(log(as.numeric(subset_data$Dist.))))
  )
  zones <- levels(subset_data$Zone)
  unique_colors <- unique(subset_data$Zone)
  
  for (i in seq_along(zones)) {
    zone <- zones[i]
    zone_subset <- subset_data[subset_data$Zone == zone, ]
    lm_model <- lm(`Avg Speed` ~ log(as.numeric(Dist.)), data = zone_subset)
    
    # Use the color of the corresponding zone
    line_color <- unique_colors[i]
    
    abline(lm_model, col = line_color, lty = 2)
  }
  legend("topright", legend = zones, col = unique_colors, pch = 16, lty = 2, title = "Zone")
})

#Server Part related to fourth tab

output$pieChart <- renderPlot({
  zone_data <- all_trains_table[all_trains_table$Train_Type == input$dropdown, ]
  zone <- zone_data %>% group_by(Zone) %>% summarise(n = n())
  ggplot(data = zone) + 
    geom_bar(mapping = aes(x = reorder(Zone, n), y = n, fill = Zone), stat = "identity") +
    labs(
      title = "Number of Shatabdi trains in various zones",
      x = "Zone",
      y = "count",
      color = "Zone"
    )
})

output$map <- renderLeaflet({
  req(input$dropdown)  # Ensure input$dropdown is available
  
  leaflet(data = all_trains_table) %>%
    addTiles() %>%
    addCircleMarkers(
      
      data = all_trains_table[all_trains_table$Train_Type == input$dropdown, ], 
      lng = ~Source_Lon, 
      lat = ~Source_Lat, 
      radius = 10, 
      color = rgb(runif(1), runif(1), runif(1)), 
      popup = ~StationNames
      
    )
})

#Server part related to fifth tab

output$speedPlot <- renderPlot({
  filtered_data <- all_trains_table %>% filter(Zone == input$zone, Train_Type %in% input$train_type)
  ggplot(filtered_data, aes(x = Train_Type, y = `Avg Speed`, fill = Train_Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal()+theme(axis.text.x = element_text(size = 12)) +
    labs(title = "Average Speed by Train Type",
         x = "Train Type",
         y = "Average Speed (km/h)")
})

}

shinyApp(ui, server)
