# Load required packages
library(shiny)
library(sf)
library(sp)
library(rgdal)
library(tidyverse)
library(ggplot2)
library(terra)
library(RCurl)
library(readr)

# Read state shapefile of India
shape_url <-"https://github.com/Anup-droid/India_Map/raw/main/Shape_files.zip"
download.file(shape_url, "Shape_files.zip")
unzip("Shape_files.zip")
ind_shape <- st_read("Shape_files/state.shp")

# Define UI for app
ui <- fluidPage(
  titlePanel("Missed Opportunity of Vaccination in India"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "var",
        label = "Select variable:",
        choices = c(
          "NFHS5:visit wise %",
          "NFHS5:OPV1-Penta1",
          "NFHS5:OPV2-Penta2",
          "NFHS5:OPV3-Penta3",
          "NFHS4:visit wise %",
          "NFHS4:OPV1-Penta1",
          "NFHS4:OPV2-Penta2",
          "NFHS4:OPV3-Penta3",
          "NFHS4 to NFHS5:visit wise",
          "NFHS4 to NFHS5:OPV1-Penta1",
          "NFHS4 to NFHS5:OPV2-Penta2",
          "NFHS4 to NFHS5:OPV3-Penta3"
        ),
        selected = "NFHS5:visit wise %"
      ),
      downloadButton("download_map", "Download Map (JPG)")
    ),
    mainPanel(plotOutput("ind_map"))
  )
)

# Define server logic for app
server <- function(input, output) {
  # Data Import
  # ind_data <- read_csv("C:/Users/Anup Kumar/Desktop/India_Map/ind_data.csv")
  #Getting data from github
  data_url = "https://raw.githubusercontent.com/Anup-droid/MO_Immunization/main/ind_data.csv"
  
  ind_data <- read_csv(data_url)
  
  # Merge data with shapefile
  ind_map_data <-
    merge(ind_shape, ind_data, by.x = "OBJECTID", by.y = "OBJECTID")
  
  # Render map
  output$ind_map <- renderPlot({
    ggplot(ind_map_data, aes(fill = !!sym(input$var))) +
      geom_sf(color = "black") +
      scale_fill_viridis_b() +
      labs(
        title = "Missed Opportunity of Vaccination in India",
        subtitle = paste0(input$var, " "),
        caption = "Source: NFHS:5 & NFHS:4"
      ) +
      theme(title = element_text(face = "bold"),
            legend.position = "left") +
      theme_void()
  })
  
  # Download map
  output$download_map <- downloadHandler(
    filename = function() {
      paste0("vaccination_map_", input$var, ".jpg")
    },
    content = function(file) {
      ggsave(
        file,
        plot = ggplot(ind_map_data, aes(fill = !!sym(input$var))) +
          geom_sf(color = "black") +
          scale_fill_viridis_b() +
          labs(
            title = "Missed Opportunity of Vaccination in India",
            subtitle = paste0(input$var, ""),
            caption = "Source: NFHS:5 & NFHS:4"
          ) +
          theme(
            title = element_text(face = "bold"),
            legend.position = "left"
          ) +
          theme_void(),
        device = "jpeg",
        dpi = 300
      )
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
