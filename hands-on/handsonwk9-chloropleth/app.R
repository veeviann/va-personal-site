library(shiny)
library(tmap)
library(colourpicker)
library(tidyverse)

mpsz_pop2020 <- read_rds("mpszpop2020.rds")

ui <- fluidPage(
  titlePanel("Distribution of Dependency Ratio by Planning Subzone"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("regionFilter", "Select Regions:", 
                  choices = unique(mpsz_pop2020$REGION_N), 
                  multiple = TRUE, 
                  selected = unique(mpsz_pop2020$REGION_N)),
      
      selectInput("clfMethod", "Select Classification Method:", 
                  choices = c("cat", "fixed", "sd", "equal", "pretty", 
                              "quantile", "kmeans", "hclust", "bclust", 
                              "fisher", "jenks", "dpih", "headtails", "log10_pretty"),
                  selected = "quantile"),
      
      sliderInput("n", "Number of Classes (n):", 
                  min = 2, max = 20, value = 5, step = 1),
      
      # Color picker for the start of the gradient
      colourInput("startColor", "Select Start Color:", value = "#95D2DB"),
      
      # Color picker for the end of the gradient
      colourInput("endColor", "Select End Color:", value = "#1219E6"),
      
      checkboxInput("legendAsHist", "Show Legend with Histogram", value = FALSE),
      
      checkboxInput("legendIsPortrait", "Legend in Portrait Mode", value = TRUE),
      
      selectInput("style", "Select Map Style", 
                  choices = c("white", "gray", "natural", "cobalt", 
                              "albatross", "beaver", "bw", "classic", 
                              "watercolor", "v3", "gray_v3", "natural_v3", "cobalt_v3", 
                              "albatross_v3", "beaver_v3", "bw_v3", "classic_v3", "watercolor_v3"),
                  selected = "white")
      
    ),
    
    mainPanel(
      plotOutput("mapPlot")
    )
  )
)

server <- function(input, output, session) {
  
  output$mapPlot <- renderPlot({
    gradient_palette <- colorRampPalette(c(input$startColor, input$endColor))
    
    filtered_data <- mpsz_pop2020 %>%
      filter(REGION_N %in% input$regionFilter)
    
    tm_shape(filtered_data) +
    tm_polygons("DEPENDENCY",
            # fill_alpha = 0.7,  # There's a bug here if we put this the portrait doesn't work!!! :@
            style = input$clfMethod,
            n = input$n,
            palette = gradient_palette(input$n),
            legend.hist = input$legendAsHist,
            legend.hist.z = 0.1,
            legend.is.portrait = input$legendIsPortrait,
            fill.legend = tm_legend(title = "Dependency Ratio")) +
      tm_compass(position = c("right", "bottom"), size = 2) +
      tm_borders(lwd = 0.5, fill_alpha = 0.5) +
      tm_style(input$style)
  })
}

shinyApp(ui, server)