# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(maps)

# Define file paths (update these paths to the actual file locations)
gdp_file <- "data/Filtered_GDP_Per_Capita.csv"
medals_file <- "data/Filtered_Olympic_Medals.csv"
urban_file <- "data/Filtered_Urbanization_Data.csv"

# Check if files exist and load datasets
if (file.exists(gdp_file) & file.exists(medals_file) & file.exists(urban_file)) {
  gdp_data <- read.csv(gdp_file)
  medals_data <- read.csv(medals_file)
  urban_data <- read.csv(urban_file)
} else {
  stop("One or more files not found. Please check the file paths.")
}

# Merge GDP, medals, and urbanization data on NOC and Year
merged_data <- merge(gdp_data, medals_data, by = c("NOC", "Year"))
merged_data <- merge(merged_data, urban_data, by = c("NOC", "Year"))

# Define top 15 and bottom 15 countries by medals
top_15_countries <- c("USA", "CHN", "GER", "RUS", "GBR", "FRA", "JPN", "CAN", "AUS", "ITA", 
                      "KOR", "NED", "NOR", "SWE", "AUT")
bottom_15_countries <- c("ROC", "SUI", "UKR", "CUB", "ESP", "BRA", "HUN", "CZE", 
                         "POL", "NZL", "BLR", "KEN", "KAZ", "DEN", "JAM")

# Define UI
ui <- fluidPage(
  titlePanel(HTML("<h2 style='color:#2E86C1; text-align:center;'>Olympic Performance Analysis</h2>")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("rank_group", "Select Country Group:",
                  choices = c("All", "Top 15", "Bottom 15"), selected = "All"),
      sliderInput("year_range", "Select Year Range:", 
                  min = min(merged_data$Year), max = max(merged_data$Year),
                  value = c(min(merged_data$Year), max(merged_data$Year)),
                  step = 1),
      selectInput("season", "Select Olympic Season:",
                  choices = c("All", "Summer", "Winter"),
                  selected = "All"),
      
      # Conditionally display "Select Metric for Color Depth" based on the selected tab
      conditionalPanel(
        condition = "input.tabSelected != 'Trends Over Time'",
        selectInput("metric", "Select Metric for Color Depth:", 
                    choices = c("GDP per Capita", "Urbanization", "Total Medals"),
                    selected = "Total Medals")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabSelected",
        
        # First tab - Trends Over Time
        tabPanel("Trends Over Time",
                 plotOutput("gdpTrend"),
                 plotOutput("medalsTrend"),
                 plotOutput("urbanizationTrend"),
                 HTML("<br><br><br><br>"),  # Add spacing
                 tags$img(src = "olympic_icon.png", height = "60px", style = "display: block; margin: auto;")),
        
        # World Map tab
        tabPanel("World Map", 
                 plotOutput("worldMap"),
                 HTML("<br><br><br><br>"),  # Add spacing
                 tags$img(src = "olympic_icon.png", height = "60px", style = "display: block; margin: auto;")),
        
        # GDP Analysis tab with description and conclusion
        tabPanel("GDP Analysis",
                 HTML("<p><strong>Analysis Goal:</strong> This page aims to explore the relationship between GDP per capita and the total number of Olympic medals. Observing this scatter plot allows us to assess if economic development influences Olympic performance.</p>"),
                 plotOutput("scatterPlot"),
                 HTML("<p><em>Conclusion:</em> The moderate correlation coefficient of 0.51 between GDP per capita and total Olympic medals suggests that there is a positive relationship, indicating that higher economic development may be associated with greater Olympic success.</p>"),
                 HTML("<br><br><br><br>"),  # Add spacing
                 tags$img(src = "olympic_icon.png", height = "60px", style = "display: block; margin: auto;")),
        
        # Urbanization Analysis tab with description and conclusion
        tabPanel("Urbanization Analysis",
                 HTML("<p><strong>Analysis Goal:</strong> This page examines the relationship between urbanization rate and Olympic medal counts. The scatter plot helps assess if urbanization levels correlate with Olympic success.</p>"),
                 plotOutput("urbanPlot"),
                 HTML("<p><em>Conclusion:</em> The low correlation coefficient of 0.08 between urbanization rate and Olympic medals indicates a weak relationship, suggesting that urbanization level alone may have minimal influence on a country’s Olympic success.</p>"),
                 HTML("<br><br><br><br>"),  # Add spacing
                 tags$img(src = "olympic_icon.png", height = "60px", style = "display: block; margin: auto;"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive data filtered by selected year range, season, and rank group
  filtered_data <- reactive({
    data <- merged_data
    
    if (input$rank_group == "Top 15") {
      data <- data %>% filter(NOC %in% top_15_countries)
    } else if (input$rank_group == "Bottom 15") {
      data <- data %>% filter(NOC %in% bottom_15_countries)
    }
    
    data <- data %>% filter(Year >= input$year_range[1], Year <= input$year_range[2])
    
    if (input$season == "Summer") {
      data <- data %>% filter(Season == "S")
    } else if (input$season == "Winter") {
      data <- data %>% filter(Season == "W")
    }
    
    return(data)
  })
  
  # GDP vs. Medals Plot
  output$scatterPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = GDP_per_Capita, y = Total, color = NOC)) +
      geom_point(size = 3, alpha = 0.8) +
      labs(title = paste("GDP per Capita vs. Total Olympic Medals -", input$season, "Olympics"),
           x = "GDP per Capita",
           y = "Total Olympic Medals") +
      theme_minimal(base_size = 15) +
      theme(
        plot.title = element_text(hjust = 0.5, color = "#2E86C1", size = 16, face = "bold"),
        legend.position = "right"
      )
  })
  
  
  # Urbanization vs. Medals Plot
  output$urbanPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Urbanization, y = Total, color = NOC)) +
      geom_point(size = 3, alpha = 0.8) +
      labs(title = paste("Urbanization vs. Total Olympic Medals -", input$season, "Olympics"),
           x = "Urbanization (%)",
           y = "Total Olympic Medals") +
      theme_minimal(base_size = 15) +
      theme(
        plot.title = element_text(hjust = 0.5, color = "#2E86C1", size = 16, face = "bold"),
        legend.position = "right"
      )
  })

  
  # Line Chart for GDP per Capita Trend
  output$gdpTrend <- renderPlot({
    ggplot(filtered_data(), aes(x = Year, y = GDP_per_Capita, color = NOC)) +
      geom_line() +
      geom_point() +
      labs(title = "GDP per Capita Over Time", y = "GDP per Capita", x = "Year") +
      theme_minimal()
  })
  
  # Line Chart for Total Medals Trend
  output$medalsTrend <- renderPlot({
    ggplot(filtered_data(), aes(x = Year, y = Total, color = NOC)) +
      geom_line() +
      geom_point() +
      labs(title = "Total Olympic Medals Over Time", y = "Total Medals", x = "Year") +
      theme_minimal()
  })
  
  # Line Chart for Urbanization Rate Trend
  output$urbanizationTrend <- renderPlot({
    ggplot(filtered_data(), aes(x = Year, y = Urbanization, color = NOC)) +
      geom_line() +
      geom_point() +
      labs(title = "Urbanization Rate Over Time", y = "Urbanization (%)", x = "Year") +
      theme_minimal()
  })
  
  # World Map with medals and selected metric as color depth
  output$worldMap <- renderPlot({
    metric <- switch(input$metric, 
                     "GDP per Capita" = "GDP_per_Capita",
                     "Urbanization" = "Urbanization",
                     "Total Medals" = "Total")
    
    # Load world map data
    world_map <- map_data("world")
    
    # Map NOC codes to full country names for compatibility with world map data
    country_codes <- data.frame(
      NOC = c("USA", "CHN", "GER", "RUS", "GBR", "FRA", "JPN", "CAN", "AUS", "ITA", 
              "KOR", "NED", "NOR", "SWE", "AUT", "ROC", "SUI", "UKR", "CUB", "ESP", 
              "BRA", "HUN", "CZE", "POL", "NZL", "BLR", "KEN", "KAZ", "DEN", "JAM"),
      country_name = c("United States", "China", "Germany", "Russia", "United Kingdom", 
                       "France", "Japan", "Canada", "Australia", "Italy", 
                       "South Korea", "Netherlands", "Norway", "Sweden", "Austria", 
                       "Russia", "Switzerland", "Ukraine", "Cuba", "Spain", 
                       "Brazil", "Hungary", "Czech Republic", "Poland", "New Zealand", 
                       "Belarus", "Kenya", "Kazakhstan", "Denmark", "Jamaica")
    )
    
    # Join country names with merged_data to match world map data
    mapped_data <- merged_data %>%
      left_join(country_codes, by = "NOC") %>%
      rename(region = country_name) %>%
      filter(!is.na(region))  # Filter out rows without a matched country name
    
    # Further join with world_map for the selected metric
    map_data <- world_map %>%
      left_join(mapped_data %>% filter(Year == max(input$year_range)), by = "region")
    
    ggplot(map_data, aes(x = long, y = lat, group = group, fill = .data[[metric]])) +
      geom_polygon(color = "white") +
      scale_fill_gradient(
        low = if (metric == "Urbanization") "#B0C4DE" else "#CD7F32",
        high = if (metric == "Urbanization") "#4682B4" else "#FFD700",
        name = input$metric
      ) +
      labs(title = paste("Olympic", input$metric, "by Country in", max(input$year_range)),
           x = "", y = "") +
      theme_void() +
      theme(legend.position = "bottom", 
            plot.title = element_text(hjust = 0.5, color = "#2E86C1", size = 16, face = "bold"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

