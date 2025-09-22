library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shiny)
library(plotly)

# Load the data
players_total_avg <- read_excel("../data/processed-data/PlayersTotal_AVG_modif.xlsx")
load("../data/extended_performance_stats.RData")

players_total_avg <- players_total_avg %>%
  filter(appearences >= 50)

# Create ranking for each surface
ranking_clay <- extended_performance_stats %>%
  filter(surface == "Clay") %>%
  arrange(desc(win_rate))

ranking_hard <- extended_performance_stats %>%
  filter(surface == "Hard") %>%
  arrange(desc(win_rate))

ranking_grass <- extended_performance_stats %>%
  filter(surface == "Grass") %>%
  arrange(desc(win_rate))

ranking_all <- extended_performance_stats %>%
  arrange(desc(win_rate))

# UI code
ui <- fluidPage(
  titlePanel("Tennis Player Performance Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("surface", "Choose a surface", choices = c("Clay", "Hard", "Grass", "All")),
      textInput("player", "Enter player name")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table View", tableOutput("results")),
        tabPanel("Service Analysis", plotlyOutput("serviceAnalysis")),
        tabPanel("Break Points", plotlyOutput("breakPointsPlot")),
        tabPanel("Serve Points Won", plotlyOutput("servePointsWonPlot")),
        tabPanel("Aces and Faults", plotlyOutput("acesAndFaultsPlot")),
        tabPanel("Serve Effectiveness", plotlyOutput("serveEffectivenessPlot"))
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  output$results <- renderTable({
    data <- switch(input$surface,
                   "Clay" = ranking_clay,
                   "Hard" = ranking_hard,
                   "Grass" = ranking_grass,
                   "All" = ranking_all)
    subset(data, grepl(input$player, name, ignore.case = TRUE))
  })
  
  # Function to generate an interactive Plotly chart
  generate_plotly <- function(data, x, y, size, tooltip_fields, title, xlabel, ylabel) {
    p <- ggplot(data, aes_string(x = x, y = y)) +
      geom_point(aes_string(size = size, text = tooltip_fields), alpha = 0.7) +
      geom_smooth(method = "lm", color = "blue") +
      labs(title = title, x = xlabel, y = ylabel) +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  }
  
  # Plots for different analyses
  output$serviceAnalysis <- renderPlotly({
    data <- filter_data(input$surface, input$player)
    generate_plotly(data, "service_games", "first_serve_in", "aces_per_game", 
                    paste("Name:", data$name, "<br>Aces per game:", data$aces_per_game), 
                    "First Serve In vs. Service Games", "Service Games", "First Serve In (%)")
  })
  
  # Service analysis plot
  output$serviceAnalysis <- renderPlotly({
    data <- switch(input$surface,
                   "Clay" = ranking_clay,
                   "Hard" = ranking_hard,
                   "Grass" = ranking_grass,
                   "All" = ranking_all)
    player_data <- subset(data, grepl(input$player, name, ignore.case = TRUE))
    p <- ggplot(player_data, aes(x = service_games, y = first_serve_in)) +
      geom_point(aes(size = aces_per_game, color = aces_per_game), alpha = 0.7) +
      geom_smooth(method = "lm", color = "blue") +
      labs(title = "First Serve In vs. Service Games", x = "Service Games", y = "First Serve In (%)") +
      scale_color_gradient(low = "blue", high = "red") +
      theme_minimal()
    ggplotly(p, tooltip = c("x", "y", "size"))
  })
  
  # Break points plot
  output$breakPointsPlot <- renderPlotly({
    data <- switch(input$surface,
                   "Clay" = ranking_clay,
                   "Hard" = ranking_hard,
                   "Grass" = ranking_grass,
                   "All" = ranking_all)
    player_data <- subset(data, grepl(input$player, name, ignore.case = TRUE))
    p <- ggplot(player_data, aes(x = bp_faced, y = bp_saved, color = bp_saved / bp_faced)) +
      geom_point(size = 5, alpha = 0.7) +
      labs(title = "Break Points Faced vs. Saved", x = "Break Points Faced", y = "Break Points Saved") +
      scale_color_gradient(low = "red", high = "green") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Serve points won plot
  output$servePointsWonPlot <- renderPlotly({
    data <- switch(input$surface,
                   "Clay" = ranking_clay,
                   "Hard" = ranking_hard,
                   "Grass" = ranking_grass,
                   "All" = ranking_all)
    player_data <- subset(data, grepl(input$player, name, ignore.case = TRUE))
    p <- ggplot(player_data, aes(x = first_serve_points_won, y = second_serve_points_won)) +
      geom_point(aes(color = service_points), size = 4, alpha = 0.6) +
      labs(title = "Serve Points Won: 1st vs 2nd", x = "First Serve Points Won", y = "Second Serve Points Won") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Aces and double faults plot
  output$acesAndFaultsPlot <- renderPlotly({
    data <- switch(input$surface,
                   "Clay" = ranking_clay,
                   "Hard" = ranking_hard,
                   "Grass" = ranking_grass,
                   "All" = ranking_all)
    player_data <- subset(data, grepl(input$player, name, ignore.case = TRUE))
    p <- ggplot(player_data, aes(x = aces_per_game, y = double_faults_per_game)) +
      geom_point(aes(color = service_games), size = 4, alpha = 0.6) +
      labs(title = "Aces vs Double Faults per Game", x = "Aces per Game", y = "Double Faults per Game") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Serve effectiveness plot
  output$serveEffectivenessPlot <- renderPlotly({
    data <- switch(input$surface,
                   "Clay" = ranking_clay,
                   "Hard" = ranking_hard,
                   "Grass" = ranking_grass,
                   "All" = ranking_all)
    player_data <- subset(data, grepl(input$player, name, ignore.case = TRUE))
    p <- ggplot(player_data, aes(x = first_serve_effectiveness, y = second_serve_effectiveness)) +
      geom_point(aes(color = first_serve_accuracy), size = 4, alpha = 0.6) +
      labs(title = "First vs Second Serve Effectiveness", x = "First Serve Effectiveness", y = "Second Serve Effectiveness") +
      theme_minimal()
    ggplotly(p)
  })
}

# Function to filter data
filter_data <- function(surface, player) {
  data <- switch(surface,
                 "Clay" = ranking_clay,
                 "Hard" = ranking_hard,
                 "Grass" = ranking_grass,
                 "All" = ranking_all)
  subset(data, grepl(player, name, ignore.case = TRUE))
}

# Run the application
shinyApp(ui = ui, server = server)

