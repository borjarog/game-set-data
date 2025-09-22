library(ggplot2)
library(shiny)
library(stringr)
library(dplyr)

load("../data/AVG_año.Rdata")

players <- players2
players$name_com <- paste(players$name_first, players$name_last)

generador <- shinyApp(
  ui = fluidPage(
    titlePanel("Interactive Player Search and Plots"),
    sidebarLayout(
      sidebarPanel(
        textInput("search", "Search player:", ""),
        uiOutput("player_select")
      ),
      mainPanel(
        plotOutput("plot1"),
        plotOutput("plot2"),
        plotOutput("plot3"),
        plotOutput("plot4"),
        plotOutput("plot5"),
        textOutput("message")
      )
    )
  ),
  server = function(input, output, session) {
    filtered_players <- reactive({
      if (nchar(input$search) == 0) {
        return(NULL)
      } else {
        players %>% 
          filter(str_detect(tolower(name_com), tolower(input$search))) %>%
          head(5)
      }
    })
    
    output$player_select <- renderUI({
      if (is.null(filtered_players())) {
        return(NULL)
      } else {
        selectInput("player", "Select player:", choices = filtered_players()$name_com)
      }
    })
    
    output$message <- renderText({
      if (is.null(input$player)) {
        return("No player has been selected.")
      }
    })
    
    observeEvent(input$player, {
      selected_player <- filtered_players() %>%
        filter(name_com == input$player)
      
      if (nrow(selected_player) > 0) {
        id <- selected_player$player_id
        nombre <- selected_player$name_com
        
        medias_jug <- list()
        for (i in seq_along(medias_list)) {
          medias <- medias_list[[i]]
          jug <- medias[medias$player_id == id, ]
          if (nrow(jug) > 0) {
            medias_jug[[i]] <- jug[8:45]
          }
        }
        
        nom_col <- colnames(medias_list[[1]][8:45])
        
        for (i in seq_along(medias_jug)) {
          if (is.null(medias_jug[[i]])) { 
            medias_jug[[i]] <- as.data.frame(matrix(NA, ncol = 38, nrow = 1))
            colnames(medias_jug[[i]]) <- nom_col
          }
        }
        
        df_jug <- do.call(rbind, medias_jug)
        
        rownames(df_jug) <- 1999 + seq_along(medias_jug)
        df_jug <- na.omit(df_jug)
        
        output$plot1 <- renderPlot({
          ggplot(df_jug, aes(x = rownames(df_jug), y = win_per, group = 1)) +
            geom_line() +
            geom_point(aes(size = appearences)) +
            ggtitle("Plot 1") +
            labs(subtitle = paste("Win% evolution of", nombre, "by year")) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        })
        
        output$plot2 <- renderPlot({
          ggplot(df_jug, aes(x = rownames(df_jug))) +
            geom_line(aes(y = aces, group = 1, color = "aces")) +
            geom_point(aes(y = aces, color = "aces", size = appearences)) +
            geom_line(aes(y = df, group = 1, color = "df")) +
            geom_point(aes(y = df, color = "df", size = appearences)) +
            ggtitle("Plot 2") +
            labs(subtitle = paste("Evolution of aces and double faults of", nombre, "by year")) +
            scale_color_manual(values = c("aces" = "blue", "df" = "red")) +
            labs(color = "Variable") + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        })
        
        output$plot3 <- renderPlot({
          ggplot(df_jug, aes(x = rownames(df_jug))) +
            geom_line(aes(y = `%1stWon`, group = 1, color = "%1stWon")) +
            geom_point(aes(y = `%1stWon`, color = "%1stWon", size = appearences)) +
            geom_line(aes(y = `%1stIn`, group = 1, color = "%1stIn")) +
            geom_point(aes(y = `%1stIn`, color = "%1stIn", size = appearences)) +
            ggtitle("Plot 3") +
            labs(subtitle = paste("Evolution of %1stWon and %1stIn of", nombre, "by year")) +
            scale_color_manual(values = c("%1stWon" = "blue", "%1stIn" = "red")) +
            labs(color = "Variable") + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        })
        
        output$plot4 <- renderPlot({
          ggplot(df_jug, aes(x = rownames(df_jug))) +
            geom_line(aes(y = `%1stWon_resto`, group = 1, color = "%1stWon_resto")) +
            geom_point(aes(y = `%1stWon_resto`, color = "%1stWon_resto", size = appearences)) +
            geom_line(aes(y = `%2ndWon_resto`, group = 1, color = "%2ndWon_resto")) +
            geom_point(aes(y = `%2ndWon_resto`, color = "%2ndWon_resto", size = appearences)) +
            ggtitle("Plot 4") + 
            labs(subtitle = paste("Evolution of %1stWon_resto and %2ndWon_resto of", nombre, "by year")) +
            scale_color_manual(values = c("%1stWon_resto" = "blue", "%2ndWon_resto" = "red")) +
            labs(color = "Variable") + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        })
        
        output$plot5 <- renderPlot({
          ggplot(df_jug, aes(x = rownames(df_jug))) +
            geom_line(aes(y = `%SvGmsWon`, group = 1, color = "%SvGmsWon")) +
            geom_point(aes(y = `%SvGmsWon`, color = "%SvGmsWon", size = appearences)) +
            geom_line(aes(y = `%RestoGmsWon`, group = 1, color = "%RestoGmsWon")) +
            geom_point(aes(y = `%RestoGmsWon`, color = "%RestoGmsWon", size = appearences)) +
            ggtitle("Plot 5") +
            labs(subtitle = paste("Evolution of %RestoGmsWon and %SvGmsWon of", nombre, "by year")) +
            scale_color_manual(values = c("%SvGmsWon" = "blue", "%RestoGmsWon" = "red")) +
            labs(color = "Variable") + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        })
      }
    })
  }
)




