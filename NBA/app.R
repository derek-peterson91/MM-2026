
library(shiny)
library(tidyverse)
library(purrr)
library(hoopR)
library(scales)

season_choices <- c(
  "2025-26", "2024-25", "2023-24", "2022-23", "2021-22",
  "2020-21", "2019-20", "2018-19", "2017-18", "2016-17"
)

ui <- fluidPage(
  titlePanel("NBA Shot Chart App"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "season",
        label = "Season",
        choices = season_choices,
        selected = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Select one or more seasons..."
        )
      ),
      
      selectizeInput(
        inputId = "player",
        label = "Player",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        options = list(
          placeholder = "Start typing a player name...",
          maxOptions = 20
        )
      )
    ),
    
    mainPanel(
      plotOutput("hex_chart", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  
  players_reactive <- reactive({
    req(input$season)
    
    map_dfr(input$season, function(season_i) {
      nba_commonallplayers(
        is_only_current_season = 0,
        season = season_i
      )$CommonAllPlayers %>%
        transmute(
          player_name = DISPLAY_FIRST_LAST,
          player_id = PERSON_ID
        )
    }) %>%
      distinct(player_id, player_name) %>%
      arrange(player_name)
  })
  
  observeEvent(input$season, {
    players <- players_reactive()
    
    updateSelectizeInput(
      session = session,
      inputId = "player",
      choices = setNames(players$player_id, players$player_name),
      selected = NULL,
      server = TRUE
    )
  }, ignoreInit = TRUE)
  
  shots_reactive <- reactive({
    req(input$season, input$player)
    
    map_dfr(input$season, function(season_i) {
      res <- nba_shotchartdetail(
        team_id = 0,
        player_id = as.numeric(input$player),
        season = season_i,
        season_type = "Regular Season",
        context_measure = "FGA"
      )
      
      res$Shot_Chart_Detail %>%
        mutate(season = season_i)
    })
  })
  
  shots_plot <- reactive({
    req(shots_reactive())
    
    shots_reactive() %>%
      mutate(
        LOC_X = as.numeric(LOC_X),
        LOC_Y = as.numeric(LOC_Y),
        SHOT_MADE_FLAG = as.numeric(SHOT_MADE_FLAG),
        x_plot = LOC_X / 10,
        y_plot = LOC_Y / 10 + BASKET_Y
      ) %>%
      filter(
        is.finite(x_plot),
        is.finite(y_plot),
        is.finite(SHOT_MADE_FLAG)
      )
  })
  
  output$hex_chart <- renderPlot({
    req(shots_plot())
    
    ggplot(shots_plot(), aes(x = x_plot, y = y_plot)) +
      
      draw_court() +
      
      stat_summary_hex(
        aes(z = SHOT_MADE_FLAG),
        fun = mean,
        bins = 30,
        alpha = 0.95
      ) +
      
      scale_fill_gradient(
        low = "#8a6950",
        high = "#ff9339",
        na.value = NA,
        name = "FG%",
        labels = label_percent(accuracy = 1)
      ) +
      coord_fixed(
        xlim = c(-COURT_W/2 - 1, COURT_W/2 + 1),
        ylim = c(BASELINE_Y, HALFCOURT_Y),
        expand = FALSE
      ) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "gray15", color = NA),
        panel.background = element_rect(fill = "gray15", color = NA),
        plot.margin = margin(10, 25, 10, 25),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.margin = margin(5, 10, 5, 10),
        legend.box.margin = margin(0, 15, 0, 0)
      )
  })
}

shinyApp(ui, server)
