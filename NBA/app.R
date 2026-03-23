
library(shiny)
library(tidyverse)
library(hoopR)
library(scales)

# ── Court constants ────────────────────────────────────────────────────────────
COURT_W     <- 50
HALFCOURT_Y <- 47
BASELINE_Y  <- 0
BASKET_X    <- 0
BASKET_Y    <- 5.25
BACKBOARD_Y <- 4
BACKBOARD_HW <- 3
KEY_HW      <- 8
FT_LINE_Y   <- 18.833
FT_R        <- 6
RA_R        <- 4
THREE_R     <- 23.75
CORNER_X    <- 22
CORNER_Y    <- BASKET_Y + sqrt(THREE_R^2 - CORNER_X^2)

# ── Court helpers ──────────────────────────────────────────────────────────────
arc <- function(cx, cy, r, angle_from, angle_to, n = 300) {
  angles <- seq(angle_from, angle_to, length.out = n) * pi / 180
  data.frame(x = cx + r * cos(angles), y = cy + r * sin(angles))
}

ft_top    <- arc(0, FT_LINE_Y, FT_R, 0, 180)
ft_bottom <- arc(0, FT_LINE_Y, FT_R, 180, 360)
ra_arc    <- arc(BASKET_X, BASKET_Y, RA_R, 0, 180)
ring      <- arc(BASKET_X, BASKET_Y, 0.75, 0, 360)
three_angle <- acos(CORNER_X / THREE_R) * 180 / pi
three_arc   <- arc(BASKET_X, BASKET_Y, THREE_R, three_angle, 180 - three_angle)

key_rect <- data.frame(
  x = c(-KEY_HW, KEY_HW, KEY_HW, -KEY_HW, -KEY_HW),
  y = c(BASELINE_Y, BASELINE_Y, FT_LINE_Y, FT_LINE_Y, BASELINE_Y)
)

draw_court <- function() {
  list(
    geom_path(data = key_rect, aes(x, y),
              color = "gray50", linewidth = 1, inherit.aes = FALSE),
    geom_path(data = ft_top, aes(x, y),
              color = "gray50", linewidth = 1, inherit.aes = FALSE),
    geom_path(data = ft_bottom, aes(x, y),
              color = "gray50", linewidth = 1, linetype = "dashed", inherit.aes = FALSE),
    geom_path(data = three_arc, aes(x, y),
              color = "gray50", linewidth = 1, inherit.aes = FALSE),
    geom_segment(aes(x = -CORNER_X, xend = -CORNER_X, y = BASELINE_Y, yend = CORNER_Y),
                 color = "gray50", linewidth = 1, inherit.aes = FALSE),
    geom_segment(aes(x = CORNER_X, xend = CORNER_X, y = BASELINE_Y, yend = CORNER_Y),
                 color = "gray50", linewidth = 1, inherit.aes = FALSE),
    geom_path(data = ra_arc, aes(x, y),
              color = "gray50", linewidth = 1, inherit.aes = FALSE),
    geom_segment(aes(x = -BACKBOARD_HW, xend = BACKBOARD_HW, y = BACKBOARD_Y, yend = BACKBOARD_Y),
                 color = "gray50", linewidth = 2, inherit.aes = FALSE),
    geom_segment(aes(x = 0, xend = 0, y = BACKBOARD_Y, yend = BASKET_Y - 0.75),
                 color = "gray50", linewidth = 1, inherit.aes = FALSE),
    geom_path(data = ring, aes(x, y),
              color = "gray50", linewidth = 1, inherit.aes = FALSE)
  )
}

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectizeInput(
        inputId = "player",
        label   = "Player",
        choices = NULL,
        options = list(placeholder = "Start typing a name...", maxOptions = 20)
      ),
      selectInput(
        inputId  = "season",
        label    = "Season",
        choices  = c("2024-25", "2023-24", "2022-23", "2021-22", "2020-21"),
        selected = "2024-25"
      ),
      selectInput(
        inputId = "game",
        label   = "Game",
        choices = NULL   # populated reactively after player selected
      )
    ),
    
    mainPanel(
      width = 9,
      plotOutput("shot_chart", height = "800px", width = "800px")
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Populate player search on app load
  observe({
    players <- nba_commonallplayers(
      league_id = "00",
      season = input$season
    )$CommonAllPlayers %>%
      filter(ROSTERSTATUS == 1) %>%   # 1 = active roster, 0 = inactive
      arrange(DISPLAY_FIRST_LAST)
    
    choices <- setNames(players$PERSON_ID, players$DISPLAY_FIRST_LAST)
    
    updateSelectizeInput(
      session, "player",
      choices  = choices,
      selected = NULL,
      server   = TRUE
    )
  })
  
  # Fetch game log when player or season changes
  game_log <- reactive({
    req(input$player, input$season)
    
    nba_playergamelog(
      player_id   = input$player,
      season      = input$season,
      season_type = "Regular Season"
    )[[1]] %>%
      mutate(
        GAME_DATE  = as.Date(GAME_DATE, format = "%b %d, %Y"),
        GAME_LABEL = paste0(format(GAME_DATE, "%b %d"), " — ",
                            str_replace(MATCHUP, "vs\\.", "vs"))
      ) %>%
      arrange(desc(GAME_DATE))
  }) %>%
    bindCache(input$player, input$season)
  
  # Populate game dropdown whenever game log updates
  observeEvent(game_log(), {
    log     <- game_log()
    choices <- setNames(log$Game_ID, log$GAME_LABEL)   # Game_ID not GAME_ID
    
    updateSelectInput(session, "game",
                      choices  = choices,
                      selected = choices[1])
  })
  
  # Fetch shot data for selected game
  shot_data <- reactive({
    req(input$game)
    
    game_id <- str_pad(input$game, width = 10, side = "left", pad = "0")
    
    nba_shotchartdetail(
      player_id        = input$player,
      game_id          = game_id,
      season           = input$season,
      season_type      = "Regular Season",
      context_measure  = "FGA"
    )$Shot_Chart_Detail %>%
      mutate(
        LOC_X  = as.numeric(LOC_X),
        LOC_Y  = as.numeric(LOC_Y),
        x_plot = LOC_X / 10,
        y_plot = LOC_Y / 10 + BASKET_Y,
        result = if_else(SHOT_MADE_FLAG == "1", "Made", "Missed")
      )
  }) %>%
    bindCache(input$player, input$game)
  
  # Render shot chart
  output$shot_chart <- renderPlot({
    req(nrow(shot_data()) > 0)
    
    shots      <- shot_data()
    log        <- game_log()
    game_label <- log$GAME_LABEL[log$Game_ID == input$game]
    
    # Pull player name from the game log — PLAYER_NAME is already there
    player_name <- log$PLAYER_NAME[1]
    
    ggplot(shots, aes(x = x_plot, y = y_plot)) +
      draw_court() +
      geom_point(
        data  = shots %>% filter(result == "Missed"),
        color = "#8a6950", size = 6, alpha = 0.7
      ) +
      geom_point(
        data  = shots %>% filter(result == "Made"),
        color = "#ff9339", size = 6, alpha = 0.9
      ) +
      coord_fixed(
        xlim   = c(-COURT_W / 2, COURT_W / 2),
        ylim   = c(BASELINE_Y, HALFCOURT_Y),
        expand = FALSE
      ) +
      labs(
        title    = player_name,
        subtitle = game_label
      ) +
      theme_void() +
      theme(
        plot.background  = element_rect(fill = "#202938", color = NA),
        panel.background = element_rect(fill = "#202938", color = NA),
        plot.title       = element_text(color = "white", size = 30, face = "bold",
                                        hjust = 0.5, margin = margin(t = 10)),
        plot.subtitle    = element_text(color = "gray70", size = 26,
                                        hjust = 0.5, margin = margin(t = 4, b = 10)),
        plot.margin      = margin(t = 15, r = 0, b = 0, l = 0)
      )
  }, bg = "#202938")
}

shinyApp(ui, server)