
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
        choices  = c("2025-26", "2024-25", "2023-24", "2022-23", "2021-22", "2020-21"),
        selected = "2025-26"
      ),
      selectInput(
        inputId = "game",
        label   = "Game",
        choices = NULL
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        
        # ── Tab 1: Game Shot Chart ───────────────────────────────────────
        tabPanel("Game Shot Chart",
                 fluidRow(
                   column(8,
                          plotOutput("shot_chart", height = "800px", width = "800px")
                   ),
                   column(4,
                          uiOutput("game_stats"),
                          uiOutput("player_headshot")
                   )
                 )
        ),
        
        # ── Tab 2: Season Heatmap ────────────────────────────────────────
        tabPanel("Season Heatmap",
                 fluidRow(
                   column(12,
                          plotOutput("hex_chart", height = "800px", width = "800px")
                   )
                 )
        )
        
      )
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
  
  output$player_headshot <- renderUI({
    req(input$player)
    
    url <- paste0(
      "https://cdn.nba.com/headshots/nba/latest/1040x760/",
      input$player,
      ".png"
    )
    
    tags$img(
      src   = url,
      width = "100%",
      style = "border-radius: 8px; margin-bottom: 10px;"
    )
  })
  
  
  
  # Fetch game log when player or season changes
  # Update GAME_LABEL in game_log reactive to use new date format
  game_log <- reactive({
    req(input$player, input$season)
    
    nba_playergamelog(
      player_id   = input$player,
      season      = input$season,
      season_type = "Regular Season"
    )[[1]] %>%
      mutate(
        GAME_DATE  = as.Date(GAME_DATE, format = "%b %d, %Y"),
        GAME_LABEL = paste0(format(GAME_DATE, "%m/%d/%Y"), " — ",
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
  
  # Fetch game details
  game_score <- reactive({
    req(input$game, input$season)
    
    game_id <- str_pad(input$game, width = 10, side = "left", pad = "0")
    
    nba_leaguegamelog(
      season      = input$season,
      season_type = "Regular Season"
    )$LeagueGameLog %>%
      filter(GAME_ID == game_id) %>%
      select(TEAM_ABBREVIATION, PTS)
  }) %>%
    bindCache(input$game, input$season)
  
  selected_player_name <- reactiveVal(NULL)
  
  observe({
    players <- nba_commonallplayers(
      league_id = "00",
      season    = input$season
    )$CommonAllPlayers %>%
      filter(ROSTERSTATUS == 1) %>%
      arrange(DISPLAY_FIRST_LAST)
    
    choices <- setNames(players$PERSON_ID, players$DISPLAY_FIRST_LAST)
    
    updateSelectizeInput(
      session, "player",
      choices  = choices,
      selected = NULL,
      server   = TRUE
    )
  })
  
  # Separately track the name when player selection changes
  observeEvent(input$player, {
    req(input$player)
    
    players <- nba_commonallplayers(
      league_id = "00",
      season    = input$season
    )$CommonAllPlayers
    
    name <- players %>%
      filter(PERSON_ID == input$player) %>%
      pull(DISPLAY_FIRST_LAST)
    
    selected_player_name(name)
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
    
    shots       <- shot_data()
    log         <- game_log()
    score       <- game_score()
    game_row    <- log[log$Game_ID == input$game, ]
    game_label  <- game_row$GAME_LABEL
    player_name <- selected_player_name()   
    matchup     <- game_row$MATCHUP  # e.g. "MIN @ BOS" or "MIN vs BOS"
    
    # LineScore rows are in matchup order — away team first for @ games
    team1 <- score$TEAM_ABBREVIATION[1]
    team2 <- score$TEAM_ABBREVIATION[2]
    pts1  <- score$PTS[1]
    pts2  <- score$PTS[2]
    
    # Map abbreviation to full city name
    name_map <- c(
      ATL = "Atlanta",    BOS = "Boston",     BKN = "Brooklyn",   CHA = "Charlotte",
      CHI = "Chicago",    CLE = "Cleveland",  DAL = "Dallas",     DEN = "Denver",
      DET = "Detroit",    GSW = "Golden State", HOU = "Houston",  IND = "Indiana",
      LAC = "LA Clippers", LAL = "LA Lakers", MEM = "Memphis",   MIA = "Miami",
      MIL = "Milwaukee",  MIN = "Minnesota",  NOP = "New Orleans", NYK = "New York",
      OKC = "Oklahoma City", ORL = "Orlando", PHI = "Philadelphia", PHX = "Phoenix",
      POR = "Portland",   SAC = "Sacramento", SAS = "San Antonio", TOR = "Toronto",
      UTA = "Utah",       WAS = "Washington"
    )
    
    selected_player_name <- reactiveVal(NULL)
    
    city1 <- name_map[team1]
    city2 <- name_map[team2]
    
    score_line <- paste0(name_map[team1], " ", pts1, " — ", name_map[team2], " ", pts2)
    
    ggplot(shots, aes(x = x_plot, y = y_plot)) +
      draw_court() +
      geom_point(
        data  = shots %>% filter(result == "Missed"),
        color = "#9ea2a2", size = 7.5, alpha = 0.7
      ) +
      geom_point(
        data  = shots %>% filter(result == "Made"),
        color = "#78BE1F", size = 7.5, alpha = 0.9
      ) +
      coord_fixed(
        xlim   = c(-COURT_W / 2, COURT_W / 2),
        ylim   = c(BASELINE_Y, HALFCOURT_Y),
        expand = FALSE
      ) +
      labs(
        title    = player_name,
        subtitle = score_line
      ) +
      theme_void() +
      theme(
        plot.background  = element_rect(fill = "#202938", color = NA),
        panel.background = element_rect(fill = "#202938", color = NA),
        plot.title       = element_text(color = "white", size = 24, face = "bold",
                                        hjust = 0.5, margin = margin(t = 20)),
        plot.subtitle    = element_text(color = "gray70", size = 18,
                                        hjust = 0.5, margin = margin(t = 4, b = 10)),
        plot.margin      = margin(t = 15, r = 0, b = 0, l = 0)
      )
  }, bg = "#202938")
  
  output$game_stats <- renderUI({
    req(input$game)
    
    log  <- game_log()
    
    # print to console so you can see exact values
    message("All columns: ", paste(names(log), collapse = ", "))
    
    game <- log[log$Game_ID == input$game, ]
    
    message("Game row: ", paste(unlist(game[1,]), collapse = ", "))
    
    stats <- list(
      "Minutes"   = game$MIN,
      "Points"    = game$PTS,
      "FGM / FGA" = paste0(game$FGM, " / ", game$FGA),
      "FG%"       = paste0(round(as.numeric(game$FG_PCT) * 100, 1), "%"),
      "3PM / 3PA" = paste0(game$FG3M, " / ", game$FG3A),
      "3P%"       = paste0(round(as.numeric(game$FG3_PCT) * 100, 1), "%"),
      "FTM / FTA" = paste0(game$FTM, " / ", game$FTA),
      "FT%"       = paste0(round(as.numeric(game$FT_PCT) * 100, 1), "%")
    )
    
    tagList(
      tags$div(
        style = "color: black; padding: 20px; margin-top: 60px;",
        tags$h4(
          style = "color: #ff9339; border-bottom: 1px solid #ff9339; 
                 padding-bottom: 8px; margin-bottom: 16px;",
          "Game Stats"
        ),
        tagList(lapply(names(stats), function(label) {
          tags$div(
            style = "display: flex; justify-content: space-between; 
                   padding: 8px 0; border-bottom: 1px solid #333;",
            tags$span(style = "color: gray;", label),
            tags$span(style = "font-weight: bold;", as.character(stats[[label]]))
          )
        }))
      )
    )
  })
  
  shot_data_season <- reactive({
    req(input$player, input$season)
    
    nba_shotchartdetail(
      player_id              = input$player,
      season                 = input$season,
      season_type            = "Regular Season",
      context_measure_simple = "FGA"
    )$Shot_Chart_Detail %>%
      mutate(
        LOC_X          = as.numeric(LOC_X),
        LOC_Y          = as.numeric(LOC_Y),
        SHOT_MADE_FLAG = as.numeric(SHOT_MADE_FLAG),
        x_plot         = LOC_X / 10,
        y_plot         = LOC_Y / 10 + BASKET_Y
      ) %>%
      filter(
        is.finite(x_plot),
        is.finite(y_plot),
        is.finite(SHOT_MADE_FLAG),
        x_plot >= -COURT_W / 2,
        x_plot <=  COURT_W / 2,
        y_plot >= BASELINE_Y,
        y_plot <= HALFCOURT_Y
      )
  }) %>%
    bindCache(input$player, input$season)
  
  # Render heatmap
  output$hex_chart <- renderPlot({
    req(nrow(shot_data_season()) > 0)
    
    shots  <- shot_data_season()
    log    <- game_log()
    player_name <- log$PLAYER_NAME[1]
    
    ggplot(shots, aes(x = x_plot, y = y_plot)) +
      draw_court() +
      stat_summary_hex(
        aes(z = SHOT_MADE_FLAG),
        fun   = mean,
        bins  = 30,
        alpha = 0.95
      ) +
      scale_fill_gradient(
        low      = "#8a6950",
        high     = "#ff9339",
        na.value = NA,
        name     = "FG%",
        labels   = scales::label_percent(accuracy = 1)
      ) +
      coord_fixed(
        xlim   = c(-COURT_W / 2 - 2, COURT_W / 2 + 2),
        ylim   = c(BASELINE_Y, HALFCOURT_Y),
        expand = FALSE
      ) +
      labs(
        title    = player_name,
        subtitle = paste0(input$season, " Regular Season")
      ) +
      theme_void() +
      theme(
        plot.background  = element_rect(fill = "#202938", color = NA),
        panel.background = element_rect(fill = "#202938", color = NA),
        plot.title       = element_text(color = "white", size = 30, face = "bold",
                                        hjust = 0.5, margin = margin(t = 10)),
        plot.subtitle    = element_text(color = "gray70", size = 26,
                                        hjust = 0.5, margin = margin(t = 4, b = 10)),
        plot.margin      = margin(t = 0, r = 0, b = 0, l = 0),
        legend.position = 'none'
        #legend.box.margin     = margin(0, 20, 0, 0),
        # legend.text           = element_text(color = "gray70", size = 16),
        # legend.title          = element_text(color = "gray70"),
        # legend.background     = element_rect(fill = "transparent", color = NA),
        # legend.box.background = element_rect(fill = "transparent", color = NA)
      )
  }, bg = "#202938")
  
}

shinyApp(ui, server)