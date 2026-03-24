
library(shiny)
library(tidyverse)
library(hoopR)
library(scales)
library(cowplot)
library(magick)

# ── Court constants ────────────────────────────────────────────────────────────
COURT_W      <- 50
HALFCOURT_Y  <- 47
BASELINE_Y   <- 0
BASKET_X     <- 0
BASKET_Y     <- 5.25
BACKBOARD_Y  <- 4
BACKBOARD_HW <- 3
KEY_HW       <- 8
FT_LINE_Y    <- 18.833
FT_R         <- 6
RA_R         <- 4
THREE_R      <- 23.75
CORNER_X     <- 22
CORNER_Y     <- BASKET_Y + sqrt(THREE_R^2 - CORNER_X^2)

# ── Name map (defined globally so all reactives can see it) ───────────────────
name_map <- c(
  ATL = "Atlanta Hawks",        BOS = "Boston Celtics",
  BKN = "Brooklyn Nets",        CHA = "Charlotte Hornets",
  CHI = "Chicago Bulls",        CLE = "Cleveland Cavaliers",
  DAL = "Dallas Mavericks",     DEN = "Denver Nuggets",
  DET = "Detroit Pistons",      GSW = "Golden State Warriors",
  HOU = "Houston Rockets",      IND = "Indiana Pacers",
  LAC = "LA Clippers",          LAL = "Los Angeles Lakers",
  MEM = "Memphis Grizzlies",    MIA = "Miami Heat",
  MIL = "Milwaukee Bucks",      MIN = "Minnesota Timberwolves",
  NOP = "New Orleans Pelicans", NYK = "New York Knicks",
  OKC = "Oklahoma City Thunder",ORL = "Orlando Magic",
  PHI = "Philadelphia 76ers",   PHX = "Phoenix Suns",
  POR = "Portland Trail Blazers", SAC = "Sacramento Kings",
  SAS = "San Antonio Spurs",    TOR = "Toronto Raptors",
  UTA = "Utah Jazz",            WAS = "Washington Wizards"
)

# ── Display name map (city only, for titles) ───────────────────────────────────
display_map <- c(
  ATL = "Atlanta",       BOS = "Boston",        BKN = "Brooklyn",
  CHA = "Charlotte",     CHI = "Chicago",        CLE = "Cleveland",
  DAL = "Dallas",        DEN = "Denver",         DET = "Detroit",
  GSW = "Golden State",  HOU = "Houston",        IND = "Indiana",
  LAC = "LA Clippers",   LAL = "LA Lakers",      MEM = "Memphis",
  MIA = "Miami",         MIL = "Milwaukee",      MIN = "Minnesota",
  NOP = "New Orleans",   NYK = "New York",        OKC = "Oklahoma City",
  ORL = "Orlando",       PHI = "Philadelphia",    PHX = "Phoenix",
  POR = "Portland",      SAC = "Sacramento",      SAS = "San Antonio",
  TOR = "Toronto",       UTA = "Utah",            WAS = "Washington"
)

# ── Court helpers ──────────────────────────────────────────────────────────────
arc <- function(cx, cy, r, angle_from, angle_to, n = 300) {
  angles <- seq(angle_from, angle_to, length.out = n) * pi / 180
  data.frame(x = cx + r * cos(angles), y = cy + r * sin(angles))
}

ft_top      <- arc(0, FT_LINE_Y, FT_R, 0, 180)
ft_bottom   <- arc(0, FT_LINE_Y, FT_R, 180, 360)
ra_arc      <- arc(BASKET_X, BASKET_Y, RA_R, 0, 180)
ring        <- arc(BASKET_X, BASKET_Y, 0.75, 0, 360)
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
      ),
      selectInput(
        inputId = "team",
        label   = "Team (for Team Shot Chart)",
        choices = NULL
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        
        tabPanel("Player Shot Chart",
                 fluidRow(
                   column(8, plotOutput("shot_chart", height = "752px", width = "800px"),
                          downloadButton("save_shot_chart", "Save Image",
                                         style = "margin-top: 10px; background-color: #ff9339; 
                              border: none; color: black; font-weight: bold;")
                   ),
                   column(4, uiOutput("game_stats"), uiOutput("player_headshot"))
                 )
        ),
        
        tabPanel("Season Heatmap",
                 fluidRow(
                   column(12, plotOutput("hex_chart", height = "752px", width = "800px"))
                 )
        ),
        
        tabPanel("Team Shot Chart",
                 fluidRow(
                   column(12, plotOutput("team_shot_chart", height = "752px", width = "800px"))
                 )
        )
      )
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  selected_player_name <- reactiveVal(NULL)
  
  # ── Player list ──────────────────────────────────────────────────────────────
  observe({
    players <- nba_commonallplayers(
      league_id = "00",
      season    = input$season
    )$CommonAllPlayers %>%
      filter(ROSTERSTATUS == 1) %>%
      arrange(DISPLAY_FIRST_LAST)
    
    choices <- setNames(players$PERSON_ID, players$DISPLAY_FIRST_LAST)
    
    updateSelectizeInput(session, "player",
                         choices  = choices,
                         selected = NULL,
                         server   = TRUE)
  })
  
  # ── Track selected player name ───────────────────────────────────────────────
  observeEvent(input$player, {
    req(input$player)
    
    name <- nba_commonallplayers(
      league_id = "00",
      season    = input$season
    )$CommonAllPlayers %>%
      filter(PERSON_ID == input$player) %>%
      pull(DISPLAY_FIRST_LAST)
    
    selected_player_name(name)
  })
  
  # ── Headshot ─────────────────────────────────────────────────────────────────
  output$player_headshot <- renderUI({
    req(input$player)
    tags$img(
      src   = paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", input$player, ".png"),
      width = "100%",
      style = "border-radius: 8px; margin-top: 16px;"
    )
  })
  
  # ── Game log ─────────────────────────────────────────────────────────────────
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
  
  # ── Populate game dropdown ───────────────────────────────────────────────────
  observeEvent(game_log(), {
    log     <- game_log()
    choices <- setNames(log$Game_ID, log$GAME_LABEL)
    updateSelectInput(session, "game", choices = choices, selected = choices[1])
  })
  
  # ── Game score ───────────────────────────────────────────────────────────────
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
  
  # ── Populate team dropdown ───────────────────────────────────────────────────
  observeEvent(input$game, {
    req(input$game)
    
    score <- game_score()
    choices <- setNames(
      score$TEAM_ABBREVIATION,
      display_map[score$TEAM_ABBREVIATION]
    )
    updateSelectInput(session, "team", choices = choices, selected = choices[1])
  })
  
  # ── Individual game shot data ────────────────────────────────────────────────
  shot_data <- reactive({
    req(input$game)
    
    game_id <- str_pad(input$game, width = 10, side = "left", pad = "0")
    
    nba_shotchartdetail(
      player_id       = input$player,
      game_id         = game_id,
      season          = input$season,
      season_type     = "Regular Season",
      context_measure = "FGA"
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
  
  # ── Season shot data ─────────────────────────────────────────────────────────
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
        is.finite(x_plot), is.finite(y_plot), is.finite(SHOT_MADE_FLAG),
        x_plot >= -COURT_W / 2, x_plot <= COURT_W / 2,
        y_plot >= BASELINE_Y,   y_plot <= HALFCOURT_Y
      )
  }) %>%
    bindCache(input$player, input$season)
  
  # ── Team shot data ───────────────────────────────────────────────────────────
  team_shot_data <- reactive({
    req(input$game, input$team)
    
    game_id   <- str_pad(input$game, width = 10, side = "left", pad = "0")
    full_name <- name_map[input$team]   # e.g. "Minnesota Timberwolves"
    
    nba_shotchartdetail(
      player_id              = "0",
      game_id                = game_id,
      season                 = input$season,
      season_type            = "Regular Season",
      context_measure_simple = "FGA"
    )$Shot_Chart_Detail %>%
      filter(TEAM_NAME == full_name) %>%
      mutate(
        LOC_X  = as.numeric(LOC_X),
        LOC_Y  = as.numeric(LOC_Y),
        x_plot = LOC_X / 10,
        y_plot = LOC_Y / 10 + BASKET_Y,
        result = if_else(SHOT_MADE_FLAG == "1", "Made", "Missed")
      )
  }) %>%
    bindCache(input$game, input$team)
  
  # ── Plot builder functions ───────────────────────────────────────────────────

build_shot_chart <- function(shots, player_name, game_label, score_line) {
  ggplot(shots, aes(x = x_plot, y = y_plot)) +
    draw_court() +
    geom_point(data = shots %>% filter(result == "Missed"),
               color = "#9ea2a2", size = 7.5, alpha = 0.7) +
    geom_point(data = shots %>% filter(result == "Made"),
               color = "#78BE1F", size = 7.5, alpha = 0.9) +
    coord_fixed(xlim = c(-COURT_W/2, COURT_W/2),
                ylim = c(BASELINE_Y, HALFCOURT_Y), expand = FALSE) +
    labs(title = player_name, subtitle = paste0(game_label, "\n", score_line)) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "#202938", color = NA),
      panel.background = element_rect(fill = "#202938", color = NA),
      plot.title    = element_text(color = "white",  size = 24, face = "bold",
                                   hjust = 0.5, margin = margin(t = 20)),
      plot.subtitle = element_text(color = "gray70", size = 18,
                                   hjust = 0.5, margin = margin(t = 4, b = 10),
                                   lineheight = 1.4),
      plot.margin   = margin(t = 15, r = 0, b = 0, l = 0)
    )
}

build_hex_chart <- function(shots, player_name, season) {
  ggplot(shots, aes(x = x_plot, y = y_plot)) +
    draw_court() +
    stat_summary_hex(aes(z = SHOT_MADE_FLAG), fun = mean, bins = 30, alpha = 0.95) +
    scale_fill_gradient(low = "#8a6950", high = "#ff9339", na.value = NA,
                        name = "FG%", labels = scales::label_percent(accuracy = 1)) +
    coord_fixed(xlim = c(-COURT_W/2 - 2, COURT_W/2 + 2),
                ylim = c(BASELINE_Y, HALFCOURT_Y), expand = FALSE) +
    labs(title = player_name, subtitle = paste0(season, " Regular Season")) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "#202938", color = NA),
      panel.background = element_rect(fill = "#202938", color = NA),
      plot.title    = element_text(color = "white",  size = 30, face = "bold",
                                   hjust = 0.5, margin = margin(t = 10)),
      plot.subtitle = element_text(color = "gray70", size = 26,
                                   hjust = 0.5, margin = margin(t = 4, b = 10)),
      plot.margin   = margin(0, 0, 0, 0),
      legend.position = "none"
    )
}

build_team_shot_chart <- function(shots, team_name, game_label, score_line) {
  ggplot(shots, aes(x = x_plot, y = y_plot)) +
    draw_court() +
    geom_point(data = shots %>% filter(result == "Missed"),
               color = "#8a6950", size = 3, alpha = 0.5) +
    geom_point(data = shots %>% filter(result == "Made"),
               color = "#ff9339", size = 3.5, alpha = 0.9) +
    coord_fixed(xlim = c(-COURT_W/2, COURT_W/2),
                ylim = c(BASELINE_Y, HALFCOURT_Y), expand = FALSE) +
    labs(title = team_name, subtitle = paste0(game_label, "\n", score_line)) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "#202938", color = NA),
      panel.background = element_rect(fill = "#202938", color = NA),
      plot.title    = element_text(color = "white",  size = 28, face = "bold",
                                   hjust = 0.5, margin = margin(t = 20, b = 4)),
      plot.subtitle = element_text(color = "gray70", size = 18,
                                   hjust = 0.5, margin = margin(t = 4, b = 10),
                                   lineheight = 1.4),
      plot.margin   = margin(t = 15, r = 0, b = 0, l = 0)
    )
}
  
  
  
  # ── Render: game shot chart ──────────────────────────────────────────────────
output$shot_chart <- renderPlot({
  req(nrow(shot_data()) > 0)
  
  score      <- game_score()
  game_row   <- game_log()[game_log()$Game_ID == input$game, ]
  team1      <- score$TEAM_ABBREVIATION[1]
  team2      <- score$TEAM_ABBREVIATION[2]
  score_line <- paste0(display_map[team1], " ", score$PTS[1],
                       " — ", display_map[team2], " ", score$PTS[2])
  
  build_shot_chart(shot_data(), selected_player_name(),
                   game_row$GAME_LABEL, score_line)
}, bg = "#202938", width = 800, height = 752)

output$save_shot_chart <- downloadHandler(
  filename = function() {
    paste0(selected_player_name(), "_", input$game, "_shot_chart.png")
  },
  content = function(file) {
    score      <- game_score()
    log        <- game_log()
    game_row   <- log[log$Game_ID == input$game, ]
    team1      <- score$TEAM_ABBREVIATION[1]
    team2      <- score$TEAM_ABBREVIATION[2]
    score_line <- paste0(display_map[team1], " ", score$PTS[1],
                         " — ", display_map[team2], " ", score$PTS[2])
    game       <- game_row
    
    # ── Shot chart plot ──────────────────────────────────────────────────
    p_court <- build_shot_chart(shot_data(), selected_player_name(),
                                game_row$GAME_LABEL, score_line)
    
    # ── Stats table as ggplot ────────────────────────────────────────────
    stats_df <- data.frame(
      label = c("Minutes", "Points", "FGM / FGA", "FG%",
                "3PM / 3PA", "3P%", "FTM / FTA", "FT%"),
      value = c(
        as.character(game$MIN),
        as.character(game$PTS),
        paste0(game$FGM, " / ", game$FGA),
        paste0(round(as.numeric(game$FG_PCT) * 100, 1), "%"),
        paste0(game$FG3M, " / ", game$FG3A),
        paste0(round(as.numeric(game$FG3_PCT) * 100, 1), "%"),
        paste0(game$FTM, " / ", game$FTA),
        paste0(round(as.numeric(game$FT_PCT) * 100, 1), "%")
      )
    )
    
    p_stats <- ggplot(stats_df) +
      geom_rect(aes(xmin = 0, xmax = 1, ymin = seq(0, 7) / 8, ymax = seq(1, 8) / 8),
                fill = "#202938", color = "gray70") +
      geom_text(aes(x = 0.05, y = (seq(0.5, 7.5)) / 8, label = label),
                color = "gray60", hjust = 0, size = 5) +
      geom_text(aes(x = 0.95, y = (seq(0.5, 7.5)) / 8, label = value),
                color = "white", hjust = 1, size = 5, fontface = "bold") +
      annotate("text", x = 0.5, y = 1.06, label = "Game Stats",
               color = "#ff9339", size = 6, fontface = "bold", hjust = 0.5) +
      annotate("segment", x = 0, xend = 1, y = 1.02, yend = 1.02,
               color = "#ff9339", linewidth = 0.8) +
      scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 1.1), expand = c(0, 0)) +
      theme_void() +
      theme(
        plot.background  = element_rect(fill = "#202938", color = NA),
        panel.background = element_rect(fill = "#202938", color = NA),
        plot.margin      = margin(20, 10, 10, 10)
      )
    
    # ── Headshot via magick ──────────────────────────────────────────────
    headshot_url <- paste0(
      "https://cdn.nba.com/headshots/nba/latest/1040x760/",
      input$player, ".png"
    )
    
    headshot_img  <- tryCatch(
      image_read(headshot_url),
      error = function(e) NULL
    )
    
    if (!is.null(headshot_img)) {
      p_headshot <- ggdraw() +
        draw_image(headshot_img) +
        theme(plot.background = element_rect(fill = "#202938", color = NA),
              plot.margin     = margin(t = 36, r = 0, b = 0, l = 0))  # adjust t to taste
    } else {
      p_headshot <- ggplot() +
        theme_void() +
        theme(plot.background = element_rect(fill = "#202938", color = NA))
    }
    
    # ── Right panel: stats on top, headshot on bottom ────────────────────
    p_right <- plot_grid(
      p_stats,
      p_headshot,
      ncol    = 1,
      rel_heights = c(1.2, 1)
    )
    
    # ── Combine court + right panel ──────────────────────────────────────
    p_final <- plot_grid(
      p_court,
      p_right,
      ncol      = 2,
      rel_widths = c(2.2, 1)
    )
    
    ggsave(file, plot = p_final, width = 9, height = 9,
           dpi = 300, bg = "#202938")
  }
)
  
  # ── Render: game stats ───────────────────────────────────────────────────────
  output$game_stats <- renderUI({
    req(input$game)
    
    game <- game_log()[game_log()$Game_ID == input$game, ]
    
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
    
    tagList(tags$div(
      style = "padding: 20px; margin-top: 60px;",
      tags$h4(style = "color: #000000; border-bottom: 1px solid #ff9339;
                        padding-bottom: 8px; margin-bottom: 16px;", "Game Stats"),
      tagList(lapply(names(stats), function(label) {
        tags$div(
          style = "display: flex; justify-content: space-between;
                   padding: 8px 0; border-bottom: 1px solid #333;",
          tags$span(style = "color: gray;", label),
          tags$span(style = "color: black; font-weight: bold;", as.character(stats[[label]]))
        )
      }))
    ))
  })
  
  # ── Render: season heatmap ───────────────────────────────────────────────────
  output$hex_chart <- renderPlot({
    req(nrow(shot_data_season()) > 0)
    
    shots       <- shot_data_season()
    player_name <- selected_player_name()
    
    ggplot(shots, aes(x = x_plot, y = y_plot)) +
      draw_court() +
      stat_summary_hex(aes(z = SHOT_MADE_FLAG), fun = mean, bins = 30, alpha = 0.95) +
      scale_fill_gradient(low = "#8a6950", high = "#ff9339", na.value = NA,
                          name = "FG%", labels = scales::label_percent(accuracy = 1)) +
      coord_fixed(xlim = c(-COURT_W/2 - 2, COURT_W/2 + 2),
                  ylim = c(BASELINE_Y, HALFCOURT_Y), expand = FALSE) +
      labs(title = player_name, subtitle = paste0(input$season, " Regular Season")) +
      theme_void() +
      theme(
        plot.background  = element_rect(fill = "#202938", color = NA),
        panel.background = element_rect(fill = "#202938", color = NA),
        plot.title    = element_text(color = "white",  size = 30, face = "bold",
                                     hjust = 0.5, margin = margin(t = 10)),
        plot.subtitle = element_text(color = "gray70", size = 26,
                                     hjust = 0.5, margin = margin(t = 4, b = 10)),
        plot.margin   = margin(0, 0, 0, 0),
        legend.position = "none"
      )
  }, bg = "#202938", width = 800, height = 752)
  
  # ── Render: team shot chart ──────────────────────────────────────────────────
  output$team_shot_chart <- renderPlot({
    req(nrow(team_shot_data()) > 0)
    
    shots      <- team_shot_data()
    log        <- game_log()
    score      <- game_score()
    game_row   <- log[log$Game_ID == input$game, ]
    game_label <- game_row$GAME_LABEL
    
    team1      <- score$TEAM_ABBREVIATION[1]
    team2      <- score$TEAM_ABBREVIATION[2]
    score_line <- paste0(display_map[team1],
                         " ",
                         score$PTS[1],
                         " — ",
                         display_map[team2],
                         " ",
                         score$PTS[2])
    team_name  <- display_map[input$team]
    
    ggplot(shots, aes(x = x_plot, y = y_plot)) +
      draw_court() +
      geom_point(
        data = shots %>% filter(result == "Missed"),
        color = "#9ea2a2",
        size = 5,
        alpha = 0.5
      ) +
      geom_point(
        data = shots %>% filter(result == "Made"),
        color = "#78BE1F",
        size = 7.5,
        alpha = 0.9
      ) +
      coord_fixed(
        xlim = c(-COURT_W / 2, COURT_W / 2),
        ylim = c(BASELINE_Y, HALFCOURT_Y),
        expand = FALSE
      ) +
      labs(
        title = paste0(team_name, " Team Shot Chart"),
        subtitle = paste0(game_label, "\n", score_line)
      ) +
      theme_void() +
      theme(
        plot.background  = element_rect(fill = "#202938", color = NA),
        panel.background = element_rect(fill = "#202938", color = NA),
        plot.title    = element_text(
          color = "white",
          size = 28,
          face = "bold",
          hjust = 0.5,
          margin = margin(t = 20, b = 4)
        ),
        plot.subtitle = element_text(
          color = "gray70",
          size = 18,
          hjust = 0.5,
          margin = margin(t = 4, b = 10),
          lineheight = 1.4
        ),
        plot.margin   = margin(
          t = 15,
          r = 0,
          b = 0,
          l = 0
        )
      )
  }, bg = "#202938", width = 800, height = 752)
  
}

shinyApp(ui, server)