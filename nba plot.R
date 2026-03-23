
library(hoopR)
library(tidyverse)
library(hexbin)


players <- nba_commonallplayers(
  is_only_current_season = 1,
  season = "2025-26"
)$CommonAllPlayers

players %>%
  filter(str_detect(DISPLAY_FIRST_LAST, regex("edwards", ignore_case = TRUE))) %>%
  select(PERSON_ID, DISPLAY_FIRST_LAST, TEAM_CITY, TEAM_NAME, TEAM_ID ,ROSTERSTATUS)

player_id <- players %>%
  filter(DISPLAY_FIRST_LAST == "Julius Randle") %>%
  select(PERSON_ID, TEAM_ID) %>%
  first()

gamelog <- nba_playergamelog(
  player_id = player_id$PERSON_ID,
  season = "2025-26",
  season_type = "Regular Season"
)$PlayerGameLog

game_id <- gamelog %>%
  filter(GAME_DATE == 'Mar 22, 2026') %>%
  pull(Game_ID) %>%
  first()

res <- nba_shotchartdetail(
  team_id = player_id$TEAM_ID,  
  player_id = player_id$PERSON_ID,
  season = "2025-26",
  season_type = "Regular Season",
  context_measure = "FGA"
)

shots <- res$Shot_Chart_Detail

shots_clean <- shots %>%
  mutate(
    LOC_X = as.numeric(LOC_X),
    LOC_Y = as.numeric(LOC_Y),
    made = as.numeric(SHOT_MADE_FLAG) == 1,
    result = if_else(made, "Made", "Missed")
  )

shots_game <- shots %>%
  filter(GAME_ID == game_id)

summary(shots_clean$LOC_X)
summary(shots_clean$LOC_Y)
table(shots_clean$result)


# Drawing the court

# ── Helper functions ────────────────────────────────────────────────────────

# Arc: returns a data.frame of (x, y) points along a circle arc

arc <- function(cx, cy, r, angle_from, angle_to, n = 300) {
  angles <- seq(angle_from, angle_to, length.out = n) * pi / 180
  data.frame(x = cx + r * cos(angles), y = cy + r * sin(angles))
}

# ── NBA dimensions (feet) ────────────────────────────────────────────────────
# Origin = center of basket
# Baseline at y = 0, half-court line at y = 47
# Basket sits 5.25 ft from baseline → y_basket = 5.25
# Backboard is 4 ft behind basket face → y_backboard = 5.25 - 4 = 1.25
# Free throw line: 18'10" from baseline = 18.833 ft
# Key: 16 ft wide (±8 from center), baseline to FT line

COURT_W      <- 50
HALFCOURT_Y  <- 47
BASELINE_Y   <- 0

BASKET_X     <- 0
BASKET_Y     <- 5.25      # basket center, 5.25 ft up from baseline
BACKBOARD_Y  <- 4      # 4 ft behind basket (toward baseline)
BACKBOARD_HW <- 3         # backboard half-width = 3 ft (6 ft total)

KEY_HW       <- 8         # key half-width (16 ft wide)
FT_LINE_Y    <- 18.833    # 18'10" from baseline

FT_R         <- 6         # free throw circle radius
RA_R         <- 4         # restricted area radius

THREE_R      <- 23.75     # 3-point arc radius from basket center
CORNER_X     <- 22        # 3 ft from sideline (25 - 3)
# y where arc meets corner straight: solve CORNER_X^2 + (y - BASKET_Y)^2 = THREE_R^2
CORNER_Y     <- BASKET_Y + sqrt(THREE_R^2 - CORNER_X^2)  # ≈ 9.93

# ── Arcs ─────────────────────────────────────────────────────────────────────

# Free throw circle
ft_top    <- arc(0, FT_LINE_Y, FT_R, 0, 180)       # solid (toward half-court)
ft_bottom <- arc(0, FT_LINE_Y, FT_R, 180, 360)     # dashed (into key)

# Restricted area arc (above basket, semicircle toward half-court)
ra_arc <- arc(BASKET_X, BASKET_Y, RA_R, 0, 180)

# Basket ring
ring <- arc(BASKET_X, BASKET_Y, 0.75, 0, 360)

# Three-point arc: from angle where arc meets corner lines
three_angle <- acos(CORNER_X / THREE_R) * 180 / pi   # ~22.6°
three_arc   <- arc(BASKET_X, BASKET_Y, THREE_R,
                   three_angle, 180 - three_angle)

# Key rectangle (baseline to FT line, ±8 ft wide)
key_rect <- data.frame(
  x = c(-KEY_HW, KEY_HW, KEY_HW, -KEY_HW, -KEY_HW),
  y = c(BASELINE_Y, BASELINE_Y, FT_LINE_Y, FT_LINE_Y, BASELINE_Y)
)

# Lane hash marks (NBA: 4 marks each side, measured from basket toward FT line)
# Positions at 7, 11, 14, and 17 ft from basket along y
hash_y <- BASKET_Y + c(3, 7, 11, 14)
hash_len <- 2  # 2 ft long each

hash_df <- do.call(rbind, lapply(hash_y, function(y) {
  data.frame(
    x    = c(-KEY_HW - hash_len, -KEY_HW,  KEY_HW,            KEY_HW + hash_len),
    xend = c(-KEY_HW,            -KEY_HW,  KEY_HW + hash_len, KEY_HW),
    y    = y, yend = y
  )
}))

shots_plot <- shots_clean %>%
  mutate(
    x_plot = LOC_X / 10,
    y_plot = LOC_Y / 10 + BASKET_Y
  )

# ── Plot ──────────────────────────────────────────────────────────────────────
draw_court <- function() {
  list(
    geom_rect(
      aes(xmin = -COURT_W/2, xmax = COURT_W/2,
          ymin = BASELINE_Y, ymax = HALFCOURT_Y),
      fill = NA, color = "gray70", linewidth = 0.8,
      inherit.aes = FALSE
    ),
    
    geom_path(
      data = key_rect,
      aes(x, y),
      color = "gray70",
      linewidth = 0.8,
      inherit.aes = FALSE
    ),
    
    geom_path(
      data = ft_top, aes(x, y),
      color = "gray70", linewidth = 0.8,
      inherit.aes = FALSE
    ),
    
    geom_path(
      data = ft_bottom, aes(x, y),
      color = "gray70", linewidth = 0.8, linetype = "dashed",
      inherit.aes = FALSE
    ),
    
    geom_path(
      data = three_arc, aes(x, y),
      color = "gray70", linewidth = 0.8,
      inherit.aes = FALSE
    ),
    
    geom_segment(
      aes(x = -CORNER_X, xend = -CORNER_X,
          y = BASELINE_Y, yend = CORNER_Y),
      color = "gray70", linewidth = 0.8,
      inherit.aes = FALSE
    ),
    
    geom_segment(
      aes(x = CORNER_X, xend = CORNER_X,
          y = BASELINE_Y, yend = CORNER_Y),
      color = "gray70", linewidth = 0.8,
      inherit.aes = FALSE
    ),
    
    geom_path(
      data = ra_arc, aes(x, y),
      color = "gray70", linewidth = 0.8,
      inherit.aes = FALSE
    ),
    
    geom_segment(
      aes(x = -BACKBOARD_HW, xend = BACKBOARD_HW,
          y = BACKBOARD_Y, yend = BACKBOARD_Y),
      color = "gray70", linewidth = 2,
      inherit.aes = FALSE
    ),
    
    geom_segment(
      aes(x = 0, xend = 0,
          y = BACKBOARD_Y, yend = BASKET_Y - 0.75),
      color = "gray70", linewidth = 1,
      inherit.aes = FALSE
    ),
    
    geom_path(
      data = ring, aes(x, y),
      color = "gray70", linewidth = 1,
      inherit.aes = FALSE
    )
  )
}




ggplot(shots_clean, aes(x = x_plot, y = y_plot)) +
  draw_court() +
  geom_point(
    data = shots_plot %>% filter(result == "Missed"),
    color = "#8a6950",
    size = 2,
    alpha = 0.4
  ) +
  geom_point(
    data = shots_plot %>% filter(result == "Made"),
    color = "#ff9339",
    size = 2.6,
    alpha = 0.9
  ) +
  coord_fixed(
    xlim = c(-COURT_W/2, COURT_W/2),
    ylim = c(BASELINE_Y, HALFCOURT_Y),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#202938", color = NA),
    panel.background = element_rect(fill = "#202938", color = NA),
    plot.margin = margin(0, 0, 0, 0)
  )

############ HEXBIN STYLE ############ 

shots_hex <- shots_clean %>%
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
    is.finite(SHOT_MADE_FLAG),
    x_plot >= -COURT_W/2,
    x_plot <=  COURT_W/2,
    y_plot >= BASELINE_Y,
    y_plot <= HALFCOURT_Y
  )

ggplot(shots_hex, aes(x = x_plot, y = y_plot)) +
  
  draw_court() +
  
  stat_summary_hex(
    aes(z = SHOT_MADE_FLAG),
    fun = mean,
    bins = 30,
    alpha = 0.95
  ) +
  
  scale_fill_gradient(
    low = "gray25",
    high = "#ff9339",
    na.value = NA,
    name = "FG%"
  ) +
  
  coord_fixed(
    xlim = c(-COURT_W/2, COURT_W/2),
    ylim = c(BASELINE_Y, HALFCOURT_Y),
    expand = FALSE
  ) +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#000000", color = NA),
    panel.background = element_rect(fill = "#000000", color = NA),
    plot.margin = margin(0, 0, 0, 0),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  )
