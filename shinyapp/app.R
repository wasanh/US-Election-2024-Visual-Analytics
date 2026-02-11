# ==============================================================================
# 2024 U.S. Election Dashboard — FINAL REVISED & FIXED BUILD
# ==============================================================================

library(shiny)
library(tidyverse)
library(scales)
library(ggrepel)
library(plotly)
library(usmap)

# ===================== 1. GLOBAL DATA PROCESSING =====================

if (file.exists("clean_election_data.csv")) {
  df_main <- read_csv("clean_election_data.csv") %>%
    mutate(
      Trump_Margin = (Votes_Trump - Votes_Harris) / Votes_Tot,
      Trump_Margin_Pct = Trump_Margin, 
      fips = str_pad(FIPS, 5, side = "left", pad = "0"),
      Income_Decile = factor(ntile(Median_Income, 10)),
      Edu_Bin_Labels = cut_number(Pct_Bachelors_Plus, n = 4, labels = c("Low Edu", "Mid-Low", "Mid-High", "High Edu")),
      White_Bin = cut_width(Pct_White_Census, width = 0.1, center = 0.05),
      Edu_Tier = cut_number(Pct_Bachelors_Plus, n = 3, labels = c("Low Education", "Middle Education", "High Education")),
      College_Bin = cut_number(Pct_Bachelors_Plus, n = 3, labels = c("Low", "Medium", "High")),
      Income_Tier = cut_number(Median_Income, n = 3, labels = c("Low Income", "Middle Income", "High Income")),
      Category_ID = case_when(
        Winner_2020 %in% c("Biden","Harris") & Winner_2024 == "Trump" ~ "Flipped Red",
        Winner_2020 %in% c("Biden","Harris") & Winner_2024 == "Harris" ~ "Always Blue",
        Winner_2020 == "Trump" & Winner_2024 == "Trump" ~ "Always Trump",
        TRUE ~ "Other"
      )
    )
  
  df_story <- df_main %>% filter(Category_ID != "Other")
  
} else {
  stop("ERROR: Required data file 'clean_election_data.csv' not found.")
}

final_theme <- theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
        legend.position = "none", panel.grid.minor = element_blank())

edu_colors <- c("Low Education" = "#e67e22", "Middle Education" = "#95a5a6", "High Education" = "#8e44ad")
inc_colors <- c("Low Income" = "#e67e22", "Middle Income" = "#95a5a6", "High Income" = "#8e44ad")
edu_colors_4tier <- c("Low Edu" = "#c0392b", "Mid-Low" = "#e67e22", "Mid-High" = "#f1c40f", "High Edu" = "#2980b9")
flip_colors <- c("Always Blue" = "#0015BC", "Always Trump" = "#DE0100", "Flipped Red" = "#FFD700")
viz9_colors <- c("Low" = "#E69F00", "Medium" = "#56B4E9", "High" = "#009E73") 

state_df_full <- df_main %>%
  group_by(STATE_NAME, STATE_ABBR) %>%
  summarise(
    Total_Votes = sum(Votes_Tot, na.rm = TRUE),
    Margin = (sum(Votes_Trump) - sum(Votes_Harris)) / Total_Votes,
    Winner = ifelse(Margin > 0, "Trump", "Harris"),
    Mean_Pct_White_Census = mean(Pct_White_Census, na.rm = TRUE),
    .groups = 'drop'
  )
national_white_avg <- mean(state_df_full$Mean_Pct_White_Census, na.rm = TRUE)

# ===================== 2. UI DEFINITION =====================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background:#F4F7FB; font-family: 'Segoe UI', sans-serif; }
      .header { background:#2C3E50; color:white; padding:20px 40px; border-radius:0 0 15px 15px; margin-bottom:20px; box-shadow: 0 4px 10px rgba(0,0,0,0.1); }
      .sidebar { background:#32475c !important; color:white; min-height:100vh; padding:20px; border-radius:12px; }
      .sidebar label, .sidebar .radio span, .sidebar h3 { color:white !important; }
      .card { background:white; border-radius:12px; padding:20px; box-shadow:0 4px 12px rgba(0,0,0,.05); margin-bottom:25px; }
      .btn-light { background: rgba(255,255,255,0.2); border:none; color:white; }
      hr { display:none; }
    "))
  ),
  
  div(class="header", h2("2024 U.S. Election Analytical Suite")),
  
  fluidRow(
    uiOutput("sidebar_ui"), 
    uiOutput("main_panel_ui") 
  )
)

# ===================== 3. SERVER LOGIC =====================

server <- function(input, output, session) {
  
  sidebar_open <- reactiveVal(TRUE)
  observeEvent(input$toggle_sidebar, { sidebar_open(!sidebar_open()) })
  
  output$sidebar_ui <- renderUI({
    if (sidebar_open()) {
      column(3, div(class = "sidebar",
                    div(style = "display: flex; justify-content: space-between; align-items: center;",
                        h3(style="margin:0; font-size:1.2em;", "Navigation"),
                        actionButton("toggle_sidebar", "", icon("chevron-left"), class = "btn-sm btn-light")
                    ),
                    br(),
                    selectInput("category", "Analysis Framework:",
                                choices = c("National Baseline" = "base", 
                                            "Socio-Demographic Patterns" = "trends", 
                                            "Electoral Outliers" = "outliers", 
                                            "Voter Realignment" = "shift")),
                    br()
      ))
    } else {
      column(1, actionButton("toggle_sidebar", "", icon("chevron-right"), class = "btn-block btn-primary", style="margin-top:10px;"))
    }
  })
  
  output$main_panel_ui <- renderUI({
    panel_width <- if (sidebar_open()) 9 else 11
    
    content <- switch(input$category,
                      "base" = tabsetPanel(
                        tabPanel("Map", plotOutput("plot_map", height = "600px")),
                        tabPanel("Popular Vote", plotOutput("plot_result", height = "600px")),
                        tabPanel("Scoreboard", plotOutput("plot_scoreboard", height = "800px")),
                        tabPanel("Vote Weight", plotOutput("plot_spectrum", height = "600px"))
                      ),
                      "trends" = tabsetPanel(
                        tabPanel("Income Deciles", plotOutput("plot_money", height = "600px")),
                        tabPanel("Race Composition", plotOutput("plot_race", height = "600px")),
                        tabPanel("Education Density", plotOutput("plot_edu", height = "600px")),
                        tabPanel("Class Reversal", plotOutput("plot_trend1", height = "600px")),
                        tabPanel("Credentialing Threshold", plotOutput("plot_trend3", height = "600px"))
                      ),
                      "outliers" = tabsetPanel(
                        tabPanel("State Quadrant Analysis", plotOutput("plot_state_deviation", height = "700px")),
                        tabPanel("High-Education Conservative Break", 
                                 div(style = "text-align: center;",
                                     tags$img(src = "11_High_Ed_Conservative_Break.png", 
                                              width = "100%", 
                                              style = "max-width: 800px; height: auto;")
                                 )
                        ),                   
                        tabPanel("Regional Exceptions", plotOutput("plot_state_exception", height = "800px"))
                      ),
                      "shift" = tabsetPanel(
                        tabPanel("Top Red Flips", plotOutput("plot_top_flips", height = "600px")),
                        tabPanel("Hispanic Shift", plotOutput("plot_hispanic", height = "600px")),
                        tabPanel("Suburban Collapse", plotOutput("plot_urban_flips", height = "600px")),
                        tabPanel("Cultural Breakpoints & Outliers", plotOutput("plot_multicultural", height = "600px")), 
                        tabPanel("Education Dam", plotOutput("plot_edu_dam", height = "600px"))
                      )
    )
    
    column(panel_width, div(class = "card", content))
  })
  
  # --- RENDER FUNCTIONS ---
  output$plot_map <- renderPlot({
    plot_usmap(data = df_main, values = "Trump_Margin_Pct", color = "gray80") +
      scale_fill_gradient2(high = "#c0392b", mid = "white", low = "#2980b9", midpoint = 0, labels = percent) +
      labs(title = "The Geographic Divide") + final_theme
  })
  
  output$plot_result <- renderPlot({
    res <- df_main %>% summarise(Trump = sum(Votes_Trump), Harris = sum(Votes_Harris), Tot = sum(Votes_Tot)) %>%
      pivot_longer(1:2) %>% mutate(Pct = value/Tot)
    ggplot(res, aes(name, Pct, fill=name)) + 
      geom_col() + scale_fill_manual(values=c("Harris"="#2980b9","Trump"="#c0392b")) + 
      labs(title = "National Popular Vote Result") + final_theme
  })
  
  output$plot_scoreboard <- renderPlot({
    ggplot(state_df_full, aes(x = reorder(STATE_NAME, Margin), y = Margin, color = Winner)) +
      geom_segment(aes(xend = STATE_NAME, yend = 0)) + geom_point(size = 3) + coord_flip() +
      scale_color_manual(values = c("Harris" = "#2980b9", "Trump" = "#c0392b")) + 
      labs(title = "State Scoreboard: Margin Ranking") + final_theme
  })
  
  output$plot_spectrum <- renderPlot({
    ggplot(state_df_full, aes(x = Margin, y = 0, size = Total_Votes, color = Winner)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
      geom_point(alpha = 0.7) +
      scale_color_manual(values = c("Harris" = "#2980b9", "Trump" = "#c0392b")) + 
      scale_size_continuous(range = c(5, 25)) +
      scale_x_continuous(labels = percent_format(), limits = c(-0.5, 0.5)) + 
      scale_y_continuous(limits = c(-0.05, 0.05)) +
      labs(title = "State Vote Weight", x = "Vote Margin", y = "") + final_theme
  })
  
  output$plot_money <- renderPlot({
    weighted <- df_main %>% group_by(Income_Decile) %>% summarise(W = weighted.mean(Trump_Margin, Votes_Tot))
    ggplot(df_main, aes(Income_Decile, Trump_Margin)) + geom_boxplot(fill="gray95") +
      geom_line(data=weighted, aes(y=W, group=1), color="#f1c40f", linewidth=2) + 
      labs(title = "Trump Margin by Income Decile") + final_theme
  })
  
  output$plot_race <- renderPlot({
    race <- df_main %>% group_by(White_Bin) %>% summarise(W = weighted.mean(Trump_Margin, Votes_Tot))
    ggplot(race, aes(White_Bin, W)) + geom_line(group=1) + geom_point(color="#c0392b", size=4) + 
      labs(title = "Vote Trend by White Population %") + final_theme
  })
  
  output$plot_edu <- renderPlot({
    ggplot(df_main, aes(Trump_Margin, fill=Edu_Bin_Labels, weight=Votes_Tot)) +
      geom_density(alpha=0.7) + facet_wrap(~Edu_Bin_Labels, ncol=1) +
      scale_fill_manual(values=edu_colors_4tier) + 
      labs(title = "Polarization by Education Level") + final_theme
  })
  
  output$plot_trend1 <- renderPlot({
    ggplot(df_main %>% filter(!is.na(Median_Income), !is.na(Edu_Tier)), aes(x = Median_Income, y = Trump_Margin, color = Edu_Tier)) +
      geom_smooth(method = "loess", se = FALSE, linewidth = 2.5) + geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      scale_x_continuous(labels = dollar_format()) + scale_y_continuous(labels = percent_format()) +
      scale_color_manual(values = edu_colors) + labs(title = "Education-Income Reversal", x = "Median Income", y = "Trump Margin") +
      final_theme + theme(legend.position = "top")
  })
  
  output$plot_trend3 <- renderPlot({
    ggplot(df_main %>% filter(!is.na(Pct_White_Census), !is.na(Edu_Tier)), aes(x = Pct_White_Census, y = Trump_Margin, color = Edu_Tier)) +
      geom_smooth(method = "loess", se = FALSE, linewidth = 2.5) + geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      scale_x_continuous(labels = percent_format()) + scale_y_continuous(labels = percent_format()) +
      scale_color_manual(values = edu_colors) + labs(title = "Credentialing Threshold: Education vs. Race", x = "White Pop %", y = "Trump Margin") +
      final_theme + theme(legend.position = "top")
  })
  
  output$plot_state_deviation <- renderPlot({
    ggplot(state_df_full, aes(x = Mean_Pct_White_Census, y = Margin)) +
      geom_hline(yintercept = 0) + geom_vline(xintercept = national_white_avg, linetype = "dashed") +
      geom_point(aes(color = Margin), size = 4) + geom_text_repel(aes(label = STATE_ABBR)) +
      scale_color_gradient2(low = "#2980b9", mid = "gray80", high = "#c0392b", midpoint = 0) +
      labs(title = "State Quadrant Analysis", x = "Mean Pct White", y = "State Margin") + final_theme
  })
  
  output$plot_multicultural <- renderPlot({
    ggplot(df_story, aes(x = Pct_Bachelors_Plus, y = Median_Income, color = Category_ID)) +
      geom_point(alpha = 0.3) + 
      geom_point(data = filter(df_story, Category_ID == "Flipped Red"), size = 3, alpha = 0.8) +
      scale_color_manual(values = flip_colors) + 
      labs(title = "Cultural Breakpoints & Outliers", x = "% Bachelors+", y = "Median Income") + final_theme
  })
  
  output$plot_state_exception <- renderPlot({
    ggplot(df_main %>% filter(STATE_NAME %in% c("Texas", "Georgia", "Kentucky", "Virginia")), aes(x = Median_Income, y = Trump_Margin)) +
      geom_point(alpha = 0.5, aes(color = College_Bin)) + 
      geom_smooth(method = "loess", se = FALSE, aes(color = College_Bin)) + 
      facet_grid(STATE_NAME ~ College_Bin, scales = "free") +
      scale_color_manual(values = viz9_colors) + 
      labs(title = "Regional Income Outliers", x = "Income", y = "Trump Margin") + final_theme
  })
  
  output$plot_top_flips <- renderPlot({
    state_flips <- df_story %>% filter(Category_ID == "Flipped Red") %>% 
      count(STATE_NAME) %>% arrange(desc(n)) %>% slice_max(n, n = 10)
    ggplot(state_flips, aes(x = reorder(STATE_NAME, n), y = n)) +
      geom_segment(aes(xend = STATE_NAME, yend = 0), color = "grey") + 
      geom_point(size = 4, color = "#DE0100") +
      coord_flip() + labs(title = "85 Flipped Counties Span 31 States") + final_theme
  })
  
  output$plot_hispanic <- renderPlot({
    ggplot(df_story, aes(x = Pct_Hispanic_Census, fill = Category_ID)) + 
      geom_density(alpha = 0.5) + 
      scale_fill_manual(values = flip_colors) + 
      labs(title = "The Hispanic Realignment") + final_theme
  })
  
  output$plot_urban_flips <- renderPlot({
    df_story %>% filter(Category_ID == "Flipped Red") %>% count(Urban_Proxy) %>%
      mutate(perc = case_when(Urban_Proxy == "Urban/High" ~ "55%", Urban_Proxy == "Suburban/Mid" ~ "33%", TRUE ~ "12%")) %>%
      ggplot(aes(x = Urban_Proxy, y = n, fill = Urban_Proxy)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = perc), vjust = -1, size = 5, fontface = "bold") + 
      scale_y_continuous(limits = c(0, 55)) + 
      labs(title = "Suburban Collapse: 88% of Flips were Non-Rural") + final_theme
  })
  
  output$plot_edu_dam <- renderPlot({
    ggplot(df_story, aes(x = Pct_Bachelors_Plus, fill = Category_ID)) +
      geom_density(alpha = 0.5) +
      geom_vline(xintercept = 0.35, linetype = "dashed", color = "black") +
      scale_fill_manual(values = flip_colors) +
      annotate("text", x = 0.45, y = 5, label = "The 'Safe' Zone", fontface = "italic") +
      labs(title = "The Education Dam") + final_theme
  })
}

shinyApp(ui, server)
