# CORN YIELD PREDICTION - SHINY APP
# Team 10: Amit Godara & Taiwo Owolanke
# 2026 DSA Final Project

library(shiny)
library(tidyverse)
library(ggplot2)

# ── Load all data ─────────────────────────────────────────────
train_trait <- read_csv("training_trait.csv",
                        show_col_types = FALSE)

train_soil  <- read_csv("training_soil.csv",
                        show_col_types = FALSE) %>%
  mutate(site = str_remove(site, "_\\d{4}$")) %>%
  group_by(year, site) %>%
  summarise(soilpH    = mean(soilpH,    na.rm = TRUE),
            om_pct    = mean(om_pct,    na.rm = TRUE),
            soilk_ppm = mean(soilk_ppm, na.rm = TRUE),
            soilp_ppm = mean(soilp_ppm, na.rm = TRUE),
            .groups   = "drop")

weather <- read_csv("weather_season_summary.csv",
                    show_col_types = FALSE) %>%
  rename(site = site_base)

val_preds <- read_csv("xgb_internal_validation_predictions.csv",
                      show_col_types = FALSE)

val_metrics <- read_csv("xgb_internal_validation_metrics.csv",
                        show_col_types = FALSE)

# ── Pre-compute summaries ─────────────────────────────────────
yield_year <- train_trait %>%
  filter(!is.na(yield_mg_ha)) %>%
  group_by(year) %>%
  summarise(mean_yield = mean(yield_mg_ha),
            sd_yield   = sd(yield_mg_ha),
            .groups    = "drop")

soil_long <- train_soil %>%
  pivot_longer(cols      = c(soilpH, om_pct, soilk_ppm, soilp_ppm),
               names_to  = "variable",
               values_to = "value") %>%
  filter(!is.na(value))

weather_long <- weather %>%
  pivot_longer(cols      = c(tmean_season, precip_total,
                             gdd_total, heat_days),
               names_to  = "variable",
               values_to = "value")

rmse_val <- val_metrics %>%
  filter(.metric == "rmse") %>%
  pull(.estimate) %>% round(3)

r2_val <- val_metrics %>%
  filter(.metric == "rsq") %>%
  pull(.estimate) %>% round(3)

# ── UI ────────────────────────────────────────────────────────
ui <- fluidPage(
  
  tags$head(tags$style(HTML("
    body { background-color:#f4f6f9; font-family:Georgia,serif; }
    h2   { color:#2c5f2e; border-bottom:2px solid #2c5f2e;
           padding-bottom:8px; margin-bottom:20px; }
    .metric-box {
      background:#2c5f2e; color:white; border-radius:10px;
      padding:20px; text-align:center; margin:8px;
    }
    .metric-box h3 { font-size:2.2em; margin:0; color:#f8c200; }
    .metric-box p  { margin:0; font-size:1em; }
    .well { background:#fff; border:1px solid #dde; border-radius:8px; }
    .nav-tabs>li.active>a {
      background-color:#2c5f2e !important; color:white !important;
    }
    .nav-tabs>li>a { color:#2c5f2e; font-weight:bold; }
  "))),
  
  # Header
  div(style="background:#2c5f2e;color:white;padding:20px 30px;margin-bottom:20px;",
      h1("🌽 Corn Yield Prediction Dashboard",
         style="margin:0;font-size:1.8em;"),
      p("Team 10 | Amit Godara & Taiwo Owolanke | 2026 DSA Final Project",
        style="margin:5px 0 0 0;opacity:0.85;")
  ),
  
  # Metric boxes
  fluidRow(
    column(3, div(class="metric-box",
                  h3("164K"), p("Training Observations"))),
    column(3, div(class="metric-box",
                  h3("22"), p("Predictors Used"))),
    column(3, div(class="metric-box",
                  h3(r2_val), p("XGBoost Validation R²"))),
    column(3, div(class="metric-box",
                  h3(rmse_val), p("RMSE (Mg/ha)")))
  ),
  
  br(),
  
  tabsetPanel(
    
    # ── Tab 1: Yield EDA ────────────────────────────────────
    tabPanel("📊 Yield EDA",
             br(),
             fluidRow(
               column(3,
                      wellPanel(
                        h4("Controls", style="color:#2c5f2e;"),
                        selectInput("eda_type", "Plot type:",
                                    choices = c(
                                      "Mean Yield by Year" = "year",
                                      "Yield Distribution" = "dist",
                                      "Yield by Site"      = "site"
                                    )
                        ),
                        conditionalPanel("input.eda_type == 'year'",
                                         checkboxInput("show_sd", "Show error bars", TRUE)
                        ),
                        conditionalPanel("input.eda_type == 'dist'",
                                         sliderInput("bins", "Number of bins:", 20, 80, 40)
                        ),
                        conditionalPanel("input.eda_type == 'site'",
                                         selectInput("site_sel", "Select site:",
                                                     choices  = sort(unique(train_trait$site)),
                                                     selected = "DEH1"
                                         )
                        )
                      )
               ),
               column(9, plotOutput("yield_plot", height = "450px"))
             )
    ),
    
    # ── Tab 2: Soil EDA ─────────────────────────────────────
    tabPanel("🌱 Soil EDA",
             br(),
             fluidRow(
               column(3,
                      wellPanel(
                        h4("Controls", style="color:#2c5f2e;"),
                        selectInput("soil_var", "Soil variable:",
                                    choices = c(
                                      "Soil pH"             = "soilpH",
                                      "Organic Matter (%)"  = "om_pct",
                                      "Soil K (ppm)"        = "soilk_ppm",
                                      "Soil P (ppm)"        = "soilp_ppm"
                                    )
                        ),
                        radioButtons("soil_type", "Plot type:",
                                     choices = c("Histogram"       = "hist",
                                                 "Boxplot by Year" = "box")
                        )
                      )
               ),
               column(9, plotOutput("soil_plot", height = "450px"))
             )
    ),
    
    # ── Tab 3: Weather EDA ──────────────────────────────────
    tabPanel("🌤️ Weather EDA",
             br(),
             fluidRow(
               column(3,
                      wellPanel(
                        h4("Controls", style="color:#2c5f2e;"),
                        selectInput("wx_var", "Weather variable:",
                                    choices = c(
                                      "Mean Temperature (°C)"    = "tmean_season",
                                      "Total Precipitation (mm)" = "precip_total",
                                      "Growing Degree Days"      = "gdd_total",
                                      "Heat Stress Days (>35°C)" = "heat_days"
                                    )
                        ),
                        sliderInput("wx_years", "Year range:",
                                    min   = 2014, max   = 2024,
                                    value = c(2014, 2024), sep = ""
                        )
                      )
               ),
               column(9, plotOutput("wx_plot", height = "450px"))
             )
    ),
    
    # ── Tab 4: Variable Importance ──────────────────────────
    tabPanel("🔍 Variable Importance",
             br(),
             h2("XGBoost — Top Predictors of Corn Yield"),
             p("Variables ranked by contribution to the XGBoost model.",
               style = "color:#555;"),
             div(style = "text-align:center;",
                 imageOutput("vip_img", height = "500px")
             )
    ),
    
    # ── Tab 5: Model Performance ────────────────────────────
    tabPanel("📈 Model Performance",
             br(),
             h2("XGBoost — Predicted vs Observed Yield"),
             fluidRow(
               column(3,
                      wellPanel(
                        h4("Model Metrics", style="color:#2c5f2e;"),
                        br(),
                        div(style="text-align:center;",
                            p("Validation R²"),
                            h3(r2_val, style="color:#2c5f2e;margin:0;"),
                            hr(),
                            p("Validation RMSE"),
                            h3(paste0(rmse_val, " Mg/ha"),
                               style="color:#2c5f2e;margin:0;"),
                            hr(),
                            p("5-fold grouped CV by site-year",
                              style="color:#777;font-size:0.85em;")
                        )
                      )
               ),
               column(9, plotOutput("pred_plot", height = "480px"))
             )
    )
    
  ) # end tabsetPanel
) # end fluidPage


# ── SERVER ────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Yield plot
  output$yield_plot <- renderPlot({
    
    if (input$eda_type == "year") {
      p <- ggplot(yield_year,
                  aes(x = factor(year), y = mean_yield)) +
        geom_col(fill = "#2c5f2e", alpha = 0.85, width = 0.7) +
        geom_hline(yintercept = mean(yield_year$mean_yield),
                   linetype  = "dashed",
                   color     = "#f8c200",
                   linewidth = 1) +
        labs(title = "Mean Corn Yield by Year (2014-2023)",
             x = "Year", y = "Mean Yield (Mg/ha)") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(color="#2c5f2e", face="bold"))
      if (input$show_sd) {
        p <- p + geom_errorbar(
          aes(ymin = mean_yield - sd_yield,
              ymax = mean_yield + sd_yield),
          width = 0.3, color = "#333")
      }
      p
      
    } else if (input$eda_type == "dist") {
      train_trait %>%
        filter(!is.na(yield_mg_ha)) %>%
        ggplot(aes(x = yield_mg_ha)) +
        geom_histogram(bins  = input$bins,
                       fill  = "#2c5f2e",
                       color = "white",
                       alpha = 0.85) +
        geom_vline(
          xintercept = mean(train_trait$yield_mg_ha, na.rm = TRUE),
          color      = "#f8c200",
          linewidth  = 1.2,
          linetype   = "dashed") +
        labs(title = "Distribution of Corn Yield — Training Data",
             x = "Yield (Mg/ha)", y = "Count") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(color="#2c5f2e", face="bold"))
      
    } else {
      train_trait %>%
        filter(site == input$site_sel, !is.na(yield_mg_ha)) %>%
        ggplot(aes(x = factor(year), y = yield_mg_ha)) +
        geom_boxplot(fill  = "#2c5f2e",
                     alpha = 0.7,
                     color = "#1a3d1c") +
        labs(title = paste("Yield by Year at Site:", input$site_sel),
             x = "Year", y = "Yield (Mg/ha)") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(color="#2c5f2e", face="bold"))
    }
  })
  
  # Soil plot
  output$soil_plot <- renderPlot({
    label <- switch(input$soil_var,
                    soilpH    = "Soil pH",
                    om_pct    = "Organic Matter (%)",
                    soilk_ppm = "Soil K (ppm)",
                    soilp_ppm = "Soil P (ppm)"
    )
    dat <- soil_long %>% filter(variable == input$soil_var)
    
    if (input$soil_type == "hist") {
      ggplot(dat, aes(x = value)) +
        geom_histogram(bins  = 30,
                       fill  = "#5a8f3c",
                       color = "white",
                       alpha = 0.85) +
        labs(title = paste("Distribution of", label),
             x = label, y = "Count") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(color="#2c5f2e", face="bold"))
    } else {
      ggplot(dat, aes(x = factor(year), y = value)) +
        geom_boxplot(fill  = "#5a8f3c",
                     alpha = 0.7,
                     color = "#1a3d1c") +
        labs(title = paste(label, "by Year"),
             x = "Year", y = label) +
        theme_minimal(base_size = 14) +
        theme(plot.title  = element_text(color="#2c5f2e", face="bold"),
              axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Weather plot
  output$wx_plot <- renderPlot({
    label <- switch(input$wx_var,
                    tmean_season = "Mean Temperature (°C)",
                    precip_total = "Total Precipitation (mm)",
                    gdd_total    = "Growing Degree Days",
                    heat_days    = "Heat Stress Days (Tmax > 35°C)"
    )
    weather_long %>%
      filter(variable == input$wx_var,
             year     >= input$wx_years[1],
             year     <= input$wx_years[2]) %>%
      group_by(year) %>%
      summarise(mean_val = mean(value, na.rm = TRUE),
                .groups  = "drop") %>%
      ggplot(aes(x = year, y = mean_val)) +
      geom_line(color = "#2c5f2e", linewidth = 1.3) +
      geom_point(color = "#f8c200", size = 4) +
      labs(title = paste("Mean", label, "by Year"),
           x = "Year", y = label) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(color="#2c5f2e", face="bold"))
  })
  
  # Variable importance image
  output$vip_img <- renderImage({
    list(
      src   = "xgb_variable_importance.png",
      width = "75%",
      style = "display:block;margin:auto;"
    )
  }, deleteFile = FALSE)
  
  # Predicted vs observed
  output$pred_plot <- renderPlot({
    val_preds %>%
      ggplot(aes(x = yield_mg_ha, y = .pred)) +
      geom_point(alpha = 0.2, color = "#2c5f2e", size = 1) +
      geom_abline(color     = "red",
                  linewidth = 1,
                  linetype  = "dashed") +
      geom_smooth(method    = "lm",
                  color     = "#f8c200",
                  linewidth = 1.2,
                  se        = FALSE) +
      annotate("label",
               x          = 3,
               y          = 14,
               label      = paste0("R² = ", r2_val,
                                   "\nRMSE = ", rmse_val, " Mg/ha"),
               size       = 5,
               color      = "#2c5f2e",
               fontface   = "bold",
               fill       = "white",
               label.size = 0.5) +
      labs(title    = "XGBoost: Predicted vs Observed Corn Yield",
           subtitle = "Grouped 5-fold cross-validation by site-year",
           x = "Observed Yield (Mg/ha)",
           y = "Predicted Yield (Mg/ha)") +
      theme_minimal(base_size = 14) +
      theme(plot.title    = element_text(color="#2c5f2e", face="bold"),
            plot.subtitle = element_text(color="#555"))
  })
  
} # end server

shinyApp(ui, server)