# =============================================================
# CORN YIELD PREDICTION - SHINY APP
# Team 10: Amit Godara & Taiwo Owolanke
# 2026 DSA CRSS 8030 Final Project
# =============================================================

library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(scales)
library(stringr)

# ── Helper functions ──────────────────────────────────────────
metric_card <- function(title, value, subtitle, color = "#0f766e") {
  div(
    class = "metric-card",
    style = paste0("--card-accent:", color, ";"),
    div(class = "metric-label",    title),
    div(class = "metric-value",    value),
    div(class = "metric-subtitle", subtitle)
  )
}

panel_card <- function(title, subtitle = NULL, ...) {
  div(
    class = "panel-card",
    div(class = "panel-title", title),
    if (!is.null(subtitle)) div(class = "panel-subtitle", subtitle),
    ...
  )
}

plotly_theme <- function(p) {
  ggplotly(p, tooltip = c("x", "y")) %>%
    layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font          = list(color = "#0f172a"),
      margin        = list(l = 60, r = 20, b = 55, t = 20)
    )
}

# ── Load data — all files in same folder as app.R ─────────────
train_df <- read_csv("train_df.csv",
                     show_col_types = FALSE)

preds_xgb <- read_csv("testing_submission_filled_xgb_2_FINAL.csv",
                      show_col_types = FALSE)

model_summary <- read_csv("model_comparison_summary.csv",
                          show_col_types = FALSE)

val_preds <- read_csv("xgb_internal_validation_predictions.csv",
                      show_col_types = FALSE)

val_metrics <- read_csv("xgb_internal_validation_metrics.csv",
                        show_col_types = FALSE)

# ── Prepare data ──────────────────────────────────────────────
num_cols <- c(
  "yield_mg_ha", "soilpH", "om_pct", "soilk_ppm", "soilp_ppm",
  "tmean_season", "tmax_mean", "tmin_mean", "heat_days",
  "gdd_total", "precip_total", "srad_mean", "longitude", "latitude"
)

train_df  <- train_df  %>% mutate(across(any_of(num_cols), as.numeric))
preds_xgb <- preds_xgb %>% mutate(yield_mg_ha = as.numeric(yield_mg_ha))
val_preds <- val_preds  %>% mutate(across(any_of(c("yield_mg_ha", ".pred")), as.numeric))

# Use site_clean if it exists, otherwise use site
if (!"site_clean" %in% names(train_df)) {
  train_df <- train_df %>% mutate(site_clean = site)
}

soil_vars    <- intersect(c("soilpH","om_pct","soilk_ppm","soilp_ppm"), names(train_df))
weather_vars <- intersect(c("tmean_season","tmax_mean","tmin_mean",
                            "heat_days","gdd_total","precip_total","srad_mean"),
                          names(train_df))

weather_pretty <- c(
  tmean_season = "Mean Temperature (°C)",
  tmax_mean    = "Max Temperature (°C)",
  tmin_mean    = "Min Temperature (°C)",
  heat_days    = "Heat Stress Days (>35°C)",
  gdd_total    = "Growing Degree Days",
  precip_total = "Total Precipitation (mm)",
  srad_mean    = "Solar Radiation"
)

soil_pretty <- c(
  soilpH    = "Soil pH",
  om_pct    = "Organic Matter (%)",
  soilk_ppm = "Soil K (ppm)",
  soilp_ppm = "Soil P (ppm)"
)

sites_train <- sort(unique(train_df$site_clean))
sites_pred  <- sort(unique(preds_xgb$site))
years_train <- sort(unique(train_df$year))

# ── Pull metrics ──────────────────────────────────────────────
xgb_rmse    <- model_summary %>% filter(model=="XGBoost",.metric=="rmse") %>% pull(mean) %>% round(3)
xgb_rsq     <- model_summary %>% filter(model=="XGBoost",.metric=="rsq")  %>% pull(mean) %>% round(3)
glmnet_rmse <- model_summary %>% filter(model=="GLMNET", .metric=="rmse") %>% pull(mean) %>% round(3)
glmnet_rsq  <- model_summary %>% filter(model=="GLMNET", .metric=="rsq")  %>% pull(mean) %>% round(3)

rmse_val <- val_metrics %>% filter(.metric=="rmse") %>% pull(.estimate) %>% round(3)
r2_val   <- val_metrics %>% filter(.metric=="rsq")  %>% pull(.estimate) %>% round(3)

# ── UI ────────────────────────────────────────────────────────
ui <- page_navbar(
  title    = div(class="brand-wrap", span(class="brand-dot"), span("Corn Yield Dashboard")),
  position = "fixed-top",
  theme = bs_theme(
    version      = 5,
    primary      = "#0f766e",
    secondary    = "#334155",
    bg           = "#f8fafc",
    fg           = "#0f172a",
    base_font    = font_google("Inter"),
    heading_font = font_google("Poppins")
  ),
  header = tags$head(tags$style(HTML("
    body { padding-top:78px; background:linear-gradient(180deg,#f8fafc 0%,#eef4f8 100%); }
    .navbar {
      background:rgba(255,255,255,0.9)!important;
      backdrop-filter:blur(14px);
      border-bottom:1px solid #dbe4ee;
      box-shadow:0 4px 16px rgba(15,23,42,0.06);
    }
    .navbar-brand,.navbar-nav .nav-link { color:#0f172a!important; font-weight:700; }
    .navbar-nav .nav-link.active { color:#0f766e!important; box-shadow:inset 0 -2px 0 #0f766e; }
    .brand-wrap { display:flex; align-items:center; gap:10px; font-weight:800; }
    .brand-dot  { width:12px; height:12px; border-radius:50%;
                  background:linear-gradient(135deg,#0f766e,#2563eb);
                  box-shadow:0 0 0 5px rgba(15,118,110,0.12); }
    .hero-wrap  {
      background:linear-gradient(135deg,#0f172a 0%,#134e4a 100%);
      color:white; padding:36px; border-radius:28px;
      margin:20px 0 26px 0; box-shadow:0 20px 50px rgba(15,23,42,0.22);
    }
    .hero-title    { font-size:2.2rem; font-weight:800; margin-bottom:8px; }
    .hero-subtitle { font-size:1rem; opacity:0.9; line-height:1.7; }
    .metric-card   {
      background:linear-gradient(135deg,var(--card-accent),#0f172a);
      border-radius:20px; padding:22px; color:white;
      box-shadow:0 12px 30px rgba(15,23,42,0.16); height:100%;
    }
    .metric-label    { font-size:0.9rem; opacity:0.8; margin-bottom:6px; }
    .metric-value    { font-size:2rem; font-weight:800; line-height:1.1; }
    .metric-subtitle { margin-top:8px; font-size:0.88rem; opacity:0.8; }
    .panel-card {
      background:rgba(255,255,255,0.9);
      border:1px solid rgba(219,228,238,0.7);
      border-radius:20px; padding:20px;
      box-shadow:0 8px 24px rgba(15,23,42,0.07);
      margin-bottom:20px;
    }
    .panel-title    { font-size:1.1rem; font-weight:800; color:#0f172a; margin-bottom:4px; }
    .panel-subtitle { font-size:0.9rem; color:#64748b; margin-bottom:14px; line-height:1.6; }
    .app-img { width:100%; border-radius:14px; border:1px solid #e2e8f0; }
  "))),
  
  # Tab 1 — Overview
  nav_panel("🏠 Overview",
            div(class="container-fluid",
                div(class="hero-wrap",
                    div(class="hero-title", "🌽 Corn Yield Prediction Dashboard"),
                    div(class="hero-subtitle",
                        "Team 10 | Amit Godara & Taiwo Owolanke | 2026 DSA Final Project | UGA")
                ),
                layout_columns(
                  col_widths = c(2,2,2,2,2,2),
                  metric_card("Training Obs.",  "164K",      "Rows of training data",          "#2563ab"),
                  metric_card("Predictors",     "22",         "Features used in model",         "#0f766e"),
                  metric_card("Best Model",     "XGBoost",    "Selected for final predictions", "#7c3aed"),
                  metric_card("XGB Val. RMSE",  rmse_val,     "Validation RMSE (Mg/ha)",        "#2563eb"),
                  metric_card("XGB Val. R²",    r2_val,       "Validation R-squared",           "#0f766e"),
                  metric_card("GLMNet RMSE",    glmnet_rmse,  "Comparison model RMSE",          "#475569")
                ),
                br(),
                panel_card("Model Comparison Summary",
                           "Cross-validation performance for XGBoost vs GLMNet.",
                           DTOutput("comparison_tbl")
                )
            )
  ),
  
  # Tab 2 — Yield EDA
  nav_panel("📊 Yield EDA",
            div(class="container-fluid",
                layout_columns(
                  col_widths = c(4,8),
                  panel_card("Filters",
                             "Filter training data by year and site.",
                             pickerInput("eda_years", "Select year(s):",
                                         choices=years_train, selected=years_train,
                                         multiple=TRUE, options=list(`actions-box`=TRUE)),
                             pickerInput("eda_sites", "Select site(s):",
                                         choices=sites_train,
                                         selected=sites_train[1:min(8,length(sites_train))],
                                         multiple=TRUE,
                                         options=list(`actions-box`=TRUE,`live-search`=TRUE)),
                             sliderInput("yield_bins","Histogram bins:",20,80,40)
                  ),
                  panel_card("Yield Distribution",
                             "Density of observed training yield.",
                             plotlyOutput("yield_density_plot", height="420px")
                  )
                ),
                panel_card("Yield by Year",
                           "How yield varied across years in the training data.",
                           plotlyOutput("yield_box_year", height="420px")
                )
            )
  ),
  
  # Tab 3 — Soil EDA
  nav_panel("🌱 Soil EDA",
            div(class="container-fluid",
                layout_columns(
                  col_widths = c(4,8),
                  panel_card("Soil Controls","Select a soil variable to explore.",
                             selectInput("soil_var","Soil variable:",
                                         choices=setNames(soil_vars, soil_pretty[soil_vars]),
                                         selected=soil_vars[1]),
                             radioButtons("soil_plot_type","Plot type:",
                                          choices=c("Histogram","Yield relationship","Boxplot by year"),
                                          inline=TRUE)
                  ),
                  panel_card("Soil Variable Exploration",
                             "Explore the selected soil predictor.",
                             plotlyOutput("soil_plot", height="460px")
                  )
                )
            )
  ),
  
  # Tab 4 — Weather EDA
  nav_panel("🌤️ Weather EDA",
            div(class="container-fluid",
                layout_columns(
                  col_widths = c(4,8),
                  panel_card("Weather Controls","Select a weather variable to explore.",
                             selectInput("weather_var","Weather variable:",
                                         choices=setNames(weather_vars, weather_pretty[weather_vars]),
                                         selected=weather_vars[1]),
                             radioButtons("weather_plot_type","Plot type:",
                                          choices=c("Histogram","Yield relationship","Mean by year"),
                                          inline=TRUE),
                             sliderInput("wx_years","Year range:",
                                         min=min(years_train), max=max(years_train),
                                         value=c(min(years_train),max(years_train)), sep="")
                  ),
                  panel_card("Weather Variable Exploration",
                             "Explore the selected weather predictor.",
                             plotlyOutput("weather_plot", height="460px")
                  )
                )
            )
  ),
  
  # Tab 5 — Model Performance
  nav_panel("📈 Model Performance",
            div(class="container-fluid",
                panel_card("Predicted vs Observed",
                           "Validation diagnostics from the held-out internal set.",
                           plotlyOutput("pred_obs_plot", height="500px")
                ),
                panel_card("🔍 Variable Importance",
                           "Top predictors contributing to the final XGBoost model.",
                           tags$img(src="xgb_variable_importance.png", class="app-img")
                )
            )
  ),
  
  # Tab 6 — 2024 Predictions
  nav_panel("🌽 2024 Predictions",
            div(class="container-fluid",
                layout_columns(
                  col_widths = c(4,8),
                  panel_card("Prediction Filters",
                             "Filter 2024 XGBoost predictions.",
                             selectInput("pred_site","Site:",
                                         choices=c("All",sites_pred), selected="All"),
                             sliderInput("pred_range","Predicted yield range:",
                                         min=floor(min(preds_xgb$yield_mg_ha,na.rm=TRUE)),
                                         max=ceiling(max(preds_xgb$yield_mg_ha,na.rm=TRUE)),
                                         value=c(floor(min(preds_xgb$yield_mg_ha,na.rm=TRUE)),
                                                 ceiling(max(preds_xgb$yield_mg_ha,na.rm=TRUE))))
                  ),
                  panel_card("Prediction Explorer",
                             "Interactive table of predicted 2024 yield values.",
                             DTOutput("pred_table")
                  )
                ),
                panel_card("Predicted Yield by Site",
                           "Site-level distribution of 2024 predicted yield.",
                           plotlyOutput("pred_site_plot", height="480px")
                )
            )
  )
) # end page_navbar


# ── SERVER ────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Model comparison table
  output$comparison_tbl <- renderDT({
    datatable(
      model_summary %>% select(model, .metric, mean, std_err),
      rownames = FALSE,
      options  = list(pageLength=5, scrollX=TRUE),
      class    = "compact stripe hover"
    )
  })
  
  # Filtered training data
  eda_filtered <- reactive({
    req(input$eda_years, input$eda_sites)
    train_df %>%
      filter(year %in% input$eda_years,
             site_clean %in% input$eda_sites)
  })
  
  # Yield density
  output$yield_density_plot <- renderPlotly({
    dat <- eda_filtered(); req(nrow(dat) > 0)
    p <- ggplot(dat, aes(x=yield_mg_ha)) +
      geom_histogram(aes(y=after_stat(density)),
                     bins=input$yield_bins, fill="#cbd5e1",
                     color="white", alpha=0.7) +
      geom_density(fill="#0f766e", alpha=0.45,
                   color="#0f172a", linewidth=1) +
      labs(x="Yield (Mg/ha)", y="Density") +
      theme_minimal(base_size=14) +
      theme(panel.grid.minor=element_blank())
    plotly_theme(p)
  })
  
  # Yield by year
  output$yield_box_year <- renderPlotly({
    dat <- eda_filtered(); req(nrow(dat) > 0)
    p <- ggplot(dat, aes(x=factor(year), y=yield_mg_ha, fill=factor(year))) +
      geom_boxplot(alpha=0.85, show.legend=FALSE, outlier.alpha=0.25) +
      scale_fill_brewer(palette="PuBuGn") +
      labs(x="Year", y="Yield (Mg/ha)") +
      theme_minimal(base_size=14) +
      theme(panel.grid.minor=element_blank())
    plotly_theme(p)
  })
  
  # Soil plot
  output$soil_plot <- renderPlotly({
    var <- input$soil_var; req(var)
    dat <- train_df %>% filter(!is.na(.data[[var]]), !is.na(yield_mg_ha))
    req(nrow(dat) > 0)
    p <- if (input$soil_plot_type == "Histogram") {
      ggplot(dat, aes(x=.data[[var]])) +
        geom_histogram(fill="#b08968", color="white", bins=30, alpha=0.9) +
        labs(x=soil_pretty[var], y="Count") + theme_minimal(base_size=14)
    } else if (input$soil_plot_type == "Yield relationship") {
      ggplot(dat, aes(x=.data[[var]], y=yield_mg_ha)) +
        geom_point(alpha=0.3, color="#8b5e34") +
        geom_smooth(method="lm", se=TRUE, color="#0f172a") +
        labs(x=soil_pretty[var], y="Yield (Mg/ha)") + theme_minimal(base_size=14)
    } else {
      ggplot(dat, aes(x=factor(year), y=.data[[var]], fill=factor(year))) +
        geom_boxplot(alpha=0.85, show.legend=FALSE) +
        labs(x="Year", y=soil_pretty[var]) + theme_minimal(base_size=14)
    }
    plotly_theme(p)
  })
  
  # Weather plot
  output$weather_plot <- renderPlotly({
    var <- input$weather_var; req(var)
    dat <- train_df %>%
      filter(!is.na(.data[[var]]), !is.na(yield_mg_ha),
             year >= input$wx_years[1], year <= input$wx_years[2])
    req(nrow(dat) > 0)
    p <- if (input$weather_plot_type == "Histogram") {
      ggplot(dat, aes(x=.data[[var]])) +
        geom_histogram(fill="#3b82f6", color="white", bins=30, alpha=0.9) +
        labs(x=weather_pretty[var], y="Count") + theme_minimal(base_size=14)
    } else if (input$weather_plot_type == "Yield relationship") {
      ggplot(dat, aes(x=.data[[var]], y=yield_mg_ha)) +
        geom_point(alpha=0.3, color="#2563eb") +
        geom_smooth(method="lm", se=TRUE, color="#0f172a") +
        labs(x=weather_pretty[var], y="Yield (Mg/ha)") + theme_minimal(base_size=14)
    } else {
      yearly <- dat %>% group_by(year) %>%
        summarise(mean_val=mean(.data[[var]], na.rm=TRUE), .groups="drop")
      ggplot(yearly, aes(x=year, y=mean_val)) +
        geom_line(color="#2563eb", linewidth=1.2) +
        geom_point(color="#0f766e", size=3) +
        labs(x="Year", y=weather_pretty[var]) + theme_minimal(base_size=14)
    }
    plotly_theme(p)
  })
  
  # Predicted vs observed
  output$pred_obs_plot <- renderPlotly({
    req(nrow(val_preds) > 0)
    p <- ggplot(val_preds, aes(x=yield_mg_ha, y=.pred)) +
      geom_point(alpha=0.25, color="#0f766e", size=1.2) +
      geom_abline(slope=1, intercept=0, color="#dc2626",
                  linetype=2, linewidth=1) +
      geom_smooth(method="lm", se=FALSE, color="#f59e0b", linewidth=1.1) +
      annotate("label", x=3, y=14,
               label=paste0("R² = ",r2_val,"\nRMSE = ",rmse_val," Mg/ha"),
               hjust=0, fill="white", color="#0f172a",
               label.size=0.25, size=5) +
      labs(x="Observed Yield (Mg/ha)", y="Predicted Yield (Mg/ha)") +
      theme_minimal(base_size=14) +
      theme(panel.grid.minor=element_blank())
    plotly_theme(p)
  })
  
  # Filtered predictions
  pred_filtered <- reactive({
    dat <- preds_xgb %>%
      filter(yield_mg_ha >= input$pred_range[1],
             yield_mg_ha <= input$pred_range[2])
    if (input$pred_site != "All") dat <- dat %>% filter(site==input$pred_site)
    dat
  })
  
  # Predictions table
  output$pred_table <- renderDT({
    datatable(pred_filtered(), rownames=FALSE, filter="top",
              options=list(pageLength=12, scrollX=TRUE),
              class="compact stripe hover")
  })
  
  # Predictions by site
  output$pred_site_plot <- renderPlotly({
    dat <- pred_filtered(); req(nrow(dat) > 0)
    p <- ggplot(dat, aes(x=reorder(site,yield_mg_ha,FUN=median),
                         y=yield_mg_ha, fill=site)) +
      geom_boxplot(show.legend=FALSE, alpha=0.85, outlier.alpha=0.2) +
      coord_flip() +
      labs(x="Site", y="Predicted Yield (Mg/ha)") +
      theme_minimal(base_size=14) +
      theme(panel.grid.minor=element_blank())
    plotly_theme(p)
  })
  
} # end server

shinyApp(ui, server)