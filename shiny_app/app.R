# CORN YIELD PREDICTION - SHINY APP
# Team 10: Amit Godara & Taiwo Owolanke
# 2026 DSA Final Project

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
library(here)

safe_read_csv <- function(path_candidates, ...) {
  for (p in path_candidates) {
    if (file.exists(p)) return(read_csv(p, show_col_types = FALSE, ...))
  }
  NULL
}

safe_image_src <- function(filename_candidates) {
  full_candidates <- unique(c(
    filename_candidates,
    file.path("www", filename_candidates),
    file.path("shiny_app", "www", filename_candidates)
  ))
  
  existing <- full_candidates[file.exists(full_candidates)]
  if (length(existing) == 0) return(NULL)
  
  basename(existing[[1]])
}

metric_card <- function(title, value, subtitle, color = "#0f766e") {
  div(
    class = "metric-card",
    style = paste0("--card-accent:", color, ";"),
    div(class = "metric-label", title),
    div(class = "metric-value", value),
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
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = "#0f172a"),
      margin = list(l = 60, r = 20, b = 55, t = 20)
    )
}

train_df <- safe_read_csv(c(
  here("shiny_app", "train_df.csv")
))

preds_xgb <- safe_read_csv(c(
  here("shiny_app", "testing_submission_filled_xgb_FINAL.csv")
))

model_summary <- safe_read_csv(c(
  here("shiny_app", "model_comparison_summary.csv")
))

val_preds <- safe_read_csv(c(
  here("shiny_app", "xgb_internal_validation_predictions.csv")
))

val_metrics <- safe_read_csv(c(
  here("shiny_app","xgb_internal_validation_metrics.csv")
))

validate(need(!is.null(train_df), "train_df.csv not found."))
validate(need(!is.null(preds_xgb), "XGBoost prediction CSV not found."))
validate(need(!is.null(model_summary), "model_comparison_summary.csv not found."))

www_dir <- "shiny_app/www"
dir.create(www_dir, showWarnings = T)

pred_perf_src <- safe_image_src(c("xgb_internal_validation_performance.jpg"
))

vip_src <- safe_image_src(c(
  "xgb_variable_importance.png"
))

num_cols <- c(
  "yield_mg_ha", "soilpH", "om_pct", "soilk_ppm", "soilp_ppm",
  "tmean_season", "tmax_mean", "tmin_mean", "heat_days",
  "gdd_total", "precip_total", "srad_mean", "longitude", "latitude"
)

train_df <- train_df %>% mutate(across(any_of(num_cols), as.numeric))
preds_xgb <- preds_xgb %>% mutate(yield_mg_ha = as.numeric(yield_mg_ha))
if (!is.null(val_preds)) val_preds <- val_preds %>% mutate(across(any_of(c("yield_mg_ha", ".pred")), as.numeric))

soil_vars <- intersect(c("soilpH", "om_pct", "soilk_ppm", "soilp_ppm"), names(train_df))
weather_vars <- intersect(c("tmean_season", "tmax_mean", "tmin_mean", "heat_days", "gdd_total", "precip_total", "srad_mean"), names(train_df))

weather_pretty_names <- c(
  "tmean_season" = "Mean Temperature (°C)",
  "tmax_mean"    = "Max Temperature (°C)",
  "tmin_mean"    = "Min Temperature (°C)",
  "heat_days"    = "Heat Stress Days (>35°C)",
  "gdd_total"    = "Growing Degree Days",
  "precip_total" = "Total Precipitation (mm)",
  "srad_mean"    = "Solar Radiation"
)

soil_pretty_names <- c(
  "soilpH"       = "Soil pH",
  "om_pct"       = "Organic Matter (%)",
  "soilk_ppm"    = "Soil K (ppm)",
  "soilp_ppm"    = "Soil P (ppm)"
)

sites_train <- sort(unique(train_df$site_clean))
sites_pred <- sort(unique(preds_xgb$site))
years_train <- sort(unique(train_df$year))

xgb_rmse <- model_summary %>% filter(model == "XGBoost", .metric == "rmse") %>% pull(mean) %>% round(3)
xgb_rsq <- model_summary %>% filter(model == "XGBoost", .metric == "rsq") %>% pull(mean) %>% round(3)
glmnet_rmse <- model_summary %>% filter(model == "GLMNET", .metric == "rmse") %>% pull(mean) %>% round(3)
glmnet_rsq <- model_summary %>% filter(model == "GLMNET", .metric == "rsq") %>% pull(mean) %>% round(3)

rmse_val <- if (!is.null(val_metrics) && any(val_metrics$.metric == "rmse")) {
  val_metrics %>% filter(.metric == "rmse") %>% pull(.estimate) %>% round(3)
} else xgb_rmse

r2_val <- if (!is.null(val_metrics) && any(val_metrics$.metric == "rsq")) {
  val_metrics %>% filter(.metric == "rsq") %>% pull(.estimate) %>% round(3)
} else xgb_rsq

ui <- page_navbar(
  title = div(class = "brand-wrap", span(class = "brand-dot"), span("Corn Yield Modeling Dashboard")),
  position = "fixed-top",
  theme = bs_theme(
    version = 5,
    primary = "#0f766e",
    secondary = "#334155",
    bg = "#f8fafc",
    fg = "#0f172a",
    base_font = font_google("Inter"),
    heading_font = font_google("Poppins"),
    code_font = font_google("JetBrains Mono")
  ),
  header = tags$head(
    tags$style(HTML(" 
      :root {
        --bg: #f8fafc;
        --surface: #ffffff;
        --surface-2: #f1f5f9;
        --text: #0f172a;
        --muted: #64748b;
        --line: #dbe4ee;
        --teal: #0f766e;
        --navy: #0f172a;
        --blue: #2563eb;
        --purple: #7c3aed;
        --shadow: 0 18px 40px rgba(15,23,42,0.08);
      }
      html { scroll-behavior: smooth; }
      body {
        padding-top: 78px;
        background: linear-gradient(180deg, #f8fafc 0%, #eef4f8 100%);
      }
      .navbar {
        background: rgba(255,255,255,0.82) !important;
        backdrop-filter: blur(14px);
        border-bottom: 1px solid rgba(219,228,238,0.8);
        box-shadow: 0 8px 24px rgba(15,23,42,0.06);
      }
      .navbar-brand, .navbar-nav .nav-link {
        color: var(--text) !important;
      }
      .navbar-nav .nav-link {
        font-weight: 700;
        padding-top: 1rem;
        padding-bottom: 1rem;
      }
      .navbar-nav .nav-link.active {
        color: var(--teal) !important;
        box-shadow: inset 0 -2px 0 var(--teal);
      }
      .brand-wrap {
        display:flex; align-items:center; gap:10px; font-weight:800;
      }
      .brand-dot {
        width: 12px; height: 12px; border-radius: 999px;
        background: linear-gradient(135deg, var(--teal), var(--blue));
        box-shadow: 0 0 0 6px rgba(15,118,110,0.12);
      }
      .hero-wrap {
        background: radial-gradient(circle at top right, rgba(59,130,246,0.25), transparent 32%),
                    linear-gradient(135deg, #0f172a 0%, #134e4a 100%);
        color: white;
        padding: 38px;
        border-radius: 30px;
        margin: 22px 0 28px 0;
        box-shadow: 0 24px 60px rgba(15, 23, 42, 0.24);
      }
      .hero-title {
        font-size: clamp(2rem, 1.6rem + 1.5vw, 2.8rem);
        font-weight: 800;
        margin-bottom: 10px;
      }
      .hero-subtitle {
        font-size: 1rem;
        opacity: 0.92;
        max-width: 950px;
        line-height: 1.7;
      }
      .metric-card {
        background: linear-gradient(135deg, var(--card-accent), var(--navy));
        border-radius: 24px;
        padding: 24px;
        color: white;
        box-shadow: 0 18px 40px rgba(15,23,42,0.18);
        height: 100%;
      }
      .metric-label { font-size: 0.95rem; opacity: 0.82; margin-bottom: 8px; }
      .metric-value { font-size: 2.1rem; font-weight: 800; line-height: 1.05; }
      .metric-subtitle { margin-top: 10px; font-size: 0.92rem; opacity: 0.82; }
      .panel-card {
        background: rgba(255,255,255,0.88);
        backdrop-filter: blur(8px);
        border: 1px solid rgba(219,228,238,0.7);
        border-radius: 24px;
        padding: 22px;
        box-shadow: var(--shadow);
        margin-bottom: 24px;
      }
      .panel-title { font-size: 1.18rem; font-weight: 800; color: var(--text); }
      .panel-subtitle { font-size: 0.95rem; color: var(--muted); margin: 6px 0 14px 0; line-height: 1.6; }
      .form-label, .control-label { font-weight: 700 !important; color: #1f2937 !important; }
      .irs--shiny .irs-bar, .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to {
        background: var(--teal); border-top-color: var(--teal); border-bottom-color: var(--teal);
      }
      .selectize-input, .form-control, .shiny-input-container .form-select {
        border-radius: 14px !important;
        border: 1px solid #cbd5e1 !important;
        min-height: 46px !important;
        box-shadow: none !important;
      }
      .dataTables_wrapper .dataTables_filter input {
        border-radius: 12px !important; border: 1px solid #cbd5e1 !important; padding: 6px 10px !important;
      }
      .app-img {
        width: 100%; border-radius: 18px; border: 1px solid rgba(219,228,238,0.9); background: white;
      }
      .note-chip {
        display:inline-flex; align-items:center; gap:8px; padding:8px 12px; border-radius:999px;
        background:#ecfeff; color:#115e59; font-size:0.88rem; font-weight:700; margin-bottom:12px;
      }
      @media (max-width: 768px) {
        body { padding-top: 88px; }
        .hero-wrap { padding: 26px; border-radius: 24px; }
      }
    "))
  ),
  nav_panel(
    "Overview",
    div(
      class = "container-fluid",
      div(
        class = "hero-wrap",
        div(class = "hero-title", "🌽 Corn Yield Prediction Dashboard"),
        div(
          class = "hero-subtitle",
          "Team 10 | Amit Godara & Taiwo Owolanke | 2026 DSA CRSS 8030 Final Project"
        )
      ),
      layout_columns(
        col_widths = c(2, 2, 2, 2, 2, 2),
        metric_card("Training Observations", "157K","Number of training", "#2563ab"),
        metric_card("Predictors Used", "22", "Number of predictors used", "#2743eb"),
        metric_card("Final model", "XGBoost", "Selected for final result communication", "#0f766e"),
        metric_card("Validation RMSE", rmse_val, "Held-out or summary error", "#2563eb"),
        metric_card("Validation R²", r2_val, "Held-out or summary fit", "#7c3aed"),
        metric_card("GLMNET RMSE", glmnet_rmse, "Benchmark comparison", "#475569")
      ),
      panel_card(
        "Model comparison summary",
        "Cross-validation summary for the two final candidate models.",
        DTOutput("comparison_tbl")
      )
    )
  ),
  nav_panel(
    "📊 Yield EDA",
    div(
      class = "container-fluid",
      layout_columns(
        col_widths = c(4, 8),
        panel_card(
          "Interactive filters",
          "Explore observed training yield by year and site.",
          pickerInput(
            "eda_years", "Select year(s)",
            choices = years_train, selected = years_train,
            multiple = TRUE, options = list(`actions-box` = TRUE)
          ),
          pickerInput(
            "eda_sites", "Select site(s)",
            choices = sites_train,
            selected = sites_train[1:min(8, length(sites_train))],
            multiple = TRUE,
            options = list(`actions-box` = TRUE, `live-search` = TRUE)
          ),
          sliderInput("yield_bins", "Histogram bins", min = 20, max = 80, value = 40)
        ),
        panel_card(
          "Yield distribution",
          "Density of observed training yield after filtering.",
          plotlyOutput("yield_density_plot", height = "420px")
        )
      ),
      panel_card(
        "Yield by year",
        "Boxplots show how yield varied across years in the filtered training data.",
        plotlyOutput("yield_box_year", height = "420px")
      )
    )
  ),
  nav_panel(
    "🌱 Soil EDA",
    div(
      class = "container-fluid",
      layout_columns(
        col_widths = c(4, 8),
        panel_card(
          "Soil controls",
          "Combines the simpler soil selector from app_2.R with the stronger yield-relationship view from app.R.",
          # selectInput("soil_var", "Soil variable", choices = soil_vars, selected = soil_vars[1]),
          selectInput(
            "soil_var",
            "Soil variable",
            choices = setNames(soil_vars, soil_pretty_names[soil_vars]),
            selected = soil_vars[1]
          ),
          radioButtons("soil_plot_type", "Plot type", choices = c("Histogram", "Yield relationship", "Boxplot by year"), inline = TRUE)
        ),
        panel_card(
          "Soil variable exploration",
          "Explore the selected soil predictor in the training data.",
          plotlyOutput("soil_plot", height = "460px")
        )
      )
    )
  ),
  nav_panel(
    "🌤️ Weather EDA",
    div(
      class = "container-fluid",
      layout_columns(
        col_widths = c(4, 8),
        panel_card(
          "Weather controls",
          "This section merges app_2.R's year-range idea with app.R's direct predictor-to-yield exploration.",
          # selectInput("weather_var", "Weather variable", choices = weather_vars, selected = weather_vars[1]),
          selectInput(
            "weather_var",
            "Weather variable",
            choices = setNames(weather_vars, weather_pretty_names[weather_vars]),
            selected = weather_vars[1]
          ),
          radioButtons("weather_plot_type", "Plot type", choices = c("Histogram", "Yield relationship", "Mean by year"), inline = TRUE),
          sliderInput("wx_years", "Year range", min = min(years_train), max = max(years_train), value = c(min(years_train), max(years_train)), sep = "")
        ),
        panel_card(
          "Weather variable exploration",
          "Explore the selected weather predictor in the training data.",
          plotlyOutput("weather_plot", height = "460px")
        )
      )
    )
  ),
  nav_panel(
    "📈 Model Performance",
    div(
      class = "container-fluid",
      panel_card(
        "Predicted vs observed",
        if (!is.null(val_preds)) "Validation-based predicted vs observed diagnostics from the held-out internal set." else "Static diagnostic figure for the final XGBoost model.",
        div(class = "note-chip", if (!is.null(val_preds)) "Validation diagnostics active" else "Static figure fallback active"),
        if (!is.null(val_preds)) plotlyOutput("pred_obs_plot", height = "520px") else if (!is.null(pred_perf_src)) tags$img(src = pred_perf_src, class = "app-img") else div("Prediction performance image not found.")
      ),
      panel_card(
        "🔍 Variable importance",
        "Top predictors contributing to the final XGBoost model.",
        if (!is.null(vip_src)) tags$img(src = vip_src, class = "app-img") else div("Variable importance image not found.")
      )
    )
  ),
  nav_panel(
    "2024 Predictions",
    div(
      class = "container-fluid",
      layout_columns(
        col_widths = c(4, 8),
        panel_card(
          "Prediction filters",
          "Search and filter final XGBoost predictions for the 2024 submission set.",
          selectInput("pred_site", "Site", choices = c("All", sites_pred), selected = "All"),
          sliderInput(
            "pred_range", "Predicted yield range",
            min = floor(min(preds_xgb$yield_mg_ha, na.rm = TRUE)),
            max = ceiling(max(preds_xgb$yield_mg_ha, na.rm = TRUE)),
            value = c(floor(min(preds_xgb$yield_mg_ha, na.rm = TRUE)), ceiling(max(preds_xgb$yield_mg_ha, na.rm = TRUE)))
          )
        ),
        panel_card(
          "Prediction explorer",
          "Interactive table of predicted 2024 yield values from the final XGBoost model.",
          DTOutput("pred_table")
        )
      ),
      panel_card(
        "Predicted yield by site",
        "Site-level distribution of predicted 2024 yield values.",
        plotlyOutput("pred_site_plot", height = "500px")
      )
    )
  )
)

server <- function(input, output, session) {
  output$comparison_tbl <- renderDT({
    datatable(
      model_summary,
      rownames = FALSE,
      options = list(pageLength = 5, scrollX = TRUE),
      class = "compact stripe hover"
    )
  })
  
  eda_filtered <- reactive({
    req(input$eda_years, input$eda_sites)
    train_df %>% filter(year %in% input$eda_years, site_clean %in% input$eda_sites)
  })
  
  output$yield_density_plot <- renderPlotly({
    dat <- eda_filtered(); req(nrow(dat) > 0)
    p <- ggplot(dat, aes(x = yield_mg_ha)) +
      geom_histogram(aes(y = after_stat(density)), bins = input$yield_bins, fill = "#cbd5e1", color = "white", alpha = 0.7) +
      geom_density(fill = "#0f766e", alpha = 0.45, color = "#0f172a", linewidth = 1) +
      labs(x = "Yield (Mg/ha)", y = "Density") +
      theme_minimal(base_size = 14) + theme(panel.grid.minor = element_blank())
    plotly_theme(p)
  })
  
  output$yield_box_year <- renderPlotly({
    dat <- eda_filtered(); req(nrow(dat) > 0)
    p <- ggplot(dat, aes(x = factor(year), y = yield_mg_ha, fill = factor(year))) +
      geom_boxplot(alpha = 0.85, show.legend = FALSE, outlier.alpha = 0.25) +
      scale_fill_brewer(palette = "PuBuGn") +
      labs(x = "Year", y = "Yield (Mg/ha)") +
      theme_minimal(base_size = 14) + theme(panel.grid.minor = element_blank())
    plotly_theme(p)
  })
  
  output$soil_plot <- renderPlotly({
    var <- input$soil_var; req(var)
    dat <- train_df %>% filter(!is.na(.data[[var]]), !is.na(yield_mg_ha))
    req(nrow(dat) > 0)
    p <- if (input$soil_plot_type == "Histogram") {
      ggplot(dat, aes(x = .data[[var]])) +
        geom_histogram(fill = "#b08968", color = "white", bins = 30, alpha = 0.9) +
        labs(x = var, y = "Count") + theme_minimal(base_size = 14)
    } else if (input$soil_plot_type == "Yield relationship") {
      ggplot(dat, aes(x = .data[[var]], y = yield_mg_ha)) +
        geom_point(alpha = 0.32, color = "#8b5e34") +
        geom_smooth(method = "lm", se = TRUE, color = "#0f172a") +
        labs(x = var, y = "Yield (Mg/ha)") + theme_minimal(base_size = 14)
    } else {
      ggplot(dat, aes(x = factor(year), y = .data[[var]], fill = factor(year))) +
        geom_boxplot(alpha = 0.85, show.legend = FALSE) +
        labs(x = "Year", y = var) + theme_minimal(base_size = 14)
    }
    plotly_theme(p)
  })
  
  output$weather_plot <- renderPlotly({
    var <- input$weather_var; req(var)
    dat <- train_df %>% filter(!is.na(.data[[var]]), !is.na(yield_mg_ha), year >= input$wx_years[1], year <= input$wx_years[2])
    req(nrow(dat) > 0)
    p <- if (input$weather_plot_type == "Histogram") {
      ggplot(dat, aes(x = .data[[var]])) +
        geom_histogram(fill = "#3b82f6", color = "white", bins = 30, alpha = 0.9) +
        labs(x = var, y = "Count") + theme_minimal(base_size = 14)
    } else if (input$weather_plot_type == "Yield relationship") {
      ggplot(dat, aes(x = .data[[var]], y = yield_mg_ha)) +
        geom_point(alpha = 0.32, color = "#2563eb") +
        geom_smooth(method = "lm", se = TRUE, color = "#0f172a") +
        labs(x = var, y = "Yield (Mg/ha)") + theme_minimal(base_size = 14)
    } else {
      yearly <- dat %>% group_by(year) %>% summarise(mean_val = mean(.data[[var]], na.rm = TRUE), .groups = "drop")
      ggplot(yearly, aes(x = year, y = mean_val)) +
        geom_line(color = "#2563eb", linewidth = 1.2) + geom_point(color = "#0f766e", size = 3) +
        labs(x = "Year", y = var) + theme_minimal(base_size = 14)
    }
    plotly_theme(p)
  })
  
  output$pred_obs_plot <- renderPlotly({
    req(!is.null(val_preds), nrow(val_preds) > 0)
    p <- ggplot(val_preds, aes(x = yield_mg_ha, y = .pred)) +
      geom_point(alpha = 0.25, color = "#0f766e", size = 1.3) +
      geom_abline(slope = 1, intercept = 0, color = "#dc2626", linetype = 2, linewidth = 1) +
      geom_smooth(method = "lm", se = FALSE, color = "#f59e0b", linewidth = 1.1) +
      annotate("label", x = Inf, y = -Inf,
               label = paste0("R² = ", r2_val, "\nRMSE = ", rmse_val, " Mg/ha"),
               hjust = 1.08, vjust = -0.4, fill = "white", color = "#0f172a", label.size = 0.25) +
      labs(x = "Observed Yield (Mg/ha)", y = "Predicted Yield (Mg/ha)") +
      theme_minimal(base_size = 14) + theme(panel.grid.minor = element_blank())
    plotly_theme(p)
  })
  
  pred_filtered <- reactive({
    dat <- preds_xgb %>% filter(yield_mg_ha >= input$pred_range[1], yield_mg_ha <= input$pred_range[2])
    if (!is.null(input$pred_site) && input$pred_site != "All") dat <- dat %>% filter(site == input$pred_site)
    dat
  })
  
  output$pred_table <- renderDT({
    datatable(
      pred_filtered(), rownames = FALSE, filter = "top",
      options = list(pageLength = 12, scrollX = TRUE), class = "compact stripe hover"
    )
  })
  
  output$pred_site_plot <- renderPlotly({
    dat <- pred_filtered(); req(nrow(dat) > 0)
    p <- ggplot(dat, aes(x = reorder(site, yield_mg_ha, FUN = median), y = yield_mg_ha, fill = site)) +
      geom_boxplot(show.legend = FALSE, alpha = 0.85, outlier.alpha = 0.2) +
      coord_flip() +
      labs(x = "Site", y = "Predicted yield (Mg/ha)") +
      theme_minimal(base_size = 14) + theme(panel.grid.minor = element_blank())
    plotly_theme(p)
  })
}

shinyApp(ui, server)
