library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinymaterial)
library(plotly)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(lmtest)
library(car)
library(broom)
library(DT)
library(leaflet)
library(leaflet.extras)
library(geojsonio)
library(sf)
library(tidyverse)

# EMISI KARBON DATA
tryCatch({
  data_karbon_raw <- read_excel("data/Emisi karbon indonesia.xlsx")
  cat("✅ Data emisi karbon berhasil dibaca!\n")
  print(head(data_karbon_raw))
  cat("Nama kolom karbon:", names(data_karbon_raw), "\n")
}, error = function(e) {
  cat("❌ Error membaca data emisi karbon:", e$message, "\n")
})

# LUAS KEBAKARAN DATA
tryCatch({
  data_raw <- read_excel("data/Luas kebakaran hutan dan lahan indonesia.xlsx")
  cat("✅ Data luas kebakaran berhasil dibaca!\n")
  print(head(data_raw))
  cat("Nama kolom:", names(data_raw), "\n")
}, error = function(e) {
  cat("❌ Error membaca data luas kebakaran:", e$message, "\n")
})

# SUHU DATA
tryCatch({
  data_suhu_raw <- read_excel("data/Suhu rata rata indonesia.xlsx")
  cat("✅ Data suhu berhasil dibaca!\n")
  print(head(data_suhu_raw))
  cat("Nama kolom suhu:", names(data_suhu_raw), "\n")
}, error = function(e) {
  cat("❌ Error membaca data suhu:", e$message, "\n")
})

# Load geojson data untuk choropleth
tryCatch({
  geojson <- geojson_sf("data/indonesia.geojson")
  cat("✅ Data geojson berhasil dibaca!\n")
}, error = function(e) {
  cat("❌ Error membaca data geojson:", e$message, "\n")
  # Create dummy geojson if file not found
  geojson <- NULL
})

data_karbon_long <- data_karbon_raw %>%
  pivot_longer(cols = -Provinsi, names_to = "tahun", values_to = "emisi_karbon") %>%
  mutate(
    tahun = as.numeric(tahun),
    emisi_karbon = as.numeric(emisi_karbon),
    emisi_karbon = ifelse(is.na(emisi_karbon), 0, emisi_karbon)
  ) %>%
  rename(provinsi = Provinsi) %>%
  filter(tahun >= 2015 & tahun <= 2024)

data_long <- data_raw %>%
  pivot_longer(cols = -Provinsi, names_to = "tahun", values_to = "luas_kebakaran") %>%
  mutate(
    tahun = as.numeric(tahun),
    luas_kebakaran = as.numeric(luas_kebakaran),
    luas_kebakaran = ifelse(is.na(luas_kebakaran), 0, luas_kebakaran)
  ) %>%
  rename(provinsi = Provinsi) %>%
  filter(tahun >= 2015 & tahun <= 2024)

data_suhu_clean <- data_suhu_raw %>%
  mutate(
    tahun = as.numeric(tahun),
    Suhu_Rata = as.numeric(Suhu_Rata)
  ) %>%
  filter(tahun >= 2015 & tahun <= 2024)

dataprovlengkap <- left_join(data_long, data_karbon_long, by = c("provinsi","tahun")) %>%
  filter(provinsi != "Indonesia")


carbon_data <- data_karbon_long %>% arrange(provinsi, tahun)
fire_data <- data_long %>% arrange(provinsi, tahun)
temp_data <- data_suhu_clean %>% arrange(tahun)


ui <- dashboardPage(
  dashboardHeader(title = tagList(
    icon("square-poll-vertical"), "Dashboard Karhutla"
  )),
  
  dashboardSidebar(
    width = 250,
    div(style = "padding: 10px; text-align: center; font-weight: bold; font-size: 16px;", "MENU"),
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem(
        "Grafik",
        tabName = "grafik",
        icon = icon("chart-bar"),
        startExpanded = FALSE,
        menuSubItem(
          "Luas Karhutla",
          tabName = "kebakaran",
          icon = icon("fire")
        ),
        menuSubItem("Emisi Karbon", tabName = "karbon", icon = icon("cloud")),
        menuSubItem(
          "Suhu Udara",
          tabName = "suhu",
          icon = icon("thermometer")
        )
      ),
      menuItem(
        "Ranking Nasional",
        tabName = "ranking",
        icon = icon("trophy")
      ),
      menuItem(
        "Peta Choropleth",
        tabName = "choropleth",
        icon = icon("globe")
      ),
      menuItem(
        "Analisis Statistik",
        tabName = "statistik",
        icon = icon("calculator")
      ),
      menuItem("Metadata", tabName = "metadata", icon = icon("info-circle")),
      menuItem("About Us", tabName = "aboutus", icon = icon("users"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$script(HTML("document.title = 'Dashboard Karhutla || Kelompok 4';")),
      tags$link(rel = "icon", type = "image/png", href = "icon.png"),
      tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Allerta+Stencil&family=Montserrat:ital,wght@0,100..900;1,100..900&display=swap"),
      tags$script(HTML("$(document).ready(function() {
  $('#sortFireDesc').click(function() {
    $('#sortFireDesc').addClass('active');
    $('#sortFireAsc').removeClass('active');
  });
  
  $('#sortFireAsc').click(function() {
    $('#sortFireAsc').addClass('active');
    $('#sortFireDesc').removeClass('active');
  });
  
  $('#sortCarbonDesc').click(function() {
    $('#sortCarbonDesc').addClass('active');
    $('#sortCarbonAsc').removeClass('active');
  });
  
  $('#sortCarbonAsc').click(function() {
    $('#sortCarbonAsc').addClass('active');
    $('#sortCarbonDesc').removeClass('active');
  });
});")),
      tags$style(
        HTML(
          "
          
    table.dataTable thead th.sorting:after,
    table.dataTable thead th.sorting_asc:after,
    table.dataTable thead th.sorting_desc:after {
      font-family: FontAwesome, Arial;
      padding-left: 6px;
      font-size: 0.8em;
      opacity: 0.7;
      color: yellow;
    }
    table.dataTable thead th.sorting_asc,
    table.dataTable thead th.sorting_desc {
      background-color: #20a8d8 !important;
    }

    table.dataTable thead th {
      font-weight: 600;
      color: white;
      background-color: #17a2b8;
    }
    .main-header {
      background-color: #109f9e !important;
      border-bottom: none !important;
      box-shadow: none !important;
      position: fixed !important;
      top: 0 !important;
      left: 0 !important;
      right: 0 !important;
      z-index: 1030 !important;
    }
    
    .main-sidebar {
      background-color: #505b6b !important;
      height: 100vh !important;
      position: fixed !important;
      top: 0 !important;
      left: 0 !important;
      z-index: 1020 !important;
      overflow-y: auto !important;
      transition: transform 0.3s ease-in-out !important;
    }
  
    .sidebar-collapse .main-sidebar {
      transform: translateX(-250px) !important;
    }
    
    .content-wrapper {
      margin-left: 250px !important;
      margin-top: 50px !important;
      min-height: calc(100vh - 50px) !important;
      transition: margin-left 0.3s ease-in-out !important;
    }
    
    .sidebar-collapse .content-wrapper {
      margin-left: 0 !important;
    }
    
    @media (max-width: 767px) {
      .content-wrapper {
        margin-left: 0 !important;
      }
      .main-sidebar {
        transform: translateX(-250px);
        transition: transform 0.3s ease-in-out;
      }
      .sidebar-open .main-sidebar {
        transform: translateX(0);
      }
    }
    
    .sidebar-menu > li.header {
      background-color: #505b6b !important;
      color: #ffffff !important;
    }
    
    .sidebar-menu > li > a {
      color: #ffffff !important;
      border-left: 3px solid transparent !important;
      transition: all 0.3s ease !important;
    }
    
    .sidebar-menu > li > a:hover {
      background-color: #109f9e !important;
      color: #ffffff !important;
      border-left-color: #0d7a79 !important;
    }
    
    .sidebar-menu > li.active > a {
      background-color: #109f9e !important;
      border-left-color: #0d7a79 !important;
      color: #ffffff !important;
    }
    
    .sidebar-menu .treeview-menu > li > a {
      color: #b8c7ce !important;
      padding-left: 25px !important;
    }
    
    .sidebar-menu .treeview-menu > li > a:hover {
      background-color: #109f9e !important;
      color: #ffffff !important;
    }
    
    .sidebar-menu .treeview-menu > li.active > a {
      background-color: #109f9e !important;
      color: #ffffff !important;
    }
    
    .box {
      box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
      background-color: white;
    }
    .content-header > h1 {
      font-size: 24px;
      font-weight: bold;
    }
    .main-header .navbar-brand {
      font-size: 16px !important;
      font-weight: bold;
    }
    .main-header .logo {
      font-size: 14px !important;
      background-color: #109f9e !important;
      font-family: 'Allerta Stencil', Montserrat;
    }
    .main-header .navbar {
      font-size: 16px !important;
      background-color: #109f9e !important;
    }
    .box .box-header {
      background-color: #3ad6d5 !important;
      color: white !important;
    }
    body {
      background-image: url('www/cover15.webp');
      background-size: cover;
      background-position: center center;
      background-attachment: fixed;
    }
    
    .home-container {
      padding: 20px;
      background: rgba(255, 255, 255, 0.95);
      border-radius: 10px;
      margin: 10px;
    }
    
    .welcome-header {
      text-align: center;
      color: #109f9e;
      font-family: 'Montserrat', sans-serif;
      font-weight: bold;
      margin-bottom: 30px;
      font-size: 28px;
    }
    
    .video-container {
      background: white;
      border-radius: 15px;
      padding: 20px;
      margin-top: 20px;
      box-shadow: 0 4px 15px rgba(0,0,0,0.1);
    }
    
    .video-header {
      color: #109f9e;
      font-weight: bold;
      font-size: 18px;
      margin-bottom: 15px;
      text-align: center;
    }
    
    .video-responsive {
      position: relative;
      padding-bottom: 56.25%;
      height: 0;
      overflow: hidden;
    }
    
    .video-responsive iframe {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      border-radius: 10px;
    }
    
    .user-guide-container {
      background: white;
      border-radius: 15px;
      padding: 20px;
      margin-top: 20px;
      box-shadow: 0 4px 15px rgba(0,0,0,0.1);
    }
    
    .user-guide-header {
      color: #109f9e;
      font-weight: bold;
      font-size: 18px;
      margin-bottom: 15px;
    }
    
    .user-guide-text {
      color: #666;
      line-height: 1.6;
      font-size: 14px;
    }
    
    /* ABOUT US */
    .about-container {
      background: linear-gradient(135deg, #ffffff, #f8f9fa);
      border-radius: 20px;
      padding: 30px;
      margin: 20px;
      box-shadow: 0 8px 25px rgba(0,0,0,0.15);
    }
    
    .about-header {
      text-align: center;
      color: #109f9e;
      font-family: 'Montserrat', sans-serif;
      font-weight: bold;
      margin-bottom: 30px;
      font-size: 32px;
      text-shadow: 2px 2px 4px rgba(0,0,0,0.1);
    }
    
    .member-card {
      background: linear-gradient(135deg, #109f9e, #3ad6d5);
      color: white;
      border-radius: 15px;
      padding: 25px;
      margin: 15px 0;
      box-shadow: 0 6px 20px rgba(16, 159, 158, 0.3);
      transition: all 0.3s ease;
    }
    
    .member-card:hover {
      transform: translateY(-5px);
      box-shadow: 0 10px 30px rgba(16, 159, 158, 0.4);
    }
    
    .member-photo {
      width: 120px;
      height: 120px;
      border-radius: 50%;
      object-fit: cover;
      border: 4px solid white;
      margin-bottom: 15px;
      display: block;
      margin-left: auto;
      margin-right: auto;
    }
    
    .member-name {
      font-weight: bold;
      font-size: 20px;
      text-align: center;
      margin-bottom: 10px;
    }
    
    .member-details {
      font-size: 14px;
      line-height: 1.6;
      text-align: center;
    }
    
    .member-motto {
      font-style: italic;
      margin-top: 10px;
      padding-top: 10px;
      border-top: 1px solid rgba(255,255,255,0.3);
      text-align: center;
    }
    

    .stats-card {
      color: white;
      border-radius: 15px;
      padding: 20px;
      margin: 10px 0;
      box-shadow: 0 8px 25px rgba(0,0,0,0.15);
      transition: all 0.3s ease;
      position: relative;
      overflow: hidden;
      cursor: pointer;
    }
    
    .stats-card::before {
      content: '';
      position: absolute;
      top: 0;
      left: 0;
      right: 0;
      bottom: 0;
      background: linear-gradient(45deg, rgba(255,255,255,0.1), rgba(255,255,255,0.05));
      pointer-events: none;
    }
    
    .stats-card:hover {
      transform: translateY(-8px) scale(1.02);
      box-shadow: 0 15px 35px rgba(0,0,0,0.2);
    }
    

    .stats-card-temperature {
      background: linear-gradient(135deg, #ff6b35, #f7931e, #ff9068);
      box-shadow: 0 8px 25px rgba(255, 107, 53, 0.4);
    }
    
    .stats-card-temperature:hover {
      box-shadow: 0 15px 35px rgba(255, 107, 53, 0.5);
    }
    
    .stats-card-fire {
      background: linear-gradient(135deg, #e74c3c, #c0392b, #ff5722);
      box-shadow: 0 8px 25px rgba(231, 76, 60, 0.4);
    }
    
    .stats-card-fire:hover {
      box-shadow: 0 15px 35px rgba(231, 76, 60, 0.5);
    }
    
    .stats-card-carbon {
      background: linear-gradient(135deg, #8e44ad, #3498db, #9b59b6);
      box-shadow: 0 8px 25px rgba(142, 68, 173, 0.4);
    }
    
    .stats-card-carbon:hover {
      box-shadow: 0 15px 35px rgba(142, 68, 173, 0.5);
    }
    
  
    .stats-card-province {
      background: linear-gradient(135deg, #27ae60, #16a085, #2ecc71);
      box-shadow: 0 8px 25px rgba(39, 174, 96, 0.4);
    }
    
    .stats-card-province:hover {
      box-shadow: 0 15px 35px rgba(39, 174, 96, 0.5);
    }
    
    .stats-number {
      font-size: 36px;
      font-weight: bold;
      margin-bottom: 5px;
      text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
    }
    
    .stats-label {
      font-size: 14px;
      opacity: 0.95;
      font-weight: 600;
    }
    
    .stats-description {
      font-size: 12px;
      margin-top: 8px;
      opacity: 0.85;
    }
    
    .timeline-container {
      background: white;
      border-radius: 15px;
      padding: 25px;
      margin-top: 20px;
      box-shadow: 0 4px 15px rgba(0,0,0,0.1);
    }
    
    .timeline-header {
      color: #109f9e;
      font-weight: bold;
      font-size: 20px;
      margin-bottom: 20px;
      text-align: center;
    }
    
    .timeline {
      position: relative;
      padding-left: 30px;
    }
    
    .timeline::before {
      content: '';
      position: absolute;
      left: 15px;
      top: 0;
      bottom: 0;
      width: 2px;
      background: linear-gradient(to bottom, #109f9e, #3ad6d5);
    }
    
    .timeline-item {
      position: relative;
      margin-bottom: 25px;
      padding-left: 25px;
    }
    
    .timeline-item::before {
      content: '';
      position: absolute;
      left: -8px;
      top: 5px;
      width: 12px;
      height: 12px;
      border-radius: 50%;
      background: linear-gradient(45deg, #109f9e, #3ad6d5);
      border: 3px solid white;
      box-shadow: 0 0 0 3px #109f9e;
    }
    
    .timeline-date {
      font-weight: bold;
      color: #109f9e;
      font-size: 14px;
    }
    
    .timeline-content {
      color: #666;
      font-size: 13px;
      margin-top: 3px;
    }
    
    .info-section {
      background: white;
      border-radius: 15px;
      padding: 20px;
      margin-top: 20px;
      box-shadow: 0 4px 15px rgba(0,0,0,0.1);
    }
    
    .info-header {
      color: #109f9e;
      font-weight: bold;
      font-size: 18px;
      margin-bottom: 15px;
    }
    
    .info-text {
      color: #666;
      line-height: 1.6;
      font-size: 14px;
    }
    
  
    .chart-container {
      background: white;
      border-radius: 10px;
      padding: 15px;
      margin: 10px 0;
    }
    
    
    .summary-stats-container {
      background: linear-gradient(135deg, #f8f9fa, #e9ecef);
      border-radius: 15px;
      padding: 20px;
      margin: 15px 0;
      border-left: 5px solid #109f9e;
      box-shadow: 0 4px 15px rgba(0,0,0,0.1);
    }
    
    .summary-header {
      color: #2c3e50;
      font-weight: bold;
      font-size: 18px;
      margin-bottom: 15px;
      text-align: center;
      border-bottom: 2px solid #109f9e;
      padding-bottom: 10px;
    }
    
    .summary-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
      gap: 15px;
      margin-top: 15px;
    }
    
    
    .summary-card {
      padding: 15px;
      border-radius: 12px;
      box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      transition: all 0.3s ease;
      text-align: center;
      position: relative;
      overflow: hidden;
      color: white;
    }
    
    .summary-card::before {
      content: '';
      position: absolute;
      top: 0;
      left: 0;
      right: 0;
      bottom: 0;
      background: linear-gradient(45deg, rgba(255,255,255,0.1), rgba(255,255,255,0.05));
      pointer-events: none;
    }
    
    .summary-card:hover {
      transform: translateY(-5px);
      box-shadow: 0 8px 20px rgba(0,0,0,0.2);
    }
    
    .summary-card-highest {
      background: linear-gradient(135deg, #e74c3c, #c0392b, #ff5722);
      cursor: pointer;
    }
    
    .summary-card-lowest {
      background: linear-gradient(135deg, #27ae60, #16a085, #2ecc71);
      cursor: pointer;
    }
    
    .summary-card-average {
      background: linear-gradient(135deg, #3498db, #2980b9, #5dade2);
      cursor: pointer;
    }
    
    .summary-card-total {
      background: linear-gradient(135deg, #f39c12, #e67e22, #f7dc6f);
      cursor: pointer;
    }
    
    .summary-card-trend {
      background: linear-gradient(135deg, #9b59b6, #8e44ad, #bb8fce);
      cursor: pointer;
    }
    
    .summary-label {
      font-weight: bold;
      font-size: 14px;
      margin-bottom: 8px;
      opacity: 0.9;
    }
    
    .summary-value {
      font-size: 18px;
      font-weight: bold;
      text-shadow: 1px 1px 2px rgba(0,0,0,0.3);
    }
    
    .summary-icon {
      font-size: 24px;
      margin-bottom: 8px;
      opacity: 0.8;
    }
    
    
    .main-sidebar::-webkit-scrollbar {
      width: 6px;
    }
    
    .main-sidebar::-webkit-scrollbar-track {
      background: #505b6b;
    }
    
    .main-sidebar::-webkit-scrollbar-thumb {
      background: #109f9e;
      border-radius: 3px;
    }
    
    .main-sidebar::-webkit-scrollbar-thumb:hover {
      background: #0d7a79;
    }
    
    
    .content {
      padding: 20px;
    }
    
    
    .selectize-dropdown {
      z-index: 1040 !important;
    }
    
    
    .content-wrapper {
      scroll-behavior: smooth;
    }
    
    
    .navbar-toggle {
      background-color: transparent !important;
      border: 1px solid #ffffff !important;
    }
    
    .navbar-toggle .icon-bar {
      background-color: #ffffff !important;
    }
    
    
    .ranking-container {
      background: linear-gradient(135deg, #ffffff, #f8f9fa);
      border-radius: 20px;
      padding: 25px;
      margin: 15px 0;
      box-shadow: 0 8px 25px rgba(0,0,0,0.15);
      border: 1px solid #e9ecef;
    }
    
    .ranking-header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 25px;
      padding-bottom: 20px;
      border-bottom: 3px solid transparent;
      background: linear-gradient(90deg, #109f9e, #3ad6d5) padding-box,
                  linear-gradient(90deg, #109f9e, #3ad6d5) border-box;
      border-image: linear-gradient(90deg, #109f9e, #3ad6d5) 1;
    }
    
    .ranking-title {
      color: #2c3e50;
      font-weight: bold;
      font-size: 22px;
      margin: 0;
      text-shadow: 1px 1px 2px rgba(0,0,0,0.1);
    }
    
    .ranking-controls {
      display: flex;
      gap: 15px;
      align-items: center;
    }
    
    
    .dataTables_wrapper {
      background: white;
      border-radius: 15px;
      padding: 20px;
      box-shadow: 0 4px 15px rgba(0,0,0,0.1);
    }
    
    .dataTables_wrapper .dataTables_length,
    .dataTables_wrapper .dataTables_filter,
    .dataTables_wrapper .dataTables_info,
    .dataTables_wrapper .dataTables_paginate {
      color: #2c3e50;
      font-weight: 500;
    }
    
    .dataTables_wrapper .dataTables_paginate .paginate_button {
      background: linear-gradient(135deg, #109f9e, #3ad6d5);
      color: white !important;
      border: none;
      border-radius: 8px;
      margin: 0 2px;
      padding: 8px 12px;
      transition: all 0.3s ease;
    }
    
    .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
      background: linear-gradient(135deg, #0d7a79, #2bb8b7) !important;
      transform: translateY(-2px);
      box-shadow: 0 4px 12px rgba(16, 159, 158, 0.3);
    }
    
    .dataTables_wrapper .dataTables_paginate .paginate_button.current {
      background: linear-gradient(135deg, #e74c3c, #c0392b) !important;
      color: white !important;
    }
    
    table.dataTable thead th {
      background: linear-gradient(135deg, #109f9e, #3ad6d5);
      color: white;
      font-weight: bold;
      border: none;
      padding: 15px;
      text-align: center;
    }
    
    table.dataTable tbody td {
      padding: 12px 15px;
      border-bottom: 1px solid #e9ecef;
      transition: all 0.3s ease;
      text-align: center;
    }
    
    table.dataTable tbody tr:hover {
      background: linear-gradient(135deg, #f8f9fa, #e9ecef);
      transform: scale(1.01);
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
    }
    
    table.dataTable tbody tr:nth-child(even) {
      background-color: #f8f9fa;
    }
    
    table.dataTable tbody tr:nth-child(odd) {
      background-color: white;
    }
    
    .province-name {
      font-weight: 600;
      color: #2c3e50;
    }
    
    .value-high {
      color: #e74c3c;
      font-weight: bold;
    }
    
    .value-medium {
      color: #f39c12;
      font-weight: bold;
    }
    
    .value-low {
      color: #27ae60;
      font-weight: bold;
    }
    
    .btn-ranking {
      background: linear-gradient(135deg, #109f9e, #3ad6d5);
      border: none;
      color: white;
      padding: 10px 20px;
      border-radius: 10px;
      font-weight: bold;
      transition: all 0.3s ease;
      cursor: pointer;
      box-shadow: 0 4px 12px rgba(16, 159, 158, 0.3);
    }
    
    .btn-ranking:hover {
      background: linear-gradient(135deg, #0d7a79, #2bb8b7);
      transform: translateY(-2px);
      box-shadow: 0 6px 16px rgba(16, 159, 158, 0.4);
    }
    
    .btn-ranking.active {
      background: linear-gradient(135deg, #e74c3c, #c0392b);
    }
    
    .filter-group {
      display: flex;
      align-items: center;
      gap: 10px;
    }
    
    .filter-label {
      font-weight: bold;
      color: #2c3e50;
      font-size: 14px;
    }
    

    @media (max-width: 768px) {
      .ranking-controls {
        flex-direction: column;
        gap: 10px;
      }
      
      .ranking-table {
        font-size: 12px;
      }
      
      .ranking-table th,
      .ranking-table td {
        padding: 8px 10px;
      }
    }
    
    .well {
      background: linear-gradient(135deg, #f8f9fa, #e9ecef);
      border: 1px solid #dee2e6;
      border-radius: 15px;
      padding: 20px;
      margin-bottom: 20px;
      box-shadow: 0 4px 12px rgba(0,0,0,0.1);
    }
    
    .btn-primary {
      background: linear-gradient(135deg, #109f9e, #3ad6d5);
      border: none;
      color: white;
      padding: 10px 20px;
      border-radius: 10px;
      font-weight: bold;
      transition: all 0.3s ease;
      box-shadow: 0 4px 12px rgba(16, 159, 158, 0.3);
    }
    
    .btn-primary:hover {
      background: linear-gradient(135deg, #0d7a79, #2bb8b7);
      transform: translateY(-2px);
      box-shadow: 0 6px 16px rgba(16, 159, 158, 0.4);
    }
    
    .metadata-container {
      background: linear-gradient(135deg, #ffffff, #f8f9fa);
      border-radius: 20px;
      padding: 30px;
      margin: 20px;
      box-shadow: 0 8px 25px rgba(0,0,0,0.15);
    }
    
    .metadata-header {
      text-align: center;
      color: #109f9e;
      font-family: 'Montserrat', sans-serif;
      font-weight: bold;
      margin-bottom: 30px;
      font-size: 32px;
      text-shadow: 2px 2px 4px rgba(0,0,0,0.1);
    }
    
    .metadata-section {
      background: linear-gradient(135deg, #f8f9fa, #e9ecef);
      border-radius: 15px;
      padding: 25px;
      margin: 20px 0;
      border-left: 5px solid #109f9e;
      box-shadow: 0 4px 15px rgba(0,0,0,0.1);
    }
    
    .metadata-section-title {
      color: #2c3e50;
      font-weight: bold;
      font-size: 20px;
      margin-bottom: 15px;
      border-bottom: 2px solid #109f9e;
      padding-bottom: 10px;
    }
    
    .metadata-content {
      color: #666;
      line-height: 1.8;
      font-size: 15px;
    }
    
    .metadata-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
      gap: 20px;
      margin-top: 20px;
    }
    
    .metadata-card {
      background: linear-gradient(135deg, #109f9e, #3ad6d5);
      color: white;
      border-radius: 15px;
      padding: 20px;
      box-shadow: 0 6px 20px rgba(16, 159, 158, 0.3);
      transition: all 0.3s ease;
    }
    
    .metadata-card:hover {
      transform: translateY(-5px);
      box-shadow: 0 10px 30px rgba(16, 159, 158, 0.4);
    }
    
    .metadata-card-title {
      font-weight: bold;
      font-size: 18px;
      margin-bottom: 10px;
    }
    
    .metadata-card-content {
      font-size: 14px;
      opacity: 0.9;
    }
    .metadata-table-container {
      background: white;
      border-radius: 15px;
      padding: 20px;
      margin: 20px 0;
      box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      overflow-x: auto;
    }
    
    .metadata-table {
      width: 100%;
      border-collapse: collapse;
      margin-top: 15px;
      font-size: 14px;
    }
    
    .metadata-table th {
      background: linear-gradient(135deg, #109f9e, #3ad6d5);
      color: white;
      padding: 12px 8px;
      text-align: center;
      font-weight: bold;
      border: 1px solid #ddd;
      font-size: 12px;
    }
    
    .metadata-table td {
      padding: 10px 8px;
      border: 1px solid #ddd;
      text-align: center;
      vertical-align: middle;
    }
    
    .metadata-table tbody tr:nth-child(even) {
      background-color: #f8f9fa;
    }
    
    .metadata-table tbody tr:hover {
      background-color: #e9ecef;
      transition: background-color 0.3s ease;
    }
    
    @media (max-width: 768px) {
      .metadata-table {
        font-size: 12px;
      }
      
      .metadata-table th,
      .metadata-table td {
        padding: 8px 4px;
      }
    }
    
    
    .leaflet-container {
      height: 500px !important;
      width: 100% !important;
      border-radius: 10px;
      box-shadow: 0 4px 15px rgba(0,0,0,0.1);
    }
    
    .box-body {
      overflow: hidden;
    }
    
    .box-body .leaflet-container {
      max-width: 100%;
      max-height: 500px;
    }
    
    "
        )
      )
    ),
    
    tabItems(
      tabItem(
        tabName = "home",
        div(class = "home-container",
            h1("Selamat Datang di Dashboard Karhutla Indonesia", class = "welcome-header"),
            
            # Summary Statistics Row with Different Colors
            fluidRow(
              column(3,
                     div(class = "stats-card stats-card-temperature",
                         div(paste(round(mean(temp_data$Suhu_Rata, na.rm = TRUE), 1), "°C"), class = "stats-number"),
                         div("Rata-rata Suhu", class = "stats-label"),
                         div("Indonesia 2015-2024", class = "stats-description")
                     )
              ),
              column(3,
                     div(class = "stats-card stats-card-fire",
                         div(paste(format(round(mean(fire_data$luas_kebakaran[fire_data$provinsi == "Indonesia"], na.rm = TRUE)), big.mark = ","), "Ha"), class = "stats-number"),
                         div("Rata-rata Luas Kebakaran", class = "stats-label"),
                         div("Hektar per tahun (2015-2024)", class = "stats-description")
                     )
              ),
              column(3,
                     div(class = "stats-card stats-card-carbon",
                         div(paste(format(round(mean(carbon_data$emisi_karbon[carbon_data$provinsi == "Indonesia"], na.rm = TRUE)/1000000, 1), big.mark = ","), "Mt"), class = "stats-number"),
                         div("Rata-rata Emisi Karbon", class = "stats-label"),
                         div("Mega ton CO2 per tahun (2015-2024)", class = "stats-description")
                     )
              ),
              column(3,
                     div(class = "stats-card stats-card-province",
                         div(length(unique(fire_data$provinsi)) - 1, class = "stats-number"), # -1 to exclude "Indonesia"
                         div("Jumlah Provinsi", class = "stats-label"),
                         div("Republik Indonesia", class = "stats-description")
                     )
              )
            ),
            
            # Video Section
            fluidRow(
              column(12,
                     div(class = "video-container",
                         h3("🎥 Video Penjelasan Dashboard", class = "video-header"),
                         div(class = "video-responsive",
                             tags$iframe(
                               src = "https://youtube.com/embed/pc0VWWSE9ls",
                               frameborder = "0",
                               allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
                               allowfullscreen = TRUE
                             )
                         )
                     )
              )
            ),
            
            # User Guide Section
            fluidRow(
              column(12,
                     div(class = "user-guide-container",
                         h3("📖 Panduan Penggunaan Dashboard", class = "user-guide-header"),
                         div(class = "user-guide-text",
                             p("Selamat datang di Dashboard Kebakaran Hutan dan Lahan Indonesia! Berikut adalah panduan singkat untuk menggunakan dashboard ini:"),
                             tags$ol(
                               tags$li(strong("Menu Home: "), "Halaman utama yang menampilkan ringkasan statistik dan informasi umum dashboard."),
                               tags$li(strong("Menu Grafik: "), "Berisi visualisasi data dalam bentuk bar chart dan line chart untuk:"),
                               tags$ul(
                                 tags$li("Luas Kebakaran Hutan - Analisis per provinsi dan tren waktu"),
                                 tags$li("Emisi Karbon - Visualisasi emisi CO2 per provinsi"),
                                 tags$li("Suhu Udara - Tren suhu rata-rata Indonesia")
                               ),
                               tags$li(strong("Menu Ranking Nasional: "), "Tabel interaktif yang menampilkan ranking provinsi berdasarkan data kebakaran hutan dan emisi karbon."),
                               tags$li(strong("Menu Peta Choropleth: "), "Visualisasi spasial data luas kebakaran hutan dan lahan serta emisi karbon di Indonesia dalam bentuk peta interaktif."),
                               tags$li(strong("Menu Analisis Statistik: "), "Fitur analisis regresi lanjutan untuk eksplorasi hubungan antar variabel."),
                               tags$li(strong("Menu Metadata: "), "Informasi detail tentang sumber data, metodologi, dan kualitas data."),
                               tags$li(strong("Menu About Us: "), "Informasi tentang tim pengembang dashboard.")
                             ),
                             br(),
                             p(strong("Tips Penggunaan:")),
                             tags$ul(
                               tags$li("Gunakan dropdown untuk memilih tahun atau provinsi yang ingin dianalisis"),
                               tags$li("Hover pada grafik untuk melihat detail data"),
                               tags$li("Klik pada peta untuk informasi lebih detail"),
                               tags$li("Gunakan fitur search dan filter pada tabel ranking"),
                               tags$li("Pada menu analisis statistik pengguna dapat menginput data sendiri dengan ekstensi .csv")
                             )
                         )
                     )
              )
            ),
            
            # Project Information
            fluidRow(
              column(6,
                     div(class = "info-section",
                         h3("Tentang Dashboard", class = "info-header"),
                         p("Dashboard ini menyajikan visualisasi data iklim Indonesia yang mencakup analisis suhu udara, luas kebakaran hutan, dan emisi karbon dari tahun 2015-2024. Data disajikan dalam berbagai format grafik interaktif, peta choropleth, dan analisis statistik untuk memberikan pemahaman komprehensif tentang kondisi iklim Indonesia.", class = "info-text"),
                         br(),
                         p("Fitur utama dashboard meliputi:", class = "info-text"),
                         tags$ul(
                           tags$li("Visualisasi grafik bar dan line chart untuk setiap variabel"),
                           tags$li("Ranking nasional berdasarkan provinsi"),
                           tags$li("Peta choropleth untuk visualisasi spasial"),
                           tags$li("Analisis statistik"),
                           style = "color: #666; font-size: 14px;"
                         )
                     )
              ),
              column(6,
                     div(class = "timeline-container",
                         h3("Timeline Pengembangan Project", class = "timeline-header"),
                         div(class = "timeline",
                             div(class = "timeline-item",
                                 div("27 April 2025", class = "timeline-date"),
                                 div("Menentukan tema ", class = "timeline-content")
                             ),
                             div(class = "timeline-item",
                                 div("30 April 2025", class = "timeline-date"),
                                 div("Membuat  rancangan  dahsboard dan rancangan analisis", class = "timeline-content")
                             ),
                             div(class = "timeline-item",
                                 div("28 Mei 2025", class = "timeline-date"),
                                 div("Pengumpulan data", class = "timeline-content")
                             ),
                             div(class = "timeline-item",
                                 div("10 Juni 2025", class = "timeline-date"),
                                 div("Analisis statistik", class = "timeline-content")
                             ),
                             div(class = "timeline-item",
                                 div("19 Juni 2025", class = "timeline-date"),
                                 div("Pembuatan frontend dashboard", class = "timeline-content")
                             ),
                             div(class = "timeline-item",
                                 div("10 Juli 2025", class = "timeline-date"),
                                 div("Pembuatan backend dashboard", class = "timeline-content")
                             ),
                             div(class = "timeline-item",
                                 div("13 Juli 2025", class = "timeline-date"),
                                 div("Finalisasi dashboard dan deployment", class = "timeline-content")
                             )
                         )
                     )
              )
            )
        )
      ),
      
      # KEBAKARAN TAB
      tabItem(tabName = "kebakaran",
              
              fluidRow(
                # Bar Chart Section
                box(
                  column(
                    width = 12,
                    tags$div(
                      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                      selectInput(
                        "dropdownKebakaran_bar",
                        label = NULL,
                        choices = c(2015:2024),
                        selected = 2024,
                        width = "200px"
                      )
                    )
                  ),
                  title = "Bar Chart - Luas Kebakaran Hutan dan Lahan per Provinsi",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  div(class = "chart-container",
                      plotlyOutput("barChartKebakaran", height = "600px")
                  )
                )
              ),
              
              fluidRow(
                div(class = "summary-stats-container",
                    h5("📊 Ringkasan Statistik Bar Chart", class = "summary-header"),
                    uiOutput("barChartSummary")
                )
              ),
              
              fluidRow(
                # Line Chart Section
                box(
                  column(
                    width = 12,
                    tags$div(
                      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                      selectInput(
                        "dropdownKebakaran_line",
                        label = NULL,
                        choices = sort(unique(fire_data$provinsi)),
                        selected = "Indonesia",
                        width = "200px"
                      )
                    )
                  ),
                  title = "Line Chart - Trend Luas Kebakaran Hutan dan Lahan dari Waktu ke Waktu",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  div(class = "chart-container",
                      plotlyOutput("lineChartKebakaran", height = "500px")
                  )
                )
              ),
              
              fluidRow(
                div(class = "summary-stats-container",
                    h5("📈 Ringkasan Statistik Line Chart", class = "summary-header"),
                    uiOutput("lineChartSummary")
                )
              )
      ),
      
      # Emkarbon
      tabItem(tabName = "karbon",
              
              fluidRow(
                # Bar Chart Section
                box(
                  column(
                    width = 12,
                    tags$div(
                      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                      selectInput(
                        "dropdownKarbon_bar",
                        label = NULL,
                        choices = c(2015:2024),
                        selected = 2024,
                        width = "200px"
                      )
                    )
                  ),
                  title = "Bar Chart - Emisi Karbon per Provinsi",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  div(class = "chart-container",
                      plotlyOutput("barChartKarbon", height = "600px")
                  )
                )
              ),
              fluidRow(
                div(class = "summary-stats-container",
                    h5("📊 Ringkasan Statistik Bar Chart Emisi Karbon", class = "summary-header"),
                    uiOutput("barChartKarbonSummary")
                )
              ),
              
              fluidRow(
                # Line Chart Section 
                box(
                  column(
                    width = 12,
                    tags$div(
                      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                      selectInput(
                        "dropdownKarbon_line",
                        label = NULL,
                        choices = sort(unique(carbon_data$provinsi)),
                        selected = "Indonesia",
                        width = "200px"
                      )
                    )
                  ),
                  title = "Line Chart - Trend Emisi Karbon dari Waktu ke Waktu",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  div(class = "chart-container",
                      plotlyOutput("lineChartKarbon", height = "500px")
                  )
                )
              ),
              fluidRow(
                div(class = "summary-stats-container",
                    h5("📈 Ringkasan Statistik Line Chart Emisi Karbon", class = "summary-header"),
                    uiOutput("lineChartKarbonSummary")
                )
              )
      ),
      
      # SUhu
      tabItem(tabName = "suhu",
              
              fluidRow(
                # Line Chart Section
                box(
                  column(
                    width = 12,
                    tags$div(
                      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                      
                      div(style = "width: 200px;") 
                    )
                  ),
                  title = "Line Chart - Trend Suhu Rata-rata Indonesia (2015-2024)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  div(class = "chart-container",
                      plotlyOutput("lineChartSuhu", height = "500px")
                  )
                )
              ),
              fluidRow(
                div(class = "summary-stats-container",
                    h5("🌡️ Ringkasan Statistik Suhu Indonesia", class = "summary-header"),
                    uiOutput("lineChartSuhuSummary")
                )
              )
      ),
      
      # Ranking
      tabItem(tabName = "ranking", 
              fluidRow(
                column(width = 3, 
                       wellPanel(
                         selectInput("yearSelect", 
                                     label = "Pilih Tahun", 
                                     choices = unique(dataprovlengkap$tahun),
                                     selected = max(dataprovlengkap$tahun),
                                     width = "100%")
                       )
                ),
                column(width = 9, 
                       box(
                         title = "Ranking Nasional Luas Karhutla dan Emisi Karbon",
                         status = "primary", 
                         solidHeader = TRUE, 
                         width = NULL, 
                         collapsible = FALSE,
                         DTOutput("tableRanking")
                       )
                )
              )
      ),
      
      # CHOROPLETH 
      tabItem(tabName = "choropleth",
              fluidRow(
                box(
                  column(
                    width = 12,
                    tags$div(
                      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                      
                      div(style = "display: flex; gap: 15px;",
                          selectInput(
                            "dropdownPetaJenis",
                            label = "Pilih Variabel:",
                            choices = list(
                              "Luas Karhutla" = "kebakaran",
                              "Emisi Karbon" = "karbon"
                            ),
                            selected = "kebakaran",
                            width = "200px"
                          ),
                          selectInput(
                            "dropdownPetaTahun",
                            label = "Pilih Tahun:",
                            choices = c(2015:2024),
                            selected = 2024,
                            width = "150px"
                          )
                      )
                    )
                  ),
                  title = "🗺️ Peta Choroplet",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  leafletOutput("choroplethCombined", height = "500px")
                )
              ),
            
              fluidRow(
                div(class = "summary-stats-container",
                    h5("🗺️ Ringkasan Peta Choropleth", class = "summary-header"),
                    uiOutput("choroplethCombinedSummary")
                )
              )
      ),
      
      tabItem(tabName = "statistik",
              fluidRow(
                # Sidebar untuk kontrol regresi
                column(3,
                       wellPanel(
                         fileInput("datafile", "Upload File CSV", accept = ".csv"),
                         uiOutput("xvar_ui"),
                         uiOutput("yvar_ui"),
                         actionButton("run_model", "Jalankan Regresi", class = "btn btn-primary")
                       )
                ),
                column(9,
                       tabsetPanel(
                         tabPanel("Ringkasan Model",
                                  verbatimTextOutput("model_summary"),
                                  withMathJax(),
                                  uiOutput("persamaan_regresi_latex")
                         ),
                         tabPanel("Plot Regresi",
                                  plotOutput("regression_plot")
                         ),
                         tabPanel("Asumsi Regresi",
                                  h4("1. Normalitas Residual"),
                                  splitLayout(
                                    cellWidths = c("50%", "50%"),
                                    plotOutput("qq_plot"),
                                    plotOutput("hist_residual")
                                  ),
                                  verbatimTextOutput("shapiro_test"),
                                  textOutput("shapiro_keputusan"),
                                  hr(),
                                  
                                  h4("2. Homoskedastisitas"),
                                  plotOutput("resid_fitted_plot"),
                                  verbatimTextOutput("bp_test"),
                                  textOutput("bp_keputusan"),
                                  hr(),
                                  
                                  h4("3. Autokorelasi Residual"),
                                  plotOutput("residual_lag_plot"),
                                  verbatimTextOutput("dw_test"),
                                  textOutput("dw_keputusan"),
                                  hr(),
                                  
                                  conditionalPanel(
                                    condition = "output.show_vif == true",
                                    h4("4. Multikolinearitas"),
                                    verbatimTextOutput("vif_result"),
                                    textOutput("vif_keputusan")
                                  )
                         ),
                         tabPanel("Ringkasan Asumsi",
                                  br(),
                                  h4("🔍 Ringkasan Hasil Pengujian Asumsi Regresi"),
                                  tableOutput("ringkasan_asumsi"),
                                  br(),
                                  textOutput("keputusan_akhir"),
                                  br(),
                                  downloadButton("download_report", "📥 Unduh Laporan", class = "btn btn-sm btn-primary")
                         )
                       )
                )
              )
      ),
      
      tabItem(tabName = "metadata",
              div(class = "metadata-container",
                  h1("📋 Metadata Dashboard Karhutla Indonesia", class = "metadata-header"),
                  
                  div(class = "metadata-section",
                      h3("🗂 Sumber Data", class = "metadata-section-title"),
                      div(class = "metadata-content",
                          p("Dashboard ini menggunakan data dari berbagai sumber resmi pemerintah Indonesia:"),
                          tags$ul(
                            tags$li("Badan Meteorologi, Klimatologi, dan Geofisika - Data suhu rata-rata nasional"),
                            tags$li("Kementerian Lingkungan Hidup dan Kehutanan - Data luas kebakaran hutan dan lahan"),
                            tags$li("Kementerian Lingkungan Hidup dan Kehutanan - Data emisi karbon per provinsi")
                          ),
                          p(HTML(
                            "Selain itu, data spasial batas wilayah administratif Indonesia (level provinsi) dalam format GeoJSON juga digunakan untuk visualisasi peta.Data spasial batas wilayah administratif Indonesia (level provinsi) dalam format GeoJSON diperoleh dari repositori GitHub pihak ketiga dan telah disesuaikan untuk kebutuhan visualisasi dalam dashboard. 
  Sumber asli dapat diakses melalui: <a href='https://github.com/superpikar/indonesia-geojson/blob/master/indonesia.geojson' target='_blank'>superpikar/indonesia-geojson</a>."
                          ))
                          
                          )
                      ),
                  
                  
                  div(class = "metadata-section",
                      h3("📅 Cakupan Data", class = "metadata-section-title"),
                      div(class = "metadata-content",
                          p("Informasi mengenai periode dan cakupan geografis data yang digunakan:"),
                          tags$ul(
                            tags$li("Periode waktu: 2015 - 2024 (10 tahun terakhir)"),
                            tags$li("Cakupan geografis: Seluruh provinsi di Indonesia (34 provinsi)"),
                            tags$li("Frekuensi update: Data diperbarui setiap tahun"),
                            tags$li("Unit pengukuran: Hektar (Ha) untuk luas karhutla, ton CO2 untuk emisi karbon, Celsius (°C) untuk suhu")
                          )
                      )
                  ),
                  
                  div(class = "metadata-table-container",
                      tags$table(class = "metadata-table",
                                 tags$thead(
                                   tags$tr(
                                     tags$th("No"),
                                     tags$th("Tipe"),
                                     tags$th("Nama Variabel"),
                                     tags$th("Label"),
                                     tags$th("Measure"),
                                     tags$th("Data Unit"),
                                     tags$th("Unduh Data")
                                   )
                                 ),
                                 tags$tbody(
                                   tags$tr(
                                     tags$td("1"),
                                     tags$td("Numerik"),
                                     tags$td("Emisi karbon (CO2)"),
                                     tags$td("Y"),
                                     tags$td("Scale"),
                                     tags$td("Mega ton (Mt)"),
                                     tags$td(downloadButton("download_emisi", "Unduh", class = "btn btn-sm btn-primary"))
                                   ),
                                   tags$tr(
                                     tags$td("2"),
                                     tags$td("Numerik"),
                                     tags$td("Luas Kebakaran Hutan dan Lahan"),
                                     tags$td("X1"),
                                     tags$td("Scale"),
                                     tags$td("Hektar are (Ha)"),
                                     tags$td(downloadButton("download_kebakaran", "Unduh", class = "btn btn-sm btn-primary"))
                                   ),
                                   tags$tr(
                                     tags$td("3"),
                                     tags$td("Numerik"),
                                     tags$td("Suhu Rata Rata"),
                                     tags$td("X2"),
                                     tags$td("Scale"),
                                     tags$td("Celsius (°C)"),
                                     tags$td(downloadButton("download_suhu", "Unduh", class = "btn btn-sm btn-primary"))
                                   )
                                 ),
                                 tags$tr(
                                   tags$td("4"),
                                   tags$td("Spasial"),
                                   tags$td("Peta Provinsi Indonesia (GeoJSON)"),
                                   tags$td("-"),
                                   tags$td("-"),
                                   tags$td("-"),
                                   tags$td(downloadButton("download_geojson", "Unduh", class = "btn btn-sm btn-primary"))
                                 )
                                 
                      )
                  ),
                  div(class = "metadata-section",
                      h3("✅ Kualitas Data", class = "metadata-section-title"),
                      div(class = "metadata-content",
                          p("Informasi mengenai kualitas dan validitas data yang digunakan:"),
                          tags$ul(
                            tags$li("Akurasi: Data telah divalidasi dan dibersihkan dari outlier ekstrem"),
                            tags$li("Kelengkapan: Tingkat kelengkapan data > 95% untuk semua variabel"),
                            tags$li("Konsistensi: Standardisasi unit dan format data antar sumber"),
                            tags$li("Keterkinian: Data terbaru hingga tahun 2024"),
                            tags$li("Reliabilitas: Sumber data dari lembaga resmi dan terverifikasi")
                          )
                      )
                  ),
                  fluidRow(
                    div(class = "metadata-grid",
                        div(class = "metadata-card",
                            div("🔧 Teknologi", class = "metadata-card-title"),
                            div("Dashboard dibangun menggunakan R Shiny dengan library plotly untuk visualisasi interaktif, leaflet untuk peta choropleth, dan DT untuk tabel interaktif.", class = "metadata-card-content")
                        ),
                        div(class = "metadata-card",
                            div("📊 Metodologi", class = "metadata-card-title"),
                            div("Analisis menggunakan metode statistik deskriptif, regresi linear, dan visualisasi spasial untuk mengidentifikasi pola dan tren data lingkungan.", class = "metadata-card-content")
                        ),
                        div(class = "metadata-card",
                            div("🎯 Tujuan", class = "metadata-card-title"),
                            div("Menyediakan platform analisis data lingkungan yang komprehensif untuk mendukung pengambilan keputusan berbasis data dalam pengelolaan lingkungan.", class = "metadata-card-content")
                        )
                    )
                  ),
                  
                  
                  div(class = "metadata-section",
                      h3("📞 Informasi Kontak", class = "metadata-section-title"),
                      div(class = "metadata-content",
                          p("Untuk pertanyaan, saran, atau laporan masalah terkait dashboard:"),
                          tags$ul(
                            tags$li("Tim Pengembang: Kelompok 4 - Analisis Data Karhutla"),
                            tags$li("Institusi: Politeknik Statistika STIS"),
                          )
                      )
                  )
              )
      ),
      
      tabItem(tabName = "aboutus",
              div(class = "about-container",
                  h1("👥 Tentang Tim Pengembang Dashboard", class = "about-header"),
                  p("Kelompok 4 | Kelas 2KS3", style = "text-align: center; color: #666; font-size: 20px; margin-bottom: 30px;"),
                  fluidRow(
                    column(12,
                           div(class = "member-card",
                               fluidRow(
                                 column(3,
                                        tags$img(src = "foto1.png", class = "member-photo", alt = "Foto Gipa")
                                 ),
                                 column(9,
                                        div(class = "member-name", "Rahman Al Gifary"),
                                        div(class = "member-details",
                                            p(strong("Nama Panggilan: "), "Gifa"),
                                            p(strong("NIM: "), "222313328"),
                                            p(strong("No. Presensi: "), "31"),
                                            div(class = "member-motto",
                                                p(strong("Moto: "), em("\"natus vincere\""))
                                            )
                                        )
                                 )
                               )
                           )
                    )
                  ),
                  fluidRow(
                    column(12,
                           div(class = "member-card",
                               fluidRow(
                                 column(3,
                                        tags$img(src = "foto2.png", class = "member-photo", alt = "Foto Aul")
                                 ),
                                 column(9,
                                        div(class = "member-name", "Aulia Ul Hasanah"),
                                        div(class = "member-details",
                                            p(strong("Nama Panggilan: "), "Aul"),
                                            p(strong("NIM: "), "222313000"),
                                            p(strong("No. Presensi: "), "6"),
                                            div(class = "member-motto",
                                                p(strong("Moto: "), em("\"Got dreams to chase, not people to impress\""))
                                            )
                                        )
                                 )
                               )
                           )
                    )
                  ),
                  fluidRow(
                    column(12,
                           div(class = "member-card",
                               fluidRow(
                                 column(3,
                                        tags$img(src = "foto3.png", class = "member-photo", alt = "Foto Tesa")
                                 ),
                                 column(9,
                                        div(class = "member-name", "Ilham Tesa Nur Mazua"),
                                        div(class = "member-details",
                                            p(strong("Nama Panggilan: "), "Tesa"),
                                            p(strong("NIM: "), "222313133"),
                                            p(strong("No. Presensi: "), "17"),
                                            div(class = "member-motto",
                                                p(strong("Moto: "), em("\"When you lose something, you can't replace\""))
                                            )
                                        )
                                 )
                               )
                           )
                    )
                  )
              ),
              
              # Team Information
              div(class = "about-container",
                  h3("ⓘ️ Informasi Tim", class = "info-header"),
                  fluidRow(
                    column(4,
                           h4("🎓 Institusi", style = "color: #109f9e;"),
                           p("Politeknik Statistika STIS"),
                           p("Program Studi D-4 Komputasi Statistik", style = "font-size: 12px; color: #666;")
                    ),
                    column(4,
                           h4("📚 Mata Kuliah", style = "color: #109f9e;"),
                           p("Komputasi Statistik"),
                           p("Semester Genap 2024/2025", style = "font-size: 12px; color: #666;")
                    ),
                    column(4,
                           h4("👨‍🏫 Dosen Pembimbing", style = "color: #109f9e;"),
                           p("Yuliagnis Transver Wijaya, S.ST., M.Sc.")
                    )
                  ),
                  br(),
              )
      )
    )
  )
)
    
    server <- function(input, output, session) {
      
      format_emission <- function(x) {
        if (x >= 1e9) {
          paste0(round(x/1e9, 1), " Gt")
        } else if (x >= 1e6) {
          paste0(round(x/1e6, 1), " Mt")
        } else if (x >= 1e3) {
          paste0(round(x/1e3, 1), " Kt")
        } else {
          paste0(round(x, 0), " ton")
        }
      }
      output$barChartKebakaran <- renderPlotly({
        req(input$dropdownKebakaran_bar)
        selected_year <- as.numeric(input$dropdownKebakaran_bar)
        
        chart_data <- fire_data %>%
          filter(tahun == selected_year, provinsi != "Indonesia") %>%
          arrange(luas_kebakaran)
        
        p <- plot_ly(
          data = chart_data,
          x = ~luas_kebakaran,
          y = ~reorder(provinsi, luas_kebakaran),
          type = 'bar',
          orientation = 'h',
          marker = list(
            color = ~luas_kebakaran,
            colorscale = 'Reds',
            colorbar = list(title = "Luas (Ha)")
          ),
          hovertemplate = paste(
            "<b>%{y}</b><br>",
            "Luas Karhutla: %{x:,.0f} Ha<br>",
            "Tahun: ", selected_year,
            "<extra></extra>"
          )
        ) %>%
          layout(
            title = list(
              text = paste("Luas Kebakaran Hutan dan Lahan per Provinsi -", selected_year),
              font = list(size = 18, color = '#2C3E50'),
              xanchor = "center",
              x = 0.5
            ),
            xaxis = list(
              title = "Luas Karhutla (Hektar)",
              titlefont = list(size = 14),
              tickfont = list(size = 12)
            ),
            yaxis = list(
              title = "",
              tickfont = list(size = 12)
            ),
            plot_bgcolor = '#FFF',
            paper_bgcolor = '#FFF',
            margin = list(l = 120, r = 40, t = 80, b = 40)
          ) %>%
          config(displayModeBar = FALSE)
        
        return(p)
      })
      
      output$lineChartKebakaran <- renderPlotly({
        req(input$dropdownKebakaran_line)
        selected_province <- input$dropdownKebakaran_line
        
        chart_data <- fire_data %>%
          filter(provinsi == selected_province)
        
        p <- plot_ly(
          data = chart_data,
          x = ~tahun,
          y = ~luas_kebakaran,
          type = 'scatter',
          mode = 'lines+markers',
          line = list(
            color = '#E74C3C',
            width = 3
          ),
          marker = list(
            color = '#C0392B',
            size = 8,
            line = list(color = 'white', width = 2)
          ),
          hovertemplate = paste(
            "<b>", selected_province, "</b><br>",
            "Tahun: %{x}<br>",
            "Luas Karhutla: %{y:,.0f} Ha<br>",
            "<extra></extra>"
          )
        ) %>%
          layout(
            title = list(
              text = paste("Tren Luas Karhutla di", selected_province, "(2015–2024)"),
              font = list(size = 18, color = '#2C3E50'),
              xanchor = "center",
              x = 0.5
            ),
            xaxis = list(
              title = "Tahun",
              titlefont = list(size = 14),
              tickfont = list(size = 12),
              dtick = 1
            ),
            yaxis = list(
              title = "Luas Karhutla (Hektar)",
              titlefont = list(size = 14),
              tickfont = list(size = 12)
            ),
            plot_bgcolor = '#FFF',
            paper_bgcolor = '#FFF',
            margin = list(l = 80, r = 40, t = 80, b = 60),
            showlegend = FALSE
          ) %>%
          config(displayModeBar = FALSE)
        
        return(p)
      })
      
      

      output$barChartSummary <- renderUI({
        req(input$dropdownKebakaran_bar)  # Ensure input exists
        selected_year <- as.numeric(input$dropdownKebakaran_bar)
        
        year_data <- fire_data %>%
          filter(tahun == selected_year, provinsi != "Indonesia")
        
        national_data <- fire_data %>%
          filter(tahun == selected_year, provinsi == "Indonesia")
        
        if(nrow(year_data) > 0) {
          max_province <- year_data %>%
            filter(luas_kebakaran == max(luas_kebakaran, na.rm = TRUE)) %>%
            slice(1)  # Take first if multiple
          
          min_province <- year_data %>%
            filter(luas_kebakaran == min(luas_kebakaran, na.rm = TRUE)) %>%
            slice(1)  # Take first if multiple
          
          div(class = "summary-grid",
              div(class = "summary-card summary-card-highest",
                  div("🔥", class = "summary-icon"),
                  div("Provinsi Tertinggi", class = "summary-label"),
                  div(paste(max_province$provinsi), class = "summary-value"),
                  div(paste(format(max_province$luas_kebakaran, big.mark = ","), "Ha"), class = "summary-value")
              ),
              div(class = "summary-card summary-card-lowest",
                  div("🌱", class = "summary-icon"),
                  div("Provinsi Terendah", class = "summary-label"),
                  div(paste(min_province$provinsi), class = "summary-value"),
                  div(paste(format(min_province$luas_kebakaran, big.mark = ","), "Ha"), class = "summary-value")
              ),
              div(class = "summary-card summary-card-total",
                  div("🇮🇩", class = "summary-icon"),
                  div("Total Nasional", class = "summary-label"),
                  div(paste(format(national_data$luas_kebakaran, big.mark = ","), "Ha"), class = "summary-value")
              ),
              div(class = "summary-card summary-card-average",
                  div("📊", class = "summary-icon"),
                  div("Rata-rata Provinsi", class = "summary-label"),
                  div(paste(format(round(mean(year_data$luas_kebakaran, na.rm = TRUE)), big.mark = ","), "Ha"), class = "summary-value")
              )
          )
        } else {
          div("Data tidak tersedia untuk tahun ini")
        }
      })
      
      output$lineChartSummary <- renderUI({
        req(input$dropdownKebakaran_line)
        selected_province <- input$dropdownKebakaran_line
        province_data <- fire_data %>%
          filter(provinsi == selected_province)
        
        if(nrow(province_data) > 0) {
          max_year <- province_data %>%
            filter(luas_kebakaran == max(luas_kebakaran, na.rm = TRUE)) %>%
            slice(1)
          
          min_year <- province_data %>%
            filter(luas_kebakaran == min(luas_kebakaran, na.rm = TRUE)) %>%
            slice(1)  
          
          avg_value <- mean(province_data$luas_kebakaran, na.rm = TRUE)
          
          first_year_value <- province_data$luas_kebakaran[province_data$tahun == min(province_data$tahun)]
          last_year_value <- province_data$luas_kebakaran[province_data$tahun == max(province_data$tahun)]
          model <- lm(luas_kebakaran ~ tahun, data = province_data)
          slope <- coef(model)["tahun"]
          
          trend_direction <- ifelse(slope > 0, "Meningkat ↗️",
                                    ifelse(slope < 0, "Menurun ↘️", "Stabil ➖"))
          
          div(class = "summary-grid",
              div(class = "summary-card summary-card-highest",
                  div("📈", class = "summary-icon"),
                  div("Tahun Tertinggi", class = "summary-label"),
                  div(paste(max_year$tahun), class = "summary-value"),
                  div(paste(format(max_year$luas_kebakaran, big.mark = ","), "Ha"), class = "summary-value")
              ),
              div(class = "summary-card summary-card-lowest",
                  div("📉", class = "summary-icon"),
                  div("Tahun Terendah", class = "summary-label"),
                  div(paste(min_year$tahun), class = "summary-value"),
                  div(paste(format(min_year$luas_kebakaran, big.mark = ","), "Ha"), class = "summary-value")
              ),
              div(class = "summary-card summary-card-average",
                  div("📊", class = "summary-icon"),
                  div("Rata-rata 2015-2024", class = "summary-label"),
                  div(paste(format(round(avg_value), big.mark = ","), "Ha"), class = "summary-value")
              ),
              div(class = "summary-card summary-card-trend",
                  div("📊", class = "summary-icon"),
                  div("Trend", class = "summary-label"),
                  div(trend_direction, class = "summary-value")
              )
          )
        } else {
          div("Data tidak tersedia untuk provinsi ini")
        }
      })
    
      output$barChartKarbon <- renderPlotly({
        req(input$dropdownKarbon_bar)
        selected_year <- as.numeric(input$dropdownKarbon_bar)
        
        chart_data <- carbon_data %>%
          filter(tahun == selected_year, provinsi != "Indonesia") %>%
          arrange(emisi_karbon)
        
        p <- plot_ly(
          data = chart_data,
          x = ~emisi_karbon,
          y = ~reorder(provinsi, emisi_karbon),
          type = 'bar',
          orientation = 'h',
          marker = list(
            color = ~emisi_karbon,
            colorscale = 'Purples',
            colorbar = list(title = "Emisi (ton CO₂)")
          ),
          hovertemplate = paste(
            "<b>%{y}</b><br>",
            "Emisi Karbon: %{x:,.0f} ton CO₂<br>",
            "Tahun: ", selected_year,
            "<extra></extra>"
          )
        ) %>%
          layout(
            title = list(
              text = paste("Emisi Karbon per Provinsi -", selected_year),
              font = list(size = 18, color = '#2C3E50'),
              xanchor = "center",
              x = 0.5
            ),
            xaxis = list(
              title = "Emisi Karbon (ton CO₂)",
              titlefont = list(size = 14),
              tickfont = list(size = 12)
            ),
            yaxis = list(
              title = "",
              tickfont = list(size = 12)
            ),
            plot_bgcolor = '#FFF',
            paper_bgcolor = '#FFF',
            margin = list(l = 120, r = 40, t = 80, b = 40)
          ) %>%
          config(displayModeBar = FALSE)
        
        return(p)
      })
      
      
      
      output$lineChartKarbon <- renderPlotly({
        req(input$dropdownKarbon_line)
        selected_province <- input$dropdownKarbon_line
        
        chart_data <- carbon_data %>%
          filter(provinsi == selected_province)
        
        p <- plot_ly(
          data = chart_data,
          x = ~tahun,
          y = ~emisi_karbon,
          type = 'scatter',
          mode = 'lines+markers',
          line = list(
            color = '#8E44AD',
            width = 3
          ),
          marker = list(
            color = '#4A148C',
            size = 8,
            line = list(color = 'white', width = 2)
          ),
          hovertemplate = paste(
            "<b>", selected_province, "</b><br>",
            "Tahun: %{x}<br>",
            "Emisi Karbon: %{y:,.0f} ton CO₂<br>",
            "<extra></extra>"
          )
        ) %>%
          layout(
            title = list(
              text = paste("Tren Emisi Karbon di", selected_province, "(2015–2024)"),
              font = list(size = 18, color = '#2C3E50'),
              xanchor = "center",
              x = 0.5
            ),
            xaxis = list(
              title = "Tahun",
              titlefont = list(size = 14),
              tickfont = list(size = 12),
              dtick = 1
            ),
            yaxis = list(
              title = "Emisi Karbon (ton CO₂)",
              titlefont = list(size = 14),
              tickfont = list(size = 12)
            ),
            plot_bgcolor = '#FFF',
            paper_bgcolor = '#FFF',
            margin = list(l = 80, r = 50, t = 80, b = 60),
            showlegend = FALSE
          ) %>%
          config(displayModeBar = FALSE)
        
        return(p)
      })
      
      output$barChartKarbonSummary <- renderUI({
        req(input$dropdownKarbon_bar) 
        selected_year <- as.numeric(input$dropdownKarbon_bar)
        year_data <- carbon_data %>%
          filter(tahun == selected_year, provinsi != "Indonesia")
        
        
        national_data <- carbon_data %>%
          filter(tahun == selected_year, provinsi == "Indonesia")
        
        if(nrow(year_data) > 0) {
          max_province <- year_data %>%
            filter(emisi_karbon == max(emisi_karbon, na.rm = TRUE)) %>%
            slice(1)
          
          min_province <- year_data %>%
            filter(emisi_karbon == min(emisi_karbon, na.rm = TRUE)) %>%
            slice(1)
          
          div(class = "summary-grid",
              div(class = "summary-card summary-card-highest",
                  div("☁️", class = "summary-icon"),
                  div("Provinsi Tertinggi", class = "summary-label"),
                  div(paste(max_province$provinsi), class = "summary-value"),
                  div(paste(format(max_province$emisi_karbon, big.mark = ","), "ton CO2"), class = "summary-value")
              ),
              div(class = "summary-card summary-card-lowest",
                  div("🌿", class = "summary-icon"),
                  div("Provinsi Terendah", class = "summary-label"),
                  div(paste(min_province$provinsi), class = "summary-value"),
                  div(paste(format(min_province$emisi_karbon, big.mark = ","), "ton CO2"), class = "summary-value")
              ),
              div(class = "summary-card summary-card-total",
                  div("🇮🇩", class = "summary-icon"),
                  div("Total Nasional", class = "summary-label"),
                  div(paste(format(national_data$emisi_karbon, big.mark = ","), "ton CO2"), class = "summary-value")
              ),
              div(class = "summary-card summary-card-average",
                  div("📊", class = "summary-icon"),
                  div("Rata-rata Provinsi", class = "summary-label"),
                  div(paste(format(round(mean(year_data$emisi_karbon, na.rm = TRUE)), big.mark = ","), "ton CO2"), class = "summary-value")
              )
          )
        } else {
          div("Data tidak tersedia untuk tahun ini")
        }
      })
      
      
      output$lineChartKarbonSummary <- renderUI({
        req(input$dropdownKarbon_line)  # Ensure input exists
        selected_province <- input$dropdownKarbon_line
        
        
        province_data <- carbon_data %>%
          filter(provinsi == selected_province)
        
        if(nrow(province_data) > 0) {
          max_year <- province_data %>%
            filter(emisi_karbon == max(emisi_karbon, na.rm = TRUE)) %>%
            slice(1)
          
          min_year <- province_data %>%
            filter(emisi_karbon == min(emisi_karbon, na.rm = TRUE)) %>%
            slice(1)
          
          avg_value <- mean(province_data$emisi_karbon, na.rm = TRUE)
          
          
          first_year_value <- province_data$emisi_karbon[province_data$tahun == min(province_data$tahun)]
          last_year_value <- province_data$emisi_karbon[province_data$tahun == max(province_data$tahun)]
          model <- lm(emisi_karbon ~ tahun, data = province_data)
          slope <- coef(model)["tahun"]
          
          trend_direction <- ifelse(slope > 0, "Meningkat ↗️",
                                    ifelse(slope < 0, "Menurun ↘️", "Stabil ➖"))
          
          div(class = "summary-grid",
              div(class = "summary-card summary-card-highest",
                  div("📈", class = "summary-icon"),
                  div("Tahun Tertinggi", class = "summary-label"),
                  div(paste(max_year$tahun), class = "summary-value"),
                  div(paste(format(max_year$emisi_karbon, big.mark = ","), "ton CO2"), class = "summary-value")
              ),
              div(class = "summary-card summary-card-lowest",
                  div("📉", class = "summary-icon"),
                  div("Tahun Terendah", class = "summary-label"),
                  div(paste(min_year$tahun), class = "summary-value"),
                  div(paste(format(min_year$emisi_karbon, big.mark = ","), "ton CO2"), class = "summary-value")
              ),
              div(class = "summary-card summary-card-average",
                  div("📊", class = "summary-icon"),
                  div("Rata-rata 2015-2024", class = "summary-label"),
                  div(paste(format(round(avg_value), big.mark = ","), "ton CO2"), class = "summary-value")
              ),
              div(class = "summary-card summary-card-trend",
                  div("📊", class = "summary-icon"),
                  div("Trend", class = "summary-label"),
                  div(trend_direction, class = "summary-value")
              )
          )
        } else {
          div("Data tidak tersedia untuk provinsi ini")
        }
      })
      
      
      output$lineChartSuhu <- renderPlotly({
        
        p <- plot_ly(
          data = temp_data,
          x = ~tahun,
          y = ~Suhu_Rata,
          type = 'scatter',
          mode = 'lines+markers',
          line = list(
            color = '#FF6B35',
            width = 3
          ),
          marker = list(
            color = '#E74C3C',
            size = 8,
            line = list(color = 'white', width = 2)
          ),
          hovertemplate = paste(
            "<b>Indonesia</b><br>",
            "Tahun: %{x}<br>",
            "Suhu Rata-rata: %{y}°C<br>",
            "<extra></extra>"
          )
        ) %>%
          layout(
            title = list(
              text = "Trend Suhu Rata-rata Indonesia (2015-2024)",
              font = list(size = 16, color = '#2C3E50')
            ),
            xaxis = list(
              title = "Tahun",
              titlefont = list(size = 14),
              tickfont = list(size = 12),
              dtick = 1
            ),
            yaxis = list(
              title = "Suhu Rata-rata (°C)",
              titlefont = list(size = 14),
              tickfont = list(size = 12),
              range = c(min(temp_data$Suhu_Rata) - 0.5, max(temp_data$Suhu_Rata) + 0.5)
            ),
            plot_bgcolor = 'rgba(0,0,0,0)',
            paper_bgcolor = 'rgba(0,0,0,0)',
            margin = list(l = 80, r = 50, t = 80, b = 60),
            showlegend = FALSE
          ) %>%
          config(displayModeBar = FALSE)
        
        return(p)
      })
      

      output$lineChartSuhuSummary <- renderUI({
        
        if(nrow(temp_data) > 0) {
          max_temp <- temp_data %>%
            filter(Suhu_Rata == max(Suhu_Rata, na.rm = TRUE)) %>%
            slice(1)
          
          min_temp <- temp_data %>%
            filter(Suhu_Rata == min(Suhu_Rata, na.rm = TRUE)) %>%
            slice(1)
          
          avg_temp <- mean(temp_data$Suhu_Rata, na.rm = TRUE)
          
          first_year_temp <- temp_data$Suhu_Rata[temp_data$tahun == min(temp_data$tahun)]
          last_year_temp <- temp_data$Suhu_Rata[temp_data$tahun == max(temp_data$tahun)]
          model <- lm(Suhu_Rata ~ tahun, data = province_data)
          slope <- coef(model)["tahun"]
          
          trend_direction <- ifelse(slope > 0, "Meningkat ↗️",
                                    ifelse(slope < 0, "Menurun ↘️", "Stabil ➖"))
          
          div(class = "summary-grid",
              div(class = "summary-card summary-card-highest",
                  div("🌡️", class = "summary-icon"),
                  div("Suhu Tertinggi", class = "summary-label"),
                  div(paste(max_temp$tahun), class = "summary-value"),
                  div(paste0(max_temp$Suhu_Rata, "°C"), class = "summary-value")
              ),
              div(class = "summary-card summary-card-lowest",
                  div("❄️", class = "summary-icon"),
                  div("Suhu Terendah", class = "summary-label"),
                  div(paste(min_temp$tahun), class = "summary-value"),
                  div(paste0(min_temp$Suhu_Rata, "°C"), class = "summary-value")
              ),
              div(class = "summary-card summary-card-average",
                  div("📊", class = "summary-icon"),
                  div("Rata-rata 2015-2024", class = "summary-label"),
                  div(paste0(round(avg_temp, 1), "°C"), class = "summary-value")
              ),
              div(class = "summary-card summary-card-trend",
                  div("📊", class = "summary-icon"),
                  div("Trend", class = "summary-label"),
                  div(trend_direction, class = "summary-value")
              )
          )
        } else {
          div("Data tidak tersedia")
        }
      })
      

      output$choroplethCombined <- renderLeaflet({
        req(input$dropdownPetaJenis, input$dropdownPetaTahun)
        selected_type <- input$dropdownPetaJenis
        selected_year <- as.numeric(input$dropdownPetaTahun)
        
        if(is.null(geojson)) {
          # Fallback jika geojson tidak tersedia
          leaflet() %>%
            addTiles() %>%
            addMarkers(lng = 118, lat = -2,
                       popup = "Data geojson tidak tersedia/") %>%
            setView(lng = 118, lat = -2, zoom = 5)
        } else {
          if(selected_type == "kebakaran") {
            data_filtered <- fire_data %>%
              filter(tahun == selected_year, provinsi != "Indonesia") %>%
              rename(Provinsi = provinsi)
            
            # Join dengan geojson
            data_tergabung <- geojson %>%
              left_join(data_filtered, by = "Provinsi")

            max_val <- quantile(data_tergabung$luas_kebakaran, 1, na.rm = TRUE)
            pal <- colorNumeric(
              palette = colorRampPalette(c("#FFF3CD", "#FFE082", "#FF8A65", "#F44336", "#B71C1C"))(100),
              domain = c(0, max_val)
            )
            
            teks_popup <- paste(
              "<b>Provinsi:</b> ", data_tergabung$Provinsi,
              "<br/><b>Luas Karhutla:</b> ", format(data_tergabung$luas_kebakaran, big.mark = ","), " Ha",
              "<br/><b>Tahun:</b> ", selected_year,
              sep = ""
            ) %>% lapply(htmltools::HTML)
            
            leaflet(data_tergabung) %>%
              addTiles() %>%
              addProviderTiles("Esri.WorldImagery", options = providerTileOptions(opacity = 0.3)) %>%
              addPolygons(
                fillColor = ~pal(luas_kebakaran),
                fillOpacity = 0.8,
                color = "white",
                weight = 2,
                popup = teks_popup,
                highlightOptions = highlightOptions(
                  weight = 3,
                  color = "#666",
                  fillOpacity = 0.9,
                  bringToFront = TRUE
                )
              ) %>%
              addLegend(
                pal = pal,
                values = ~luas_kebakaran,
                opacity = 0.8,
                position = "bottomright",
                title = "Luas Karhutla (Ha)"
              ) %>%
              setView(lng = 118, lat = -2, zoom = 5)
            
          } else if(selected_type == "karbon") {
            data_filtered <- carbon_data %>%
              filter(tahun == selected_year, provinsi != "Indonesia") %>%
              rename(Provinsi = provinsi)
            data_tergabung <- geojson %>%
              left_join(data_filtered, by = "Provinsi")
            
            max_val <- quantile(data_tergabung$emisi_karbon, 1, na.rm = TRUE)
            pal <- colorNumeric(
              palette = colorRampPalette(c("#E8F5E8", "#C8E6C9", "#8E44AD", "#6A1B9A", "#4A148C"))(100),
              domain = c(0, max_val)
            )
            teks_popup <- paste(
              "<b>Provinsi:</b> ", data_tergabung$Provinsi,
              "<br/><b>Emisi Karbon:</b> ", format(data_tergabung$emisi_karbon, big.mark = ","), " ton CO2",
              "<br/><b>Tahun:</b> ", selected_year,
              sep = ""
            ) %>% lapply(htmltools::HTML)
            
            leaflet(data_tergabung) %>%
              addTiles() %>%
              addProviderTiles("Esri.WorldImagery", options = providerTileOptions(opacity = 0.3)) %>%
              addPolygons(
                fillColor = ~pal(emisi_karbon),
                fillOpacity = 0.8,
                color = "white",
                weight = 2,
                popup = teks_popup,
                highlightOptions = highlightOptions(
                  weight = 3,
                  color = "#666",
                  fillOpacity = 0.9,
                  bringToFront = TRUE
                )
              ) %>%
              addLegend(
                pal = pal,
                values = ~emisi_karbon,
                opacity = 0.8,
                position = "bottomright",
                title = "Emisi Karbon (ton CO2)"
              ) %>%
              setView(lng = 118, lat = -2, zoom = 5)
          }
        }
      })
      
      output$choroplethCombinedSummary <- renderUI({
        req(input$dropdownPetaJenis, input$dropdownPetaTahun)
        selected_type <- input$dropdownPetaJenis
        selected_year <- as.numeric(input$dropdownPetaTahun)
        
        if(selected_type == "kebakaran") {
          year_data <- fire_data %>%
            filter(tahun == selected_year, provinsi != "Indonesia")
          
          if(nrow(year_data) > 0) {
            max_province <- year_data %>%
              filter(luas_kebakaran == max(luas_kebakaran, na.rm = TRUE)) %>%
              slice(1)
            
            min_province <- year_data %>%
              filter(luas_kebakaran == min(luas_kebakaran, na.rm = TRUE)) %>%
              slice(1)
            
            total_kebakaran <- sum(year_data$luas_kebakaran, na.rm = TRUE)
            avg_kebakaran <- mean(year_data$luas_kebakaran, na.rm = TRUE)
            
            div(class = "summary-grid",
                div(class = "summary-card summary-card-highest",
                    div("🗺️", class = "summary-icon"),
                    div("Provinsi Tertinggi", class = "summary-label"),
                    div(paste(max_province$provinsi), class = "summary-value"),
                    div(paste(format(max_province$luas_kebakaran, big.mark = ","), "Ha"), class = "summary-value")
                ),
                div(class = "summary-card summary-card-lowest",
                    div("🌿", class = "summary-icon"),
                    div("Provinsi Terendah", class = "summary-label"),
                    div(paste(min_province$provinsi), class = "summary-value"),
                    div(paste(format(min_province$luas_kebakaran, big.mark = ","), "Ha"), class = "summary-value")
                ),
                div(class = "summary-card summary-card-total",
                    div("🔥", class = "summary-icon"),
                    div("Total Nasional", class = "summary-label"),
                    div(paste(format(total_kebakaran, big.mark = ","), "Ha"), class = "summary-value")
                ),
                div(class = "summary-card summary-card-average",
                    div("📊", class = "summary-icon"),
                    div("Rata-rata Provinsi", class = "summary-label"),
                    div(paste(format(round(avg_kebakaran), big.mark = ","), "Ha"), class = "summary-value")
                )
            )
          }
        } else if(selected_type == "karbon") {
          year_data <- carbon_data %>%
            filter(tahun == selected_year, provinsi != "Indonesia")
          
          if(nrow(year_data) > 0) {
            max_province <- year_data %>%
              filter(emisi_karbon == max(emisi_karbon, na.rm = TRUE)) %>%
              slice(1)
            
            min_province <- year_data %>%
              filter(emisi_karbon == min(emisi_karbon, na.rm = TRUE)) %>%
              slice(1)
            
            total_karbon <- sum(year_data$emisi_karbon, na.rm = TRUE)
            avg_karbon <- mean(year_data$emisi_karbon, na.rm = TRUE)
            
            div(class = "summary-grid",
                div(class = "summary-card summary-card-highest",
                    div("🗺️", class = "summary-icon"),
                    div("Provinsi Tertinggi", class = "summary-label"),
                    div(paste(max_province$provinsi), class = "summary-value"),
                    div(format_emission(max_province$emisi_karbon), class = "summary-value")
                ),
                div(class = "summary-card summary-card-lowest",
                    div("🌿", class = "summary-icon"),
                    div("Provinsi Terendah", class = "summary-label"),
                    div(paste(min_province$provinsi), class = "summary-value"),
                    div(format_emission(min_province$emisi_karbon), class = "summary-value")
                ),
                div(class = "summary-card summary-card-total",
                    div("☁️", class = "summary-icon"),
                    div("Total Nasional", class = "summary-label"),
                    div(format_emission(total_karbon), class = "summary-value")
                ),
                div(class = "summary-card summary-card-average",
                    div("📊", class = "summary-icon"),
                    div("Rata-rata Provinsi", class = "summary-label"),
                    div(format_emission(avg_karbon), class = "summary-value")
                )
            )
          }
        }
      })
      
      output$tableRanking <- renderDT({
        tahun_terpilih <- input$yearSelect
        
        data_filtered <- dataprovlengkap[dataprovlengkap$tahun == tahun_terpilih, ]
        
        data_filtered <- data_filtered %>%
          mutate(dummy = "") %>%
          select(dummy, everything())
        
        colnames(data_filtered) <- c("Rank", "Provinsi", "Tahun", 
                                     "Luas Karhutla (ha)", "Emisi Karbon (Mt)")
        
        datatable(
          data_filtered,
          options = list(
            pageLength = 10,
            ordering = TRUE,
            columnDefs = list(
              list(orderable = FALSE, targets = 0),
              list(orderable = FALSE, targets = 1),
              list(visible = FALSE, targets = 2)   
            ),
            rowCallback = JS(
              "function(row, data, displayIndex) {",
              "  $('td:eq(0)', row).html(displayIndex + 1);",
              "}"
            )
          ),
          rownames = FALSE
        )
      })
      
      
      
      
      dataset <- reactive({
        if (is.null(input$datafile)) {
          read.csv("data/DataProject.csv")
        } else {
          read.csv(input$datafile$datapath)
        }
      })
      
      output$xvar_ui <- renderUI({
        req(dataset())
        checkboxGroupInput("xvar", "Variabel Prediktor (X)", choices = names(dataset()),
                           selected = c(names(dataset())[3], names(dataset())[4]))
      })
      
      output$yvar_ui <- renderUI({
        req(dataset())
        selectInput("yvar", "Variabel Respons (Y)", choices = names(dataset()),
                    selected = names(dataset())[2])
      })
      
      model <- eventReactive(input$run_model, {
        req(input$xvar, input$yvar)
        formula <- as.formula(paste(input$yvar, "~", paste(input$xvar, collapse = "+")))
        lm(formula, data = dataset())
      })
      
      output$model_summary <- renderPrint({
        req(model())
        summary(model())
      })
      
      output$persamaan_regresi_latex <- renderUI({
        req(model())
        coef <- round(coef(model()), 3)
        xvars <- names(coef)[-1]
        yvar <- input$yvar
        intercept <- coef[1]
        terms <- paste0(coef[-1], xvars, collapse = " + ")
        equation <- paste0("$$", yvar, " = ", intercept, " + ", terms, "$$")
        withMathJax(HTML(equation))
      })
      
      
      output$regression_plot <- renderPlot({
        req(model(), input$yvar)
        
        df <- dataset()
        df$prediksi <- predict(model())
        r2 <- summary(model())$r.squared
        r2_label <- paste0("R^2 = ", round(r2, 3))
        
        ggplot(df, aes(x = prediksi, y = !!sym(input$yvar))) +
          geom_point(size = 3, color = "#2ca02c", alpha = 0.7) +
          geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray30") +
          annotate("text", x = min(df$prediksi, na.rm = TRUE), 
                   y = max(df[[input$yvar]], na.rm = TRUE), 
                   label = r2_label, hjust = 0, vjust = 1, size = 5, fontface = "bold") +
          labs(
            title = paste("Plot Nilai Prediksi vs Aktual:", input$yvar),
            x = "Nilai Prediksi",
            y = "Nilai Aktual"
          ) +
          theme_minimal(base_size = 14) +
          theme(
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            axis.title = element_text(face = "bold"),
            panel.grid.major = element_line(color = "gray90"),
            panel.grid.minor = element_blank()
          )
      })
      
      
      output$qq_plot <- renderPlot({
        req(model())
        ggplot(data = data.frame(residuals = residuals(model())), aes(sample = residuals)) +
          stat_qq(color = "#2c7fb8", size = 2) +
          stat_qq_line(color = "#d73027", linetype = "dashed") +
          labs(title = "Q-Q Plot Residual", x = "Teoritis", y = "Residual") +
          theme_minimal(base_size = 14)
      })
      
      
      output$hist_residual <- renderPlot({
        req(model())
        ggplot(data = data.frame(residuals = residuals(model())), aes(x = residuals)) +
          geom_histogram(fill = "#74c476", color = "white", bins = 30) +
          labs(title = "Histogram Residual", x = "Residual", y = "Frekuensi") +
          theme_minimal(base_size = 14)
      })
      
      
      output$shapiro_test <- renderPrint({
        req(model())
        shapiro.test(residuals(model()))
      })
      
      output$shapiro_keputusan <- renderText({
        req(model())
        pval <- shapiro.test(residuals(model()))$p.value
        if (pval > 0.05) {
          "✅ Residual berdistribusi normal (p > 0.05)"
        } else {
          "❌ Residual tidak normal (p < 0.05)"
        }
      })
      
      output$resid_fitted_plot <- renderPlot({
        req(model())
        df <- data.frame(fitted = fitted(model()), residuals = residuals(model()))
        ggplot(df, aes(x = fitted, y = residuals)) +
          geom_point(color = "#1f78b4", alpha = 0.7) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
          labs(title = "Plot Residual vs Fitted", x = "Fitted Values", y = "Residuals") +
          theme_minimal(base_size = 14)
      })
      
      
      output$bp_test <- renderPrint({
        req(model())
        bptest(model())
      })
      
      output$bp_keputusan <- renderText({
        req(model())
        pval <- bptest(model())$p.value
        if (pval > 0.05) {
          "✅ Tidak ditemukan heteroskedastisitas (p > 0.05)"
        } else {
          "❌ Ada indikasi heteroskedastisitas (p < 0.05)"
        }
      })
      
      output$residual_lag_plot <- renderPlot({
        req(model())
        res <- residuals(model())
        df <- data.frame(res_t1 = head(res, -1), res_t = tail(res, -1))
        ggplot(df, aes(x = res_t1, y = res_t)) +
          geom_point(color = "#e6550d", size = 2) +
          geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dotted") +
          labs(title = "Lag Plot Residual", x = "Residual t-1", y = "Residual t") +
          theme_minimal(base_size = 14)
      })
      
      
      output$dw_test <- renderPrint({
        req(model())
        dwtest(model())
      })
      
      output$dw_keputusan <- renderText({
        req(model())
        dw <- dwtest(model())
        if (dw$p.value > 0.05) {
          "✅ Tidak ditemukan autokorelasi (p > 0.05)"
        } else {
          "❌ Ada indikasi autokorelasi (p < 0.05)"
        }
      })
      
      output$show_vif <- reactive({
        req(model())
        length(input$xvar) > 1
      })
      outputOptions(output, "show_vif", suspendWhenHidden = FALSE)
      
      output$vif_result <- renderPrint({
        req(model())
        vif(model())
      })
      
      output$vif_keputusan <- renderText({
        req(model())
        vif_vals <- vif(model())
        if (all(vif_vals < 10)) {
          "✅ Tidak ada multikolinearitas (semua VIF < 10)"
        } else {
          "❌ Ditemukan multikolinearitas (ada VIF ≥ 10)"
        }
      })
      
      hasil_asumsi <- reactive({
        req(model())
        list(
          normalitas = shapiro.test(residuals(model()))$p.value > 0.05,
          homoskedastisitas = bptest(model())$p.value > 0.05,
          autokorelasi = dwtest(model())$p.value > 0.05,
          multikolinearitas = if (length(input$xvar) > 1) all(vif(model()) < 10) else NA
        )
      })
      
      output$ringkasan_asumsi <- renderTable({
        hasil <- hasil_asumsi()
        data.frame(
          Asumsi = c("Normalitas Residual", "Homoskedastisitas", "Autokorelasi", "Multikolinearitas"),
          Status = c(
            ifelse(hasil$normalitas, "✅ Lolos", "❌ Tidak Lolos"),
            ifelse(hasil$homoskedastisitas, "✅ Lolos", "❌ Tidak Lolos"),
            ifelse(hasil$autokorelasi, "✅ Lolos", "❌ Tidak Lolos"),
            ifelse(is.na(hasil$multikolinearitas), "— (Prediktor tunggal)",
                   ifelse(hasil$multikolinearitas, "✅ Lolos", "❌ Tidak Lolos"))
          )
        )
      }, striped = TRUE, bordered = TRUE, hover = TRUE)
      
      output$keputusan_akhir <- renderText({
        hasil <- hasil_asumsi()
        semua_lolos <- hasil$normalitas && hasil$homoskedastisitas && hasil$autokorelasi &&
          (is.na(hasil$multikolinearitas) || hasil$multikolinearitas)
        if (semua_lolos) {
          "\U0001F389 Semua asumsi terpenuhi. Model regresi dapat dikatakan valid untuk dianalisis lebih lanjut."
        } else {
          "\u26A0\uFE0F Terdapat asumsi yang tidak terpenuhi. Hasil regresi perlu ditinjau kembali atau dilakukan transformasi/perbaikan model."
        }
      })
      output$download_report <- downloadHandler(
        filename = function() {
          paste0("Laporan_Regresi_", Sys.Date(), ".html")
        },
        content = function(file) {
          tempReport <- file.path(tempdir(), "report_template.Rmd")
          file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
          
          params <- list(
            data = dataset(),
            model = model(),
            yvar = input$yvar,
            xvar = input$xvar,
            asumsi = hasil_asumsi()
          )
          
          rmarkdown::render(tempReport,
                            output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv()))
        }
      )
      
    output$download_emisi <- downloadHandler(
      filename = function() {
        paste0("data_emisi_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(emisi_data, file, row.names = FALSE)
      }
    )
    
    output$download_kebakaran <- downloadHandler(
      filename = function() {
        paste0("data_kebakaran_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(fire_data, file, row.names = FALSE)
      }
    )
    
    output$download_suhu <- downloadHandler(
      filename = function() {
        paste0("data_suhu_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(suhu_data, file, row.names = FALSE)
      }
    )
    output$download_geojson <- downloadHandler(
      filename = function() {
        paste0("peta_indonesia_", Sys.Date(), ".geojson")
      },
      content = function(file) {
        file.copy("data/indonesia.geojson", file)
      }
    )
    }
    
    
    
    shinyApp(ui = ui, server = server)