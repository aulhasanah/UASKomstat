library(shiny)
library(bs4Dash)
library(dplyr)
library(DT)
library(readr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(spdep)
library(spatialreg)
library(lmtest)
library(car)
library(nortest)
library(classInt)
library(RColorBrewer)
library(moments)
library(htmlwidgets)
library(webshot)
library(knitr)
library(rmarkdown)
library(gridExtra)
library(kableExtra)
library(htmltools)
# Load data with error handling

tryCatch({
  data_raw <- read_csv("sovi_data.csv")
  
  data_raw <- data_raw %>% select(DISTRICTCODE, FAMILYSIZE, LOWEDU, POVERTY, ILLITERATE, NOSEWER, NOELECTRIC, TAPWATER)
  data_raw$DISTRICTCODE <- as.character(data_raw$DISTRICTCODE)
  data_raw <- data_raw %>%
    mutate(
      wilayah = case_when(
        substr(DISTRICTCODE, 1, 2) %in% c("11","12","13","14","15","16","17","18","19","21") ~ "Sumatra",
        substr(DISTRICTCODE, 1, 2) %in% c("31","32","33","34","35","36") ~ "Jawa",
        substr(DISTRICTCODE, 1, 2) %in% c("61","62","63","64","65") ~ "Kalimantan",
        substr(DISTRICTCODE, 1, 2) %in% c("71","72","73","74","75","76") ~ "Sulawesi",
        substr(DISTRICTCODE, 1, 2) %in% c("51","52","53") ~ "Bali-Nusra",
        substr(DISTRICTCODE, 1, 2) %in% c("81","82","91","92", "94") ~ "Maluku-Papua",
        TRUE ~ "Lainnya"
      )
    )
  data_raw <- data_raw %>%
    relocate(wilayah, .before = 1)  
  cat("Data raw berhasil dibaca!\n")
}, error = function(e) {
  cat("Error membaca data:", e$message, "\n")
})
# Load geojson
tryCatch({
  indonesia_geojson <- st_read("indonesia511kab.geojson")
  cat("Geojson berhasil dibaca!\n")
}, error = function(e) {
  cat("Error membaca geojson:", e$message, "\n")
  indonesia_geojson <- NULL
})

tryCatch({
  peta_lengkap <- indonesia_geojson %>%
    left_join(data_raw, by = c("kodeprkab" = "DISTRICTCODE"))
  peta_lengkap <- st_as_sf(peta_lengkap)
  cat("Join dan konversi ke sf berhasil!\n")
}, error = function(e) {
  cat("Error joining geojson with csv:", e$message, "\n")
  peta_lengkap <- NULL
})


# Enhanced bivariate colors with better contrast
bivariate_colors <- c(
  "1-1" = "#f3f3f3", "1-2" = "#b8d4e3", "1-3" = "#5d8abd",
  "2-1" = "#c7e2c7", "2-2" = "#90b8b3", "2-3" = "#567994",
  "3-1" = "#7fb069", "3-2" = "#5a9178", "3-3" = "#2a5a5b"
)

# Enhanced function for bivariate classification
get_bivariate_class <- function(var1, var2) {
  # Hitung quantile secara aman
  q1 <- quantile(var1, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
  q2 <- quantile(var2, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
  
  # Tangani kasus quantile sama (nilai konstan)
  if (length(unique(q1)) < 4) {
    var1_class <- factor(2, levels = 1:3)  # fallback ke tengah
  } else {
    var1_class <- cut(var1, breaks = q1, include.lowest = TRUE, labels = c(1, 2, 3))
  }
  
  if (length(unique(q2)) < 4) {
    var2_class <- factor(2, levels = 1:3)
  } else {
    var2_class <- cut(var2, breaks = q2, include.lowest = TRUE, labels = c(1, 2, 3))
  }
  
  result <- paste0(var1_class, "-", var2_class)
  result[is.na(result)] <- "2-2"  # fallback aman
  result
}

create_download_button <- function(outputId, label, icon = "download", class = "btn-success") {
  downloadButton(outputId, label, icon = icon(icon), class = paste("btn", class), 
                 style = "margin: 5px; font-weight: bold;")
}

# Function to save ggplot
save_ggplot <- function(plot, filename, width = 12, height = 8) {
  ggsave(filename, plot, width = width, height = height, dpi = 300, bg = "white")
}

# Function to save plotly as static image
save_plotly <- function(plotly_obj, filename, width = 1200, height = 800) {
  tryCatch({
    htmlwidgets::saveWidget(plotly_obj, "temp_plot.html", selfcontained = TRUE)
    webshot::webshot("temp_plot.html", filename, width = width, height = height)
    file.remove("temp_plot.html")
  }, error = function(e) {
    message("Error saving plotly: ", e$message)
  })
}

# Function to create PDF report
create_pdf_report <- function(content_list, filename, title) {
  tryCatch({
    # Create temporary Rmd file
    rmd_content <- paste0(
      "---\n",
      "title: '", title, "'\n",
      "date: '", Sys.Date(), "'\n",
      "output:\n",
      "  pdf_document:\n",
      "    latex_engine: xelatex\n",
      "    keep_tex: false\n",
      "header-includes:\n",
      "  - \\usepackage{float}\n",
      "  - \\usepackage{booktabs}\n",
      "  - \\usepackage{longtable}\n",
      "---\n\n",
      paste(content_list, collapse = "\n\n")
    )
    
    writeLines(rmd_content, "temp_report.Rmd")
    rmarkdown::render("temp_report.Rmd", output_file = filename, quiet = TRUE)
    file.remove("temp_report.Rmd")
    
    return(filename)
  }, error = function(e) {
    message("Error creating PDF: ", e$message)
    return(NULL)
  })
}
ui <- dashboardPage(
  title = "",
  
  header = dashboardHeader(
    title = NULL,
    skin = "light",
    status = "primary",
    controlbarIcon = NULL,
    fixed = TRUE
  ),
  
  sidebar = dashboardSidebar(
    skin = "light",
    status = "primary",
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Manajemen Data", icon = icon("sliders-h"),
               menuSubItem("Kategorisasi", tabName = "kategorisasi"),
               menuSubItem("Transformasi", tabName = "transformasi")
      ),
      menuItem("Eksplorasi Data", icon = icon("chart-bar"),
               menuSubItem("Variabel Numerik", tabName = "desc_numeric"),
               menuSubItem("Variabel Kategorik", tabName = "desc_categoric"),
               menuSubItem("Peta Interaktif", tabName = "desc_map")
      ),
      menuItem("Uji Asumsi", icon = icon("microscope"),
               menuSubItem("Uji Normalitas", tabName = "uji_normalitas"),
               menuSubItem("Uji Homogenitas", tabName = "uji_homogenitas")
      ),
      
      menuItem("Statistik Inferensial", icon = icon("microscope"),
               menuSubItem("Uji Beda Rata-rata", tabName = "uji_mean"),
               menuSubItem("Uji Varians", tabName = "uji_var"),
               menuSubItem("Uji Proporsi", tabName = "uji_prop")
      ),
      menuItem("Regresi", icon = icon("chart-line"),
               menuSubItem("OLS Regression", tabName = "ols_reg"),
               menuSubItem("Spatial Regression", tabName = "spatial_reg")
      )
    )
  ),
  
  body = dashboardBody(
    tags$head(
      tags$style(HTML("
        /* ENHANCED COLOR SCHEME - Purple, Yellow, Blue */
        :root {
          --primary-purple: #3b0f70;
          --secondary-purple: #5e4fa2;
          --light-purple: #9970ab;
          --accent-yellow: #ffff99;
          --accent-blue: #4a90e2;
          --light-blue: #87ceeb;
          --success-green: #28a745;
          --warning-orange: #fd7e14;
          --danger-red: #dc3545;
          --light-bg: #ffffff;
          --light-text: #333333;
          --dark-bg: #1a1a2e;
          --dark-text: #ffffff;
        }
        .nav-item.dropdown.help-menu {
          display: none !important;
        }
      .main-header .nav-item.dropdown,
        .main-header .nav-item.controlbar-toggle,
        .main-header .nav-item.dropdown.help-menu {
          display: none !important;
        }
        

        /* LIGHT MODE STYLES */
        .main-header {
          background: linear-gradient(135deg, var(--primary-purple) 0%, var(--secondary-purple) 100%) !important;
          box-shadow: 0 2px 4px rgba(59, 15, 112, 0.3) !important;
        }
        .main-sidebar {
          background: linear-gradient(180deg, #ede4f5 0%, #f8f9ff 100%) !important;
          border-right: 2px solid var(--light-purple) !important;
        }
        .main-sidebar .nav-link {
          color: var(--secondary-purple) !important;
          font-weight: 500 !important;
          transition: all 0.3s ease !important;
        }
        .main-sidebar .nav-link:hover {
          background-color: #d9cef7 !important;
          color: var(--primary-purple) !important;
          transform: translateX(5px) !important;
        }
        .main-sidebar .nav-link.active {
          background: linear-gradient(135deg, #d9cef7 0%, var(--accent-yellow) 100%) !important;
          color: var(--primary-purple) !important;
          font-weight: bold !important;
          border-left: 4px solid var(--primary-purple) !important;
        }
        .content-wrapper {
          background: linear-gradient(135deg, var(--light-bg) 0%, #f8f9ff 100%) !important;
          color: var(--light-text) !important;
        }
        
        /* Enhanced Card Headers */
        .card-primary .card-header {
          background: linear-gradient(135deg, var(--accent-yellow) 0%, #fff3cd 100%) !important;
          color: var(--primary-purple) !important;
          font-weight: bold !important;
          border-bottom: 2px solid var(--secondary-purple) !important;
        }
        .card-info .card-header {
          background: linear-gradient(135deg, var(--accent-blue) 0%, var(--light-blue) 100%) !important;
          color: white !important;
          font-weight: bold !important;
        }
        .card-warning .card-header {
          background: linear-gradient(135deg, var(--warning-orange) 0%, #ffc107 100%) !important;
          color: white !important;
          font-weight: bold !important;
        }
        .card-success .card-header {
          background: linear-gradient(135deg, var(--success-green) 0%, #20c997 100%) !important;
          color: white !important;
          font-weight: bold !important;
        }
                /* DOWNLOAD BUTTONS STYLING */
        .download-section {
          background: linear-gradient(135deg, #f8f9ff 0%, #e7d4e8 100%);
          border: 2px solid var(--secondary-purple);
          border-radius: 15px;
          padding: 20px;
          margin: 20px 0;
          box-shadow: 0 6px 12px rgba(59, 15, 112, 0.15);
        }
        .download-section h5 {
          color: var(--primary-purple);
          font-weight: bold;
          margin-bottom: 15px;
          text-align: center;
        }
        .download-buttons {
          display: flex;
          flex-wrap: wrap;
          justify-content: center;
          gap: 10px;
        }
        /* Enhanced Buttons */
        .btn-primary {
          background: linear-gradient(135deg, var(--primary-purple) 0%, var(--secondary-purple) 100%) !important;
          border: none !important;
          color: white !important;
          box-shadow: 0 2px 4px rgba(59, 15, 112, 0.3) !important;
          transition: all 0.3s ease !important;
        }
        .btn-primary:hover {
          transform: translateY(-1px) !important;
          box-shadow: 0 4px 8px rgba(59, 15, 112, 0.4) !important;
          color: white !important;
        }
        .btn-info {
          background: linear-gradient(135deg, var(--accent-blue) 0%, #3a7bd5 100%) !important;
          border: none !important;
          color: white !important;
          box-shadow: 0 2px 4px rgba(74, 144, 226, 0.3) !important;
        }
        .btn-info:hover {
          color: white !important;
        }
        .btn-warning {
          background: linear-gradient(135deg, var(--warning-orange) 0%, #e67e22 100%) !important;
          border: none !important;
          color: white !important;
        }
        .btn-warning:hover {
          color: white !important;
        }
        
        /* Dramatic Title Enhancement */
        .dramatic-title {
          text-align: center;
          background: linear-gradient(45deg, var(--primary-purple), var(--secondary-purple), var(--light-purple));
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          background-clip: text;
          font-size: 2.5rem;
          font-weight: 800;
          margin-bottom: 30px;
        }
        
        /* Enhanced Dashboard Info Box */
        .dashboard-info {
          background: linear-gradient(135deg, #f8f9ff 0%, #e7d4e8 20%, var(--accent-yellow) 100%);
          border: 3px solid var(--secondary-purple);
          border-radius: 20px;
          padding: 25px;
          margin: 25px 0;
          box-shadow: 0 8px 16px rgba(59, 15, 112, 0.2);
          position: relative;
          overflow: hidden;
        }
        .dashboard-info::before {
          content: '';
          position: absolute;
          top: -50%;
          left: -50%;
          width: 200%;
          height: 200%;
          background: radial-gradient(circle, rgba(255,255,153,0.1) 0%, transparent 70%);
          animation: rotate 20s linear infinite;
        }
        .dashboard-info h4 {
          color: var(--primary-purple);
          font-weight: bold;
          margin-bottom: 20px;
          font-size: 1.5rem;
          position: relative;
          z-index: 1;
        }
        .dashboard-info p, .dashboard-info ul {
          color: var(--secondary-purple);
          line-height: 1.8;
          margin-bottom: 15px;
          position: relative;
          z-index: 1;
        }
        
        @keyframes rotate {
          from { transform: rotate(0deg); }
          to { transform: rotate(360deg); }
        }
        
        /* Enhanced Stats Cards */
        .stats-card {
          background: linear-gradient(135deg, #f8f9ff 0%, #e7d4e8 50%, var(--accent-yellow) 100%);
          border: 2px solid var(--light-purple);
          border-radius: 15px;
          padding: 25px;
          margin: 15px 0;
          box-shadow: 0 6px 12px rgba(59, 15, 112, 0.15);
          transition: all 0.3s ease;
        }
        .stats-card:hover {
          transform: translateY(-5px);
          box-shadow: 0 10px 20px rgba(59, 15, 112, 0.25);
        }
        .stats-header {
          color: var(--primary-purple);
          font-size: 20px;
          font-weight: bold;
          margin-bottom: 20px;
          border-bottom: 3px solid var(--secondary-purple);
          padding-bottom: 10px;
        }
        .stats-content {
          color: var(--secondary-purple);
          font-size: 16px;
        }
        
        /* Enhanced Hypothesis Testing Cards */
        .hypothesis-card {
          background: linear-gradient(135deg, #f8f9ff 0%, #e7d4e8 30%, #fff 100%);
          border: 2px solid var(--light-purple);
          border-radius: 12px;
          padding: 25px;
          margin: 20px 0;
          box-shadow: 0 6px 12px rgba(59, 15, 112, 0.15);
          transition: all 0.3s ease;
        }
        .hypothesis-card:hover {
          transform: translateY(-3px);
          box-shadow: 0 8px 16px rgba(59, 15, 112, 0.2);
        }
        .hypothesis-header {
          color: var(--primary-purple);
          font-size: 18px;
          font-weight: bold;
          margin-bottom: 15px;
          border-bottom: 3px solid var(--secondary-purple);
          padding-bottom: 8px;
        }
        .hypothesis-content {
          color: var(--secondary-purple);
          line-height: 1.8;
          font-size: 15px;
        }
        
        /* Enhanced Decision Boxes */
        .decision-accept {
          background: linear-gradient(135deg, #d4edda 0%, #c3e6cb 100%);
          color: #155724;
          padding: 15px;
          border-radius: 8px;
          border-left: 5px solid var(--success-green);
          font-weight: bold;
          box-shadow: 0 3px 6px rgba(40, 167, 69, 0.2);
        }
        .decision-reject {
          background: linear-gradient(135deg, #f8d7da 0%, #f5c6cb 100%);
          color: #721c24;
          padding: 15px;
          border-radius: 8px;
          border-left: 5px solid var(--danger-red);
          font-weight: bold;
          box-shadow: 0 3px 6px rgba(220, 53, 69, 0.2);
        }
        
        /* Enhanced Value Boxes */
        .small-box {
          border-radius: 15px !important;
          box-shadow: 0 6px 12px rgba(0,0,0,0.1) !important;
          transition: all 0.3s ease !important;
        }
        .small-box:hover {
          transform: translateY(-5px) !important;
          box-shadow: 0 10px 20px rgba(0,0,0,0.15) !important;
        }
        
        /* Enhanced Statistics Grid */
        .enhanced-stats-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
          gap: 20px;
          margin: 20px 0;
        }
        .enhanced-stat-item {
          background: linear-gradient(135deg, #f8f9ff 0%, #e7d4e8 50%, var(--accent-yellow) 100%);
          border: 2px solid var(--light-purple);
          border-radius: 12px;
          padding: 20px;
          text-align: center;
          transition: all 0.3s ease;
          box-shadow: 0 4px 8px rgba(59, 15, 112, 0.1);
        }
        .enhanced-stat-item:hover {
          transform: translateY(-5px);
          box-shadow: 0 8px 16px rgba(59, 15, 112, 0.2);
        }
        .enhanced-stat-label {
          color: var(--secondary-purple);
          font-weight: 600;
          font-size: 14px;
          margin-bottom: 10px;
        }
        .enhanced-stat-value {
          color: var(--primary-purple);
          font-weight: bold;
          font-size: 24px;
        }
        
        /* Enhanced Transposed Table */
        .enhanced-transposed-table {
          width: 100%;
          border-collapse: collapse;
          margin: 20px 0;
          border-radius: 10px;
          overflow: hidden;
          box-shadow: 0 4px 8px rgba(59, 15, 112, 0.1);
        }
        .enhanced-transposed-table th,
        .enhanced-transposed-table td {
          border: 1px solid var(--light-purple);
          padding: 12px;
          text-align: center;
        }
        .enhanced-transposed-table th {
          background: linear-gradient(135deg, var(--secondary-purple) 0%, var(--primary-purple) 100%);
          color: var(--accent-yellow);
          font-weight: bold;
          font-size: 16px;
        }
        .enhanced-transposed-table td {
          background: rgba(248, 249, 255, 0.8);
          color: var(--primary-purple);
          font-weight: 500;
        }
        .enhanced-transposed-table tr:hover td {
          background: rgba(231, 212, 232, 0.8);
        }
        
        /* Enhanced Legend Box */
        .enhanced-legend {
          background: linear-gradient(135deg, #f8f9ff 0%, #e7d4e8 100%);
          border: 2px solid var(--secondary-purple);
          border-radius: 15px;
          padding: 20px;
          box-shadow: 0 6px 12px rgba(59, 15, 112, 0.15);
        }
        
        /* Loading Animation */
        .loading-spinner {
          display: inline-block;
          width: 20px;
          height: 20px;
          border: 3px solid rgba(94, 79, 162, 0.3);
          border-radius: 50%;
          border-top-color: var(--secondary-purple);
          animation: spin 1s ease-in-out infinite;
        }
        
        @keyframes spin {
          to { transform: rotate(360deg); }
        }
        
        /* Notification Enhancements */
        .shiny-notification {
          border-radius: 10px !important;
          box-shadow: 0 6px 12px rgba(0,0,0,0.15) !important;
        }
        .shiny-notification-content {
          font-weight: 500 !important;
        }
        body.dark-mode .download-section {
          background: linear-gradient(135deg, #2d1b4e 0%, #4a3268 100%);
          border: 2px solid var(--accent-blue);
        }
        
        body.dark-mode .download-section h5 {
          color: var(--accent-yellow);
        }
        /* DARK MODE STYLES */
        body.dark-mode {
          background-color: var(--dark-bg) !important;
          color: var(--dark-text) !important;
        }
        
        body.dark-mode .main-header {
          background: linear-gradient(135deg, #2d1b4e 0%, var(--primary-purple) 100%) !important;
          border-bottom: 2px solid var(--accent-yellow) !important;
        }
        
        body.dark-mode .main-sidebar {
          background: linear-gradient(180deg, #2e1e4f 0%, #1a1a2e 100%) !important;
          border-right: 2px solid var(--accent-blue) !important;
        }
        
        body.dark-mode .main-sidebar .nav-link {
          color: #d9cef7 !important;
        }
        
        body.dark-mode .main-sidebar .nav-link:hover {
          background-color: var(--primary-purple) !important;
          color: var(--accent-yellow) !important;
        }
        
        body.dark-mode .main-sidebar .nav-link.active {
          background: linear-gradient(135deg, var(--primary-purple) 0%, var(--secondary-purple) 100%) !important;
          color: var(--accent-yellow) !important;
          border-left: 4px solid var(--accent-yellow) !important;
        }
        
        body.dark-mode .content-wrapper {
          background: linear-gradient(135deg, #1c1c2e 0%, #2d1b4e 100%) !important;
          color: var(--dark-text) !important;
        }
        
        /* Dark Mode Cards */
        body.dark-mode .card {
          background-color: rgba(45, 27, 78, 0.9) !important;
          border: 1px solid var(--accent-blue) !important;
          color: var(--dark-text) !important;
        }
        
        body.dark-mode .card-primary .card-header {
          background: linear-gradient(135deg, var(--secondary-purple) 0%, var(--primary-purple) 100%) !important;
          color: var(--accent-yellow) !important;
          border-bottom: 2px solid var(--accent-yellow) !important;
        }
        
        body.dark-mode .card-info .card-header {
          background: linear-gradient(135deg, var(--accent-blue) 0%, #2a5a8a 100%) !important;
          color: var(--dark-text) !important;
        }
        
        body.dark-mode .card-warning .card-header {
          background: linear-gradient(135deg, #b8860b 0%, var(--warning-orange) 100%) !important;
          color: var(--dark-text) !important;
        }
        
        body.dark-mode .card-success .card-header {
          background: linear-gradient(135deg, #1e6b3a 0%, var(--success-green) 100%) !important;
          color: var(--dark-text) !important;
        }
        
        body.dark-mode .dashboard-info {
          background: linear-gradient(135deg, #2d1b4e 0%, #4a3268 50%, #5e4fa2 100%);
          border: 3px solid var(--accent-yellow);
          color: #e7d4e8;
        }
        
        body.dark-mode .dashboard-info h4 {
          color: var(--accent-yellow);
        }
        
        body.dark-mode .dashboard-info p, 
        body.dark-mode .dashboard-info ul {
          color: #b19cd9;
        }
        
        body.dark-mode .stats-card {
          background: linear-gradient(135deg, #2d1b4e 0%, #4a3268 50%, #5e4fa2 100%);
          border: 2px solid var(--accent-yellow);
        }
        
        body.dark-mode .stats-header {
          color: var(--accent-yellow);
          border-bottom: 3px solid var(--accent-yellow);
        }
        
        body.dark-mode .stats-content {
          color: #b19cd9;
        }
        
        body.dark-mode .hypothesis-card {
          background: linear-gradient(135deg, #2d1b4e 0%, #4a3268 50%, #3b0f70 100%);
          border: 2px solid var(--accent-blue);
        }
        
        body.dark-mode .hypothesis-header {
          color: var(--accent-yellow);
          border-bottom: 3px solid var(--accent-blue);
        }
        
        body.dark-mode .hypothesis-content {
          color: #b19cd9;
        }
        
        body.dark-mode .enhanced-stat-item {
          background: linear-gradient(135deg, #2d1b4e 0%, #4a3268 50%, var(--secondary-purple) 100%);
          border: 2px solid var(--accent-blue);
        }
        
        body.dark-mode .enhanced-stat-label {
          color: var(--accent-yellow);
        }
        
        body.dark-mode .enhanced-stat-value {
          color: var(--dark-text);
        }
        
        body.dark-mode .enhanced-transposed-table th {
          background: linear-gradient(135deg, var(--accent-blue) 0%, var(--secondary-purple) 100%);
          color: var(--accent-yellow);
        }
        
        body.dark-mode .enhanced-transposed-table td {
          background: rgba(45, 27, 78, 0.8);
          color: #e7d4e8;
          border: 1px solid #6b4c87;
        }
        
        body.dark-mode .enhanced-transposed-table tr:hover td {
          background: rgba(74, 50, 104, 0.9);
          color: var(--accent-yellow);
        }
        
        body.dark-mode .enhanced-legend {
          background: linear-gradient(135deg, #2d1b4e 0%, #4a3268 100%);
          border: 2px solid var(--accent-blue);
        }
        
        /* Enhanced DT Tables for Dark Mode */
        body.dark-mode .dataTables_wrapper {
          color: var(--dark-text) !important;
        }
        
        body.dark-mode .dataTables_wrapper table {
          color: var(--dark-text) !important;
          background: linear-gradient(135deg, #2d1b4e 0%, #4a3268 100%) !important;
          border-radius: 10px !important;
          overflow: hidden !important;
        }
        
        body.dark-mode .dataTables_wrapper table thead th {
          background: linear-gradient(135deg, var(--primary-purple) 0%, var(--secondary-purple) 100%) !important;
          color: var(--accent-yellow) !important;
          border-bottom: 2px solid var(--accent-yellow) !important;
          font-weight: bold !important;
        }
        
        body.dark-mode .dataTables_wrapper table tbody td {
          background: rgba(45, 27, 78, 0.8) !important;
          color: #e7d4e8 !important;
          border-bottom: 1px solid #6b4c87 !important;
        }
        
        body.dark-mode .dataTables_wrapper table tbody tr:hover td {
          background: rgba(74, 50, 104, 0.9) !important;
          color: var(--accent-yellow) !important;
        }
        
        /* Dark mode for decision boxes */
        body.dark-mode .decision-accept {
          background: linear-gradient(135deg, #1e4d2b 0%, #2d5a3d 100%);
          color: #90ee90;
          border-left: 5px solid #28a745;
        }
        
        body.dark-mode .decision-reject {
          background: linear-gradient(135deg, #4d1e1e 0%, #5a2d2d 100%);
          color: #ffb3b3;
          border-left: 5px solid #dc3545;
        }
        
        /* Dark mode for buttons */
        body.dark-mode .btn-primary {
          background: linear-gradient(135deg, var(--secondary-purple) 0%, var(--primary-purple) 100%) !important;
          border: 1px solid var(--accent-yellow) !important;
        }
        
        body.dark-mode .btn-info {
          background: linear-gradient(135deg, var(--accent-blue) 0%, #2a5a8a 100%) !important;
          border: 1px solid var(--accent-blue) !important;
        }
        
        body.dark-mode .btn-warning {
          background: linear-gradient(135deg, #b8860b 0%, var(--warning-orange) 100%) !important;
          border: 1px solid var(--warning-orange) !important;
        }
        
        /* Dark mode for inputs */
        body.dark-mode .form-control {
          background-color: rgba(45, 27, 78, 0.8) !important;
          border: 1px solid var(--accent-blue) !important;
          color: var(--dark-text) !important;
        }
        
        body.dark-mode .form-control:focus {
          background-color: rgba(45, 27, 78, 0.9) !important;
          border-color: var(--accent-yellow) !important;
          box-shadow: 0 0 0 0.2rem rgba(255, 255, 153, 0.25) !important;
        }
        
        body.dark-mode .selectize-input {
          background-color: rgba(45, 27, 78, 0.8) !important;
          border: 1px solid var(--accent-blue) !important;
          color: var(--dark-text) !important;
        }
        
        body.dark-mode .selectize-dropdown {
          background-color: rgba(45, 27, 78, 0.95) !important;
          border: 1px solid var(--accent-blue) !important;
          color: var(--dark-text) !important;
        }
        
        body.dark-mode .selectize-dropdown-content .option {
          color: var(--dark-text) !important;
        }
        
        body.dark-mode .selectize-dropdown-content .option:hover {
          background-color: var(--secondary-purple) !important;
          color: var(--accent-yellow) !important;
        }
      "))
    ),
    
    tabItems(
      # ENHANCED HOME TAB
      tabItem(
        tabName = "home",
        fluidRow(
          column(12,
                 div(class = "dramatic-title", "🌟 Grid Gap: Ketimpangan Akses Listrik di Indonesia 🌟"),
                 div(class = "dashboard-info",
                     h4("🎯 Tentang Dashboard Grid Gap"),
                     p("Grid Gap adalah dashboard interaktif untuk memetakan dan menganalisis ketimpangan akses listrik di 511 kabupaten/kota Indonesia."),
                     p("Dashboard ini menyediakan fitur analisis sosial-ekonomi yang interaktif dan lengkap melalui tab berikut:"),
                     tags$ul(
                       tags$li("🔧 Manajemen Data — untuk melihat, mengkategorikan, serta melakukan transformasi variabel"),
                       tags$li("📊 Eksplorasi Data — menampilkan statistik deskriptif dan visualisasi spasial berupa peta choropleth"),
                       tags$li("🧪 Uji Asumsi — menyediakan alat untuk memeriksa normalitas dan homogenitas data"),
                       tags$li("📉 Statistik Inferensial — mendukung berbagai uji hubungan variabel"),
                       tags$li("📈 Regresi — memuat analisis regresi OLS dan regresi spasial lengkap dengan uji diagnostik")
                     ),
                     p("Dikembangkan dengan oleh Aulia Ul Hasanah, mahasiswi Politeknik Statistika STIS.")
                 )
          )
        ),
        
        
        fluidRow(
          valueBoxOutput("total_obs"),
          valueBoxOutput("avg_poverty"),
          valueBoxOutput("avg_noelectric")
        ),
        
        fluidRow(
          box(
            title = "📋 Metadata Variabel", width = 12, status = "primary", solidHeader = TRUE,
            DTOutput("metadata_table")
          )
        ),
        # DOWNLOAD SECTION FOR HOME
        fluidRow(
          column(12,
                 div(class = "download-section",
                     h5("📥 Download Laporan Home"),
                     div(class = "download-buttons",
                         create_download_button("download_home_table", "📊 Download Tabel", "table"),
                         create_download_button("download_home_pdf", "📄 Download PDF Lengkap", "file-pdf", "warning")
                     )
                 )
          )
        )
      ),
      
      # ENHANCED KATEGORISASI TAB
      tabItem(
        tabName = "kategorisasi",
        fluidRow(
          column(12, h2("🏷️ Kategorisasi Variabel Lanjutan", style = "color: #3b0f70; font-weight: bold; text-align: center; margin-bottom: 30px;"))
        ),
        
        fluidRow(
          column(
            width = 4,
            box(
              title = "⚙️ Pengaturan Kategorisasi", width = 12, status = "primary", solidHeader = TRUE,
              selectInput("cat_var", "Pilih Variabel Numerik:",
                          choices = names(select_if(data_raw, is.numeric))),
              selectInput("cat_method", "Metode Kategorisasi:",
                          choices = c("Natural Breaks (Jenks)" = "natural",
                                      "Quantile (Equal Count)" = "quantile",
                                      "Equal Interval" = "equal",
                                      "Standard Deviation" = "sd")),
              numericInput("cat_n_classes", "Jumlah Kategori:", value = 4, min = 2, max = 10),
              textAreaInput("cat_labels", "Nama Kategori (pisahkan dengan koma):",
                            value = "Rendah, Sedang, Tinggi, Sangat Tinggi", rows = 3),
              actionButton("create_categories", "🎯 Buat Kategori", class = "btn-primary", style = "width: 100%;")
            )
          ),
          column(
            width = 8,
            conditionalPanel(
              condition = "output.categories_created",
              box(
                title = "📊 Distribusi Variabel Asli", width = 12, status = "info", solidHeader = TRUE,
                plotlyOutput("cat_histogram", height = "400px"),
                div(style = "margin-top: 15px; padding: 15px; background: rgba(94, 79, 162, 0.1); border-radius: 10px; border-left: 4px solid #5e4fa2;",
                    h5("💡 Interpretasi Distribusi:", style = "color: #3b0f70; margin-bottom: 10px;"),
                    htmlOutput("cat_distribution_interpretation")
                )
              )
            )
          )
        ),
        conditionalPanel(
          condition = "output.categories_created",
          fluidRow(
            column(
              width = 6,
              box(
                title = "📈 Hasil Kategorisasi", width = 12, status = "warning", solidHeader = TRUE,
                plotlyOutput("cat_barchart", height = "400px")
              )
            ),
            column(
              width = 6,
              box(
                title = "📋 Statistik Kategorisasi", width = 12, status = "success", solidHeader = TRUE,
                htmlOutput("cat_statistics")
              )
            )
          )
        ),
        conditionalPanel(
          condition = "output.categories_created",
          fluidRow(
            column(12,
                   div(class = "download-section",
                       h5("📥 Download Hasil Kategorisasi"),
                       div(class = "download-buttons",
                           create_download_button("download_cat_histogram", "📊 Download Histogram", "chart-bar"),
                           create_download_button("download_cat_barchart", "📈 Download Bar Chart", "chart-column"),
                           create_download_button("download_cat_table", "📋 Download Tabel", "table"),
                           create_download_button("download_cat_pdf", "📄 Download PDF Lengkap", "file-pdf", "warning")
                       )
                   )
            )
          )
        )
      ),
      
      # ENHANCED TRANSFORMASI TAB
      tabItem(
        tabName = "transformasi",
        fluidRow(
          column(12, h2("🔄 Transformasi Variabel Lanjutan", style = "color: #3b0f70; font-weight: bold; text-align: center; margin-bottom: 30px;"))
        ),
        
        fluidRow(
          column(
            width = 4,
            box(
              title = "⚙️ Pengaturan Transformasi", width = 12, status = "primary", solidHeader = TRUE,
              selectInput("trans_var", "Pilih Variabel Numerik:", choices = NULL),
              selectInput("trans_method", "Metode Transformasi:",
                          choices = c("Log Natural (ln)" = "log",
                                      "Square Root (√)" = "sqrt",
                                      "Standardisasi Z-Score" = "scale",
                                      "Min-Max Normalization" = "minmax",
                                      "Box-Cox Transform" = "boxcox"),
                          selected = NULL),
              conditionalPanel(
                condition = "input.trans_method == 'boxcox'",
                numericInput("lambda_boxcox", "Lambda (λ) untuk Box-Cox:", value = 0, step = 0.1)
              ),
              actionButton("apply_transformation", "🚀 Terapkan Transformasi", class = "btn-primary", style = "width: 100%;")
            )
          ),
          column(
            width = 4,
            conditionalPanel(
              condition = "output.transformation_applied",
              box(
                title = "✨ Sebelum Transformasi", width = 12, status = "info", solidHeader = TRUE,
                plotlyOutput("trans_boxplot_before", height = "400px")
              )
            )
          ),
          column(
            width = 4,
            conditionalPanel(
              condition = "output.transformation_applied",
              box(
                title = "✨ Setelah Transformasi", width = 12, status = "warning", solidHeader = TRUE,
                plotlyOutput("trans_boxplot_after", height = "400px")
              )
            )
          )
        ),
        conditionalPanel(
          condition = "output.transformation_applied",
          fluidRow(
            column(
              width = 12,
              box(
                title = "📈 Perbandingan Statistik Lengkap", width = 12, status = "success", solidHeader = TRUE,
                htmlOutput("trans_statistics")
              )
            )
          )
        ),
        conditionalPanel(
          condition = "output.transformation_applied",
          fluidRow(
            column(12,
                   div(class = "download-section",
                       h5("📥 Download Hasil Transformasi"),
                       div(class = "download-buttons",
                           create_download_button("download_trans_before", "📦 Download Plot Sebelum", "chart-simple"),
                           create_download_button("download_trans_after", "✨ Download Plot Setelah", "chart-simple"),
                           create_download_button("download_trans_table", "📊 Download Tabel Statistik", "table"),
                           create_download_button("download_trans_pdf", "📄 Download PDF Lengkap", "file-pdf", "warning")
                       )
                   )
            )
          )
        )
      ),
      
      # ENHANCED DESCRIPTIVE NUMERIC TAB
      tabItem(
        tabName = "desc_numeric",
        fluidRow(
          column(12, h2("📊 Analisis Deskriptif - Variabel Numerik", style = "color: #3b0f70; font-weight: bold; text-align: center; margin-bottom: 30px;"))
        ),
        
        fluidRow(
          column(
            width = 3,
            box(
              title = "🎯 Pilih Variabel", width = 12, status = "primary", solidHeader = TRUE,
              selectInput("desc_var", "Variabel:", choices = names(select_if(data_raw, is.numeric))),
              br(),
              actionButton("refresh_desc", "🔄 Refresh Analisis", class = "btn-info", style = "width: 100%;")
            )
          ),
          column(
            width = 9,
            conditionalPanel(
              condition = "output.desc_stats_available",
              box(
                title = "📈 Statistik Deskriptif Lengkap", width = 12, status = "info", solidHeader = TRUE,
                htmlOutput("desc_stats_display")
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.desc_stats_available",
          fluidRow(
            box(
              title = "📊 Histogram & Kurva Densitas", width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("desc_histogram", height = "400px"),
              div(style = "margin-top: 15px; padding: 15px; background: rgba(94, 79, 162, 0.1); border-radius: 10px; border-left: 4px solid #5e4fa2;",
                  h5("💡 Interpretasi Histogram:", style = "color: #3b0f70; margin-bottom: 10px;"),
                  htmlOutput("histogram_interpretation")
              )
            ),
            box(
              title = "📦 Box Plot dengan Outliers", width = 6, status = "warning", solidHeader = TRUE,
              plotlyOutput("desc_boxplot", height = "400px"),
              div(style = "margin-top: 15px; padding: 15px; background: rgba(253, 126, 20, 0.1); border-radius: 10px; border-left: 4px solid #fd7e14;",
                  h5("💡 Interpretasi Box Plot:", style = "color: #fd7e14; margin-bottom: 10px;"),
                  htmlOutput("boxplot_interpretation")
              )
            )
          )
        ),
        conditionalPanel(
          condition = "output.desc_stats_available",
          fluidRow(
            column(12,
                   div(class = "download-section",
                       h5("📥 Download Analisis Deskriptif Numerik"),
                       div(class = "download-buttons",
                           create_download_button("download_desc_histogram", "📊 Download Histogram", "chart-bar"),
                           create_download_button("download_desc_boxplot", "📦 Download Box Plot", "chart-simple"),
                           create_download_button("download_desc_stats", "📈 Download Statistik", "table"),
                           create_download_button("download_desc_numeric_pdf", "📄 Download PDF Lengkap", "file-pdf", "warning")
                       )
                   )
            )
          )
        )
      ),
      
      # ENHANCED DESCRIPTIVE CATEGORICAL TAB
      tabItem(
        tabName = "desc_categoric",
        fluidRow(
          column(12, h2("🏷️ Analisis Deskriptif - Variabel Kategorik", style = "color: #3b0f70; font-weight: bold; text-align: center; margin-bottom: 30px;"))
        ),
        
        fluidRow(
          column(
            width = 3,
            box(
              title = "🎯 Pilih Variabel", width = 12, status = "primary", solidHeader = TRUE,
              selectInput("desc_cat_var", "Variabel Kategorik:", choices = NULL),
              br(),
              actionButton("refresh_desc_cat", "🔄 Refresh Analisis", class = "btn-info", style = "width: 100%;")
            )
          ),
          column(
            width = 9,
            conditionalPanel(
              condition = "output.desc_cat_available",
              box(
                title = "📋 Tabel Frekuensi Lengkap", width = 12, status = "info", solidHeader = TRUE,
                DTOutput("desc_freq_table"),
                div(style = "margin-top: 15px; padding: 15px; background: rgba(74, 144, 226, 0.1); border-radius: 10px; border-left: 4px solid #4a90e2;",
                    h5("💡 Interpretasi Tabel Frekuensi:", style = "color: #4a90e2; margin-bottom: 10px;"),
                    htmlOutput("freq_table_interpretation")
                )
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.desc_cat_available",
          fluidRow(
            box(
              title = "📊 Bar Chart Interaktif", width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("desc_bar_chart", height = "400px"),
              div(style = "margin-top: 15px; padding: 15px; background: rgba(94, 79, 162, 0.1); border-radius: 10px; border-left: 4px solid #5e4fa2;",
                  h5("💡 Interpretasi Bar Chart:", style = "color: #3b0f70; margin-bottom: 10px;"),
                  htmlOutput("bar_chart_interpretation")
              )
            ),
            box(
              title = "🥧 Pie Chart Dinamis", width = 6, status = "warning", solidHeader = TRUE,
              plotlyOutput("desc_pie_chart", height = "400px"),
              div(style = "margin-top: 15px; padding: 15px; background: rgba(253, 126, 20, 0.1); border-radius: 10px; border-left: 4px solid #fd7e14;",
                  h5("💡 Interpretasi Pie Chart:", style = "color: #fd7e14; margin-bottom: 10px;"),
                  htmlOutput("pie_chart_interpretation")
              )
            )
          )
        ),
        conditionalPanel(
          condition = "output.desc_cat_available",
          fluidRow(
            column(12,
                   div(class = "download-section",
                       h5("📥 Download Analisis Deskriptif Kategorik"),
                       div(class = "download-buttons",
                           create_download_button("download_desc_bar", "📊 Download Bar Chart", "chart-column"),
                           create_download_button("download_desc_pie", "🥧 Download Pie Chart", "chart-pie"),
                           create_download_button("download_desc_freq", "📋 Download Tabel Frekuensi", "table"),
                           create_download_button("download_desc_cat_pdf", "📄 Download PDF Lengkap", "file-pdf", "warning")
                       )
                   )
            )
          )
        )
      ),
      
      # ENHANCED MAP TAB
      tabItem(
        tabName = "desc_map",
        fluidRow(
          column(12, h2("🗺️ Peta Interaktif Indonesia", style = "color: #3b0f70; font-weight: bold; text-align: center; margin-bottom: 30px;"))
        ),
        
        fluidRow(
          column(
            width = 4,
            box(
              title = "⚙️ Pengaturan Peta", status = "primary", solidHeader = TRUE, width = 12,
              selectInput("map_vars", "Pilih 1 atau 2 variabel:",
                          choices = names(select_if(data_raw, is.numeric)),
                          multiple = TRUE, selectize = TRUE),
              actionButton("update_choropleth", "🗺️ Perbarui Peta", class = "btn-primary", style = "width: 100%;"),
              br(), br(),
              div(id = "map_info", style = "background: #f8f9ff; padding: 15px; border-radius: 10px; border: 2px solid #5e4fa2;",
                  h5("ℹ️ Informasi Peta", style = "color: #3b0f70; margin-bottom: 10px;"),
                  p("• Pilih 1 variabel untuk peta univariat", style = "color: #5e4fa2; margin: 5px 0;"),
                  p("• Pilih 2 variabel untuk peta bivariat", style = "color: #5e4fa2; margin: 5px 0;"),
                  p("• Klik pada wilayah untuk detail", style = "color: #5e4fa2; margin: 5px 0;")
              )
            ),
            conditionalPanel(
              condition = "output.map_updated",
              uiOutput("legend_box")
            )
          ),
          
          column(
            width = 8,
            box(
              title = "🌏 Peta Choropleth Indonesia", status = "info", solidHeader = TRUE, width = 12,
              leafletOutput("choropleth_map", height = "600px"),
              div(style = "margin-top: 15px; padding: 15px; background: rgba(74, 144, 226, 0.1); border-radius: 10px; border-left: 4px solid #4a90e2;",
                  h5("💡 Interpretasi Peta:", style = "color: #4a90e2; margin-bottom: 10px;"),
                  htmlOutput("map_interpretation")
              )
            )
          )
        ),
        conditionalPanel(
          condition = "output.map_updated",
          fluidRow(
            column(12,
                   div(class = "download-section",
                       h5("📥 Download Peta dan Analisis"),
                       div(class = "download-buttons",
                           create_download_button("download_map_image", "🗺️ Download Peta", "map"),
                           create_download_button("download_map_data", "📊 Download Data Peta", "table"),
                           create_download_button("download_map_pdf", "📄 Download PDF Lengkap", "file-pdf", "warning")
                       )
                   )
            )
          )
        )
      ),
      
      # NEW: UJI NORMALITAS TAB
      tabItem(
        tabName = "uji_normalitas",
        fluidRow(
          column(12, h2("📊 Uji Normalitas (1 Populasi)", style = "color: #3b0f70; font-weight: bold; text-align: center; margin-bottom: 30px;"))
        ),
        
        fluidRow(
          column(
            width = 4,
            box(
              title = "⚙️ Pengaturan Uji Normalitas", width = 12, status = "primary", solidHeader = TRUE,
              selectInput("norm_var", "Pilih Variabel Numerik:", choices = NULL),
              selectInput("norm_cat_var", "Variabel Kategorik (Opsional):",
                          choices = c("Semua Data" = "", "Pilih Variabel...")),
              conditionalPanel(
                condition = "input.norm_cat_var != ''",
                selectInput("norm_category", "Pilih Kategori:", choices = NULL)
              ),
              selectInput("norm_test_method", "Metode Uji:",
                          choices = c("Shapiro-Wilk" = "shapiro",
                                      "Anderson-Darling" = "ad",
                                      "Kolmogorov-Smirnov" = "ks",
                                      "Lilliefors" = "lillie")),
              numericInput("alpha_norm", "Tingkat Signifikansi (α):", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
              actionButton("run_norm_test", "🧪 Jalankan Uji", class = "btn-primary", style = "width: 100%;")
            )
          ),
          column(
            width = 8,
            conditionalPanel(
              condition = "output.norm_test_done",
              box(
                title = "📋 Hasil Uji Normalitas", width = 12, status = "info", solidHeader = TRUE,
                htmlOutput("norm_test_results")
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.norm_test_done",
          fluidRow(
            box(
              title = "📊 Q-Q Plot", width = 6, status = "warning", solidHeader = TRUE,
              plotlyOutput("norm_qq_plot", height = "400px"),
              div(style = "margin-top: 15px; padding: 15px; background: rgba(253, 126, 20, 0.1); border-radius: 10px; border-left: 4px solid #fd7e14;",
                  h5("💡 Interpretasi Q-Q Plot:", style = "color: #fd7e14; margin-bottom: 10px;"),
                  htmlOutput("qq_plot_interpretation")
              )
            ),
            box(
              title = "📈 Histogram dengan Kurva Normal", width = 6, status = "success", solidHeader = TRUE,
              plotlyOutput("norm_histogram", height = "400px"),
              div(style = "margin-top: 15px; padding: 15px; background: rgba(40, 167, 69, 0.1); border-radius: 10px; border-left: 4px solid #28a745;",
                  h5("💡 Interpretasi Histogram:", style = "color: #28a745; margin-bottom: 10px;"),
                  htmlOutput("norm_histogram_interpretation")
              )
            )
          )
        ),
        conditionalPanel(
          condition = "output.norm_test_done",
          fluidRow(
            column(12,
                   div(class = "download-section",
                       h5("📥 Download Hasil Uji Normalitas"),
                       div(class = "download-buttons",
                           create_download_button("download_norm_qq", "📊 Download Q-Q Plot", "chart-bar"),
                           create_download_button("download_norm_hist", "📈 Download Histogram", "chart-bar"),
                           create_download_button("download_norm_results", "📋 Download Hasil Uji", "table"),
                           create_download_button("download_norm_pdf", "📄 Download PDF Lengkap", "file-pdf", "warning")
                       )
                   )
            )
          )
        )
      ),
      
      # NEW: UJI HOMOGENITAS TAB
      tabItem(
        tabName = "uji_homogenitas",
        fluidRow(
          column(12, h2("📏 Uji Homogenitas (1 Populasi)", style = "color: #3b0f70; font-weight: bold; text-align: center; margin-bottom: 30px;"))
        ),
        
        fluidRow(
          column(
            width = 4,
            box(
              title = "⚙️ Pengaturan Uji Homogenitas", width = 12, status = "primary", solidHeader = TRUE,
              selectInput("homog_var", "Pilih Variabel Numerik:", choices = NULL),
              selectInput("homog_cat_var", "Variabel Kategorik:", choices = NULL),
              selectInput("homog_test_method", "Metode Uji:",
                          choices = c("Levene Test" = "levene",
                                      "Bartlett Test" = "bartlett",
                                      "Fligner-Killeen" = "fligner")),
              numericInput("alpha_homog", "Tingkat Signifikansi (α):", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
              actionButton("run_homog_test", "🧪 Jalankan Uji", class = "btn-primary", style = "width: 100%;")
            )
          ),
          column(
            width = 8,
            conditionalPanel(
              condition = "output.homog_test_done",
              box(
                title = "📋 Hasil Uji Homogenitas", width = 12, status = "info", solidHeader = TRUE,
                htmlOutput("homog_test_results")
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.homog_test_done",
          fluidRow(
            box(
              title = "📦 Box Plot per Grup", width = 6, status = "warning", solidHeader = TRUE,
              plotlyOutput("homog_boxplot", height = "400px"),
              div(style = "margin-top: 15px; padding: 15px; background: rgba(253, 126, 20, 0.1); border-radius: 10px; border-left: 4px solid #fd7e14;",
                  h5("💡 Interpretasi Box Plot:", style = "color: #fd7e14; margin-bottom: 10px;"),
                  htmlOutput("homog_boxplot_interpretation")
              )
            ),
            box(
              title = "📊 Statistik Deskriptif per Grup", width = 6, status = "success", solidHeader = TRUE,
              htmlOutput("homog_descriptive_stats"),
              div(style = "margin-top: 15px; padding: 15px; background: rgba(40, 167, 69, 0.1); border-radius: 10px; border-left: 4px solid #28a745;",
                  h5("💡 Interpretasi Statistik:", style = "color: #28a745; margin-bottom: 10px;"),
                  htmlOutput("homog_stats_interpretation")
              )
            )
          )
        ),
        conditionalPanel(
          condition = "output.homog_test_done",
          fluidRow(
            column(12,
                   div(class = "download-section",
                       h5("📥 Download Hasil Uji Homogenitas"),
                       div(class = "download-buttons",
                           create_download_button("download_homog_boxplot", "📦 Download Box Plot", "chart-simple"),
                           create_download_button("download_homog_violin", "🎻 Download Violin Plot", "chart-area"),
                           create_download_button("download_homog_results", "📋 Download Hasil Uji", "table"),
                           create_download_button("download_homog_pdf", "📄 Download PDF Lengkap", "file-pdf", "warning")
                       )
                   )
            )
          )
        )
      ),
      
      # ENHANCED HYPOTHESIS TESTING TABS
      tabItem(
        tabName = "uji_mean",
        
        fluidRow(
          column(12, h2("📊 Uji Beda Rata-rata", 
                        style = "color: #3b0f70; font-weight: bold; text-align: center; margin-bottom: 30px;"))
        ),
        
        fluidRow(
          column(
            width = 4,
            box(
              title = "⚙️ Pengaturan Uji Hipotesis", width = 12, status = "primary", solidHeader = TRUE,
              
              selectInput("mean_numeric_var", "Variabel Numerik:", choices = NULL),
              
              selectInput("mean_cat_var", "Variabel Kategorik 1:",
                          choices = c("Semua Data" = "", "Pilih Variabel...")),
              
              selectInput("mean_cat_var2", "Variabel Kategorik 2 (Opsional):",
                          choices = c("Tidak Ada" = "", "Pilih Variabel...")),
              
              # Kondisi hanya untuk One-Way
              conditionalPanel(
                condition = "input.mean_cat_var != '' && input.mean_cat_var2 == ''",
                selectInput("mean_categories", "Pilih Kategori untuk Diuji:",
                            choices = NULL, multiple = TRUE)
              ),
              
              # Kondisi jika bukan two-way dan hanya 1 kategori
              conditionalPanel(
                condition = "(input.mean_cat_var2 == '') && 
                       (input.mean_cat_var == '' || 
                        (input.mean_categories != null && input.mean_categories.length == 1))",
                numericInput("mu_0_mean", "Nilai μ₀ (Hipotesis):", value = 0, step = 0.01)
              ),
              conditionalPanel(
                condition = "input.mean_cat_var != '' && input.mean_cat_var2 != ''",
                checkboxInput("include_interaction", "Sertakan Interaksi", value = TRUE)
              ),
              
              
              # Arah hipotesis jika bukan >2 kategori
              conditionalPanel(
                condition = "(input.mean_cat_var2 == '') && 
                       (input.mean_categories == '' || input.mean_categories.length <= 2)",
                selectInput("mean_alternative", "Arah Hipotesis Alternatif:",
                            choices = c("≠ (dua sisi)" = "two.sided", 
                                        "> (kanan)" = "greater", 
                                        "< (kiri)" = "less"),
                            selected = "two.sided")
              ),
              
              numericInput("alpha_mean", "Tingkat Signifikansi (α):", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
              
              actionButton("run_mean_test", "🧪 Jalankan Uji", class = "btn-primary", style = "width: 100%;")
            )
          ),
          
          column(
            width = 8,
            box(
              title = "📊 Hipotesis", width = 12, status = "info", solidHeader = TRUE,
              htmlOutput("mean_hypothesis_preview")
            ),
            conditionalPanel(
              condition = "output.mean_test_done",
              box(
                title = "📋 Hasil Uji Hipotesis", width = 12, status = "info", solidHeader = TRUE,
                htmlOutput("mean_test_results")
              )
            )
          )
        ),
        conditionalPanel(
          condition = "output.mean_test_done",
          fluidRow(
            column(12,
                   div(class = "download-section",
                       h5("📥 Download Hasil Uji Beda Rata-rata"),
                       div(class = "download-buttons",
                           create_download_button("download_mean_plot", "📊 Download Plot", "chart-column"),
                           create_download_button("download_mean_stats", "📈 Download Statistik", "table"),
                           create_download_button("download_mean_results", "📋 Download Hasil Uji", "table"),
                           create_download_button("download_mean_pdf", "📄 Download PDF Lengkap", "file-pdf", "warning")
                       )
                   )
            )
          )
        )
      ),
      
      
      
      tabItem(
        tabName = "uji_var",
        fluidRow(
          column(12, h2("📏 Uji Varians", style = "color: #3b0f70; font-weight: bold; text-align: center; margin-bottom: 30px;"))
        ),
        
        fluidRow(
          column(
            width = 4,
            box(
              title = "⚙️ Pengaturan Uji Varians", width = 12, status = "primary", solidHeader = TRUE,
              selectInput("var_cat_var", "Variabel Kategorik (Opsional):",
                          choices = c("Semua Data" = "", "Pilih Variabel...")),
              conditionalPanel(
                condition = "input.var_cat_var != ''",
                selectInput("var_categories", "Pilih Kategori untuk Diuji:",
                            choices = NULL, multiple = TRUE)
              ),
              conditionalPanel(
                condition = "input.var_cat_var == '' || (input.var_categories != null && input.var_categories.length == 1)",
                numericInput("sigma2_0_var", "Nilai σ²₀ (Hipotesis):", value = 1, step = 0.01)
              ),
              selectInput("var_numeric_var", "Variabel Numerik:", choices = NULL),
              numericInput("alpha_var", "Tingkat Signifikansi (α):", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
              actionButton("run_var_test", "🧪 Jalankan Uji", class = "btn-primary", style = "width: 100%;")
            )
          ),
          column(
            width = 8,
            conditionalPanel(
              condition = "output.var_test_done",
              box(
                title = "📋 Hasil Uji Varians", width = 12, status = "info", solidHeader = TRUE,
                htmlOutput("var_test_results")
              )
            )
          )
        ),
        conditionalPanel(
          condition = "output.var_test_done",
          fluidRow(
            column(12,
                   div(class = "download-section",
                       h5("📥 Download Hasil Uji Varians"),
                       div(class = "download-buttons",
                           create_download_button("download_var_plot", "📊 Download Plot", "chart-simple"),
                           create_download_button("download_var_stats", "📈 Download Statistik", "table"),
                           create_download_button("download_var_results", "📋 Download Hasil Uji", "table"),
                           create_download_button("download_var_pdf", "📄 Download PDF Lengkap", "file-pdf", "warning")
                       )
                   )
            )
          )
        )
      ),
      
      tabItem(
        tabName = "uji_prop",
        fluidRow(
          column(12, h2("🎯 Uji Proporsi Lanjutan", style = "color: #3b0f70; font-weight: bold; text-align: center; margin-bottom: 30px;"))
        ),
        
        fluidRow(
          column(
            width = 4,
            box(
              title = "⚙️ Pengaturan Uji Proporsi", width = 12, status = "primary", solidHeader = TRUE,
              selectInput("prop_test_var", "Variabel untuk Diuji:", choices = NULL),
              selectInput("prop_cat_var", "Variabel Kategorik (Opsional):",
                          choices = c("Semua Data" = "", "Pilih Variabel...")),
              conditionalPanel(
                condition = "input.prop_cat_var != ''",
                selectInput("prop_categories", "Pilih Kategori untuk Diuji:",
                            choices = NULL, multiple = TRUE)
              ),
              selectInput("prop_success", "Kategori Sukses:", choices = NULL),
              conditionalPanel(
                condition = "input.prop_categories != null && input.prop_categories.length == 1",
                numericInput("p_0_prop", "Nilai p₀ (Hipotesis):", value = 0.5, min = 0, max = 1, step = 0.01)
              ),
              numericInput("alpha_prop", "Tingkat Signifikansi (α):", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
              actionButton("run_prop_test", "🧪 Jalankan Uji", class = "btn-primary", style = "width: 100%;")
            )
          ),
          column(
            width = 8,
            conditionalPanel(
              condition = "output.prop_test_done",
              box(
                title = "📋 Hasil Uji Proporsi", width = 12, status = "info", solidHeader = TRUE,
                htmlOutput("prop_test_results")
              )
            )
          )
        ),
        conditionalPanel(
          condition = "output.prop_test_done",
          fluidRow(
            column(12,
                   div(class = "download-section",
                       h5("📥 Download Hasil Uji Proporsi"),
                       div(class = "download-buttons",
                           create_download_button("download_prop_plot", "📊 Download Plot", "chart-pie"),
                           create_download_button("download_prop_table", "📈 Download Tabel", "table"),
                           create_download_button("download_prop_results", "📋 Download Hasil Uji", "table"),
                           create_download_button("download_prop_pdf", "📄 Download PDF Lengkap", "file-pdf", "warning")
                       )
                   )
            )
          )
        )
      ),
      
      # ENHANCED OLS REGRESSION TAB
      tabItem(
        tabName = "ols_reg",
        fluidRow(
          column(12, h2("📈 Analisis Regresi OLS Lanjutan", style = "color: #3b0f70; font-weight: bold; text-align: center; margin-bottom: 30px;"))
        ),
        
        fluidRow(
          column (
            width = 4,
            box(
              title = "⚙️ Pengaturan Model Regresi", width = 12, status = "primary", solidHeader = TRUE,
              selectInput("ols_y", "Variabel Dependen (Y):", choices = NULL),
              selectInput("ols_x", "Variabel Independen (X):", choices = NULL, multiple = TRUE),
              numericInput("alpha_ols", "Tingkat Signifikansi (α):", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
              actionButton("run_ols", "📈 Jalankan Regresi", class = "btn-primary", style = "width: 100%;"),
              br(), br(),
              conditionalPanel(
                condition = "output.assumption_tests_done",
                actionButton("test_ols_assumptions", "🔍 Uji Asumsi", class = "btn-info", style = "width: 100%;")
              )
            )
          ),
          column (
            width = 8,
            conditionalPanel(
              condition = "output.assumption_tests_done",
              box(
                title = "📊 Hasil Regresi OLS", width = 12, status = "info", solidHeader = TRUE,
                htmlOutput("ols_results")
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.assumption_tests_done",
          fluidRow(
            box(
              title = "📈 Plot Diagnostik", width = 6, status = "warning", solidHeader = TRUE,
              plotlyOutput("ols_diagnostic", height = "600px"),
              div(style = "margin-top: 15px; padding: 15px; background: rgba(253, 126, 20, 0.1); border-radius: 10px; border-left: 4px solid #fd7e14;",
                  h5("💡 Interpretasi Plot Diagnostik:", style = "color: #fd7e14; margin-bottom: 10px;"),
                  htmlOutput("ols_diagnostic_interpretation")
              )
            ),
            box(
              title = "💡 Interpretasi Model", width = 6, status = "success", solidHeader = TRUE,
              htmlOutput("ols_interpretation")
            )
          )
        ),
        conditionalPanel(
          condition = "output.ols_done",
          fluidRow(
            column(12,
                   div(class = "download-section",
                       h5("📥 Download Hasil Regresi OLS"),
                       div(class = "download-buttons",
                           create_download_button("download_ols_scatter", "📊 Download Scatter Plot", "chart-line"),
                           create_download_button("download_ols_residual", "📈 Download Residual Plot", "chart-line"),
                           create_download_button("download_ols_results", "📋 Download Hasil Regresi", "table"),
                           create_download_button("download_ols_diagnostic", "🧪 Download Uji Diagnostik", "table"),
                           create_download_button("download_ols_pdf", "📄 Download PDF Lengkap", "file-pdf", "warning")
                       )
                   )
            )
          )
        )
      ),
      
      # ENHANCED SPATIAL REGRESSION TAB
      tabItem(
        tabName = "spatial_reg",
        fluidRow(
          column(12, h2("🗺️ Analisis Regresi Spasial Lanjutan", style = "color: #3b0f70; font-weight: bold; text-align: center; margin-bottom: 30px;"))
        ),
        
        fluidRow(
          column(
            width = 4,
            box(
              title = "⚙️ Pengaturan Model Spasial", width = 12, status = "primary", solidHeader = TRUE,
              selectInput("spatial_y", "Variabel Dependen (Y):", choices = NULL),
              selectInput("spatial_x", "Variabel Independen (X):", choices = NULL, multiple = TRUE),
              selectInput("spatial_contiguity", "Metode Ketetanggaan:",
                          choices = c("Queen's Contiguity" = "queen",
                                      "Rook's Contiguity" = "rook",
                                      "K-Nearest Neighbors" = "knn",
                                      "Distance-based" = "distance"), selected = "knn"),
              conditionalPanel(
                condition = "input.spatial_contiguity == 'knn'",
                numericInput("spatial_k", "Jumlah Tetangga (k):", value = 5, min = 1, max = 20)
              ),
              conditionalPanel(
                condition = "input.spatial_contiguity == 'distance'",
                numericInput("spatial_distance", "Threshold Jarak:", value = 1, min = 0.1, step = 0.1)
              ),
              conditionalPanel(
                condition = "output.spatial_diagnostics_available",
                selectInput("spatial_model_type", "Jenis Model Spasial:",
                          choices = c("Spatial Lag" = "lag",
                                      "Spatial Error" = "error",
                                      "Spatial Durbin" = "durbin"), selected = "error")
              ),
              numericInput("alpha_spatial", "Tingkat Signifikansi (α):", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
              actionButton("run_spatial_diag", "🗺️ Diagnosis Spasial", class = "btn-primary", style = "width: 100%;"),
              br(), br(),
              conditionalPanel(
                condition = "output.spatial_diagnostics_available",
                actionButton("run_spatial", "🗺️ Jalankan Regresi Spasial", class = "btn-primary", style = "width: 100%;")
                
              )
            )
          ),
          column(
            width = 8,
            conditionalPanel(
              condition = "output.spatial_diagnostics_available",
              box(
                title = "🧪 Diagnosis Spasial", width = 12, status = "warning", solidHeader = TRUE,
                htmlOutput("spatial_diagnostics"),
                div(style = "margin-top: 15px; padding: 15px; background: rgba(253, 126, 20, 0.1); border-radius: 10px; border-left: 4px solid #fd7e14;",
                    h5("💡 Interpretasi Diagnosis:", style = "color: #fd7e14; margin-bottom: 10px;"),
                    htmlOutput("spatial_diagnosis_interpretation")
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.spatial_results_available",
              box(
                title = "📊 Hasil Regresi Spasial", width = 12, status = "info", solidHeader = TRUE,
                htmlOutput("spatial_results")
              )
            )
          )
        ),
        conditionalPanel(
          condition = "output.spatial_results_available",
          fluidRow(
            box(
              title = "💡 Interpretasi Model Spasial", width = 12, status = "success", solidHeader = TRUE,
              htmlOutput("spatial_interpretation")
            )
          )
        ),
        conditionalPanel(
          condition = "output.spatial_done",
          fluidRow(
            column(12,
                   div(class = "download-section",
                       h5("📥 Download Hasil Regresi Spasial"),
                       div(class = "download-buttons",
                           create_download_button("download_spatial_map", "🗺️ Download Peta Residual", "map"),
                           create_download_button("download_spatial_results", "📋 Download Hasil Regresi", "table"),
                           create_download_button("download_spatial_moran", "📊 Download Moran's I", "table"),
                           create_download_button("download_spatial_diagnostic", "🧪 Download Uji Diagnostik", "table"),
                           create_download_button("download_spatial_pdf", "📄 Download PDF Lengkap", "file-pdf", "warning")
                       )
                   )
            )
          )
        )
      )
    )
  ),
  footer = dashboardFooter(
    left = "Aulia Ul Hasanah | 222313000", 
    right = "Dashboard Grid Gap"
  )
)

server <- function(input, output, session) {
  # Enhanced reactive values
  values <- reactiveValues(
    data = data_raw,
    categorized_vars = character(0),
    transformed_vars = character(0),
    spatial_weights = NULL,
    ols_model = NULL,
    spatial_model = NULL,
    last_update = Sys.time()
  )
  
  # Enhanced variable choices update
  observe({
    numeric_vars <- names(select_if(values$data, is.numeric))
    categorical_vars <- names(select_if(values$data, function(x) is.character(x) | is.factor(x)))
    
    available_for_transform <- setdiff(numeric_vars, values$categorized_vars)
    
    updateSelectInput(session, "trans_var", choices = available_for_transform)
    updateSelectInput(session, "desc_cat_var", choices = categorical_vars)
    updateSelectInput(session, "mean_cat_var", choices = c("Semua Data" = "", categorical_vars))
    updateSelectInput(session, "mean_cat_var2", choices = c("Semua Data" = "", categorical_vars))
    updateSelectInput(session, "mean_numeric_var", choices = numeric_vars)
    updateSelectInput(session, "var_cat_var", choices = c("Semua Data" = "", categorical_vars))
    updateSelectInput(session, "var_numeric_var", choices = numeric_vars)
    updateSelectInput(session, "prop_test_var", choices = categorical_vars)
    updateSelectInput(session, "prop_cat_var", choices = c("Semua Data" = "", categorical_vars))
    updateSelectInput(session, "ols_y", choices = numeric_vars, selected = "NOELECTRIC")
    updateSelectInput(session, "ols_x", choices = numeric_vars, selected = c("POVERTY", "FAMILYSIZE", "TAPWATER", "ILLITERATE", "NOSEWER"))
    updateSelectInput(session, "spatial_y", choices = numeric_vars, selected = "NOELECTRIC")
    updateSelectInput(session, "spatial_x", choices = numeric_vars, selected = c("POVERTY", "FAMILYSIZE", "TAPWATER", "ILLITERATE", "NOSEWER"))
    
    # NEW: Update choices for normality and homogeneity tests
    updateSelectInput(session, "norm_var", choices = numeric_vars)
    updateSelectInput(session, "norm_cat_var", choices = c("Semua Data" = "", categorical_vars))
    updateSelectInput(session, "homog_var", choices = numeric_vars)
    updateSelectInput(session, "homog_cat_var", choices = categorical_vars)
  })
  
  # ENHANCED HOME TAB OUTPUTS
  output$total_obs <- renderValueBox({
    valueBox(
      value = tags$span(nrow(values$data), style = "font-size: 2rem; font-weight: bold;"),
      subtitle = tags$span("Total Observasi", style = "font-size: 1rem;"),
      icon = icon("database", style = "font-size: 2rem;"),
      color = "purple"
    )
  })
  
  output$avg_poverty <- renderValueBox({
    valueBox(
      value = tags$span(paste0(round(mean(values$data$POVERTY, na.rm = TRUE), 1), "%"), style = "font-size: 2rem; font-weight: bold;"),
      subtitle = tags$span("Rata-rata Kemiskinan", style = "font-size: 1rem;"),
      icon = icon("chart-line", style = "font-size: 2rem;"),
      color = "fuchsia"
    )
  })
  
  output$avg_noelectric <- renderValueBox({
    valueBox(
      value = tags$span(paste0(round(mean(values$data$NOELECTRIC, na.rm = TRUE), 1), "%"), style = "font-size: 2rem; font-weight: bold;"),
      subtitle = tags$span("Rata-rata Tanpa Listrik", style = "font-size: 1rem;"),
      icon = icon("bolt", style = "font-size: 2rem;"),
      color = "info"
    )
  })
  
 
  # Enhanced metadata table
  output$metadata_table <- renderDT({
    metadata <- data.frame(
      "Variabel" = c("NOELECTRIC", "POVERTY", "FAMILYSIZE", "TAPWATER", "ILLITERATE", "NOSEWER", "REGION"),
      "Deskripsi" = c("Proporsi rumah tanpa listrik", "Tingkat kemiskinan", "Ukuran keluarga rata-rata",
                      "Akses air bersih", "Tingkat buta huruf", "Rumah tanpa saluran pembuangan", "Wilayah geografis"),
      "Skala" = c("Rasio", "Rasio", "Rasio", "Rasio", "Rasio", "Rasio", "Nominal"),
      "Range" = c("0-1", "0-1", "1-10", "0-1", "0-1", "0-1", "Kategorikal"),
      stringsAsFactors = FALSE
    )
    
    # Add emojis to column names after creation
    colnames(metadata) <- c("🏷️ Variabel", "📝 Deskripsi", "📊 Skala", "📈 Range")
    
    datatable(metadata, 
              options = list(
                pageLength = 10, 
                dom = 't',
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background': 'linear-gradient(135deg, #3b0f70 0%, #5e4fa2 100%)', 'color': '#ffff99', 'font-weight': 'bold'});",
                  "}"
                )
              ), 
              rownames = FALSE) %>%
      formatStyle(columns = 1:4, backgroundColor = "rgba(248, 249, 255, 0.8)", color = "#3b0f70")
  })
  
 
  cat_histogram_data <- eventReactive(input$create_categories, {
    req(input$cat_var)
    values$data[[input$cat_var]]
  })
  
  output$cat_histogram <- renderPlotly({
    var_data <- cat_histogram_data()
    
    p <- ggplot(data.frame(x = var_data), aes(x = x)) +
      geom_histogram(bins = 30, fill = "#5e4fa2", alpha = 0.8, color = "white", size = 0.5) +
      geom_density(aes(y = ..density.. * length(var_data) * diff(range(var_data))/30), 
                   color = "#3b0f70", size = 1.5, alpha = 0.7) +
      labs(title = paste("📊 Distribusi", input$cat_var), 
           x = input$cat_var, 
           y = "Frekuensi",
           subtitle = paste("n =", length(var_data), "| Mean =", round(mean(var_data, na.rm = TRUE), 3))) +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#3b0f70", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#5e4fa2", size = 12),
        axis.title = element_text(color = "#5e4fa2", size = 12, face = "bold"),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        panel.grid.minor = element_line(color = "grey95", size = 0.3)
      )
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # NEW: Distribution interpretation for categorization
  output$cat_distribution_interpretation <- renderUI({
    req(input$create_categories)
    var_data <- cat_histogram_data()
    
    skew_val <- skewness(var_data, na.rm = TRUE)
    kurt_val <- kurtosis(var_data, na.rm = TRUE)
    
    skew_interp <- ifelse(abs(skew_val) < 0.5, "simetris", 
                          ifelse(skew_val > 0.5, "condong kanan (positif)", "condong kiri (negatif)"))
    
    kurt_interp <- ifelse(abs(kurt_val) > 3, "leptokurtik (runcing)", 
                          ifelse(abs(kurt_val) < 3, "platykurtik (datar)", "mesokurtik (normal)"))
    
    HTML(paste0(
      '<p style="color: #5e4fa2; margin: 5px 0;">📊 <strong>Bentuk Distribusi:</strong> ', skew_interp, '</p>',
      '<p style="color: #5e4fa2; margin: 5px 0;">📐 <strong>Kurtosis:</strong> ', kurt_interp, '</p>',
      '<p style="color: #5e4fa2; margin: 5px 0;">🎯 <strong>Rekomendasi:</strong> ',
      switch(input$cat_method,
             "natural" = "Natural breaks cocok untuk data dengan cluster alami",
             "quantile" = "Quantile memberikan distribusi kategori yang seimbang",
             "equal" = "Equal interval cocok untuk data yang terdistribusi merata",
             "sd" = "Standard deviation cocok untuk data yang mendekati normal"),
      '</p>'
    ))
  })
  
  # FIXED categorization logic
  observeEvent(input$create_categories, {
    req(input$cat_var, input$cat_method, input$cat_n_classes)
    
    tryCatch({
      var_data <- values$data[[input$cat_var]]
      n_classes <- input$cat_n_classes
      
      # Enhanced break creation
      breaks <- switch(input$cat_method,
                       "natural" = classIntervals(var_data, n = n_classes, style = "jenks")$brks,
                       "quantile" = quantile(var_data, probs = seq(0, 1, length.out = n_classes + 1), na.rm = TRUE),
                       "equal" = seq(min(var_data, na.rm = TRUE), max(var_data, na.rm = TRUE), length.out = n_classes + 1),
                       "sd" = {
                         mean_val <- mean(var_data, na.rm = TRUE)
                         sd_val <- sd(var_data, na.rm = TRUE)
                         c(min(var_data, na.rm = TRUE), 
                           mean_val - sd_val, mean_val, mean_val + sd_val, 
                           max(var_data, na.rm = TRUE))[1:(n_classes + 1)]
                       }
      )
      
      labels <- trimws(unlist(strsplit(input$cat_labels, ",")))
      if(length(labels) != n_classes) {
        labels <- paste("Kategori", 1:n_classes)
      }
      
      cat_var_name <- paste0(input$cat_var, "_cat")
      values$data[[cat_var_name]] <- cut(var_data, breaks = breaks, labels = labels, include.lowest = TRUE)
      values$categorized_vars <- c(values$categorized_vars, input$cat_var)
      values$last_update <- Sys.time()
      
      output$categories_created <- reactive({ TRUE })
      outputOptions(output, "categories_created", suspendWhenHidden = FALSE)
      
      showNotification("✅ Kategorisasi berhasil dibuat!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("❌ Error kategorisasi:", e$message), type = "error", duration = 5)
    })
  })
  
  # Enhanced categorization results - hanya update saat tombol ditekan
  output$cat_barchart <- renderPlotly({
    req(input$create_categories)
    cat_var_name <- paste0(input$cat_var, "_cat")
    
    if(cat_var_name %in% names(values$data)) {
      freq_data <- as.data.frame(table(values$data[[cat_var_name]]))
      names(freq_data) <- c("Category", "Frequency")
      freq_data$Percentage <- round(freq_data$Frequency / sum(freq_data$Frequency) * 100, 1)
      
      p <- ggplot(freq_data, aes(x = Category, y = Frequency, fill = Category, text = paste("Kategori:", Category, "<br>Frekuensi:", Frequency, "<br>Persentase:", Percentage, "%"))) +
        geom_bar(stat = "identity", alpha = 0.8, color = "white", size = 0.5) +
        scale_fill_manual(values = c("#3b0f70", "#5e4fa2", "#9970ab", "#c2a5cf", "#e7d4e8", "#ffff99")[1:nrow(freq_data)]) +
        labs(title = paste("📈 Hasil Kategorisasi", input$cat_var), 
             x = "Kategori", 
             y = "Frekuensi",
             subtitle = paste("Total:", sum(freq_data$Frequency), "observasi")) +
        theme_minimal() +
        theme(
          plot.title = element_text(color = "#3b0f70", size = 16, face = "bold"),
          plot.subtitle = element_text(color = "#5e4fa2", size = 12),
          axis.title = element_text(color = "#5e4fa2", size = 12, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, color = "#5e4fa2"),
          legend.position = "none",
          panel.grid.major = element_line(color = "grey90", size = 0.5)
        )
      
      ggplotly(p, tooltip = "text")
    }
  })
  
  # Enhanced categorization statistics - hanya update saat tombol ditekan
  output$cat_statistics <- renderUI({
    req(input$create_categories)
    cat_var_name <- paste0(input$cat_var, "_cat")
    
    if(cat_var_name %in% names(values$data)) {
      freq_table <- table(values$data[[cat_var_name]])
      total <- sum(freq_table)
      
      stats_html <- '<div class="stats-card">'
      stats_html <- paste0(stats_html, '<div class="stats-header">📊 Statistik Kategorisasi Lengkap</div>')
      stats_html <- paste0(stats_html, '<div class="stats-content">')
      
      # Add summary statistics
      stats_html <- paste0(stats_html, '<h5 style="color: #3b0f70; margin-bottom: 15px;">📈 Ringkasan:</h5>')
      stats_html <- paste0(stats_html, '<p><strong>Total Observasi:</strong> ', total, '</p>')
      stats_html <- paste0(stats_html, '<p><strong>Jumlah Kategori:</strong> ', length(freq_table), '</p>')
      stats_html <- paste0(stats_html, '<p><strong>Metode:</strong> ', 
                           switch(input$cat_method,
                                  "natural" = "Natural Breaks (Jenks)",
                                  "quantile" = "Quantile (Equal Count)", 
                                  "equal" = "Equal Interval",
                                  "sd" = "Standard Deviation"), '</p>')
      
      stats_html <- paste0(stats_html, '<hr style="border-color: #5e4fa2; margin: 15px 0;">')
      stats_html <- paste0(stats_html, '<h5 style="color: #3b0f70; margin-bottom: 15px;">📋 Detail per Kategori:</h5>')
      
      for(i in 1:length(freq_table)) {
        cat_name <- names(freq_table)[i]
        freq <- freq_table[i]
        prop <- round(freq/total, 4)
        
        stats_html <- paste0(stats_html, 
                             '<div style="background: rgba(94, 79, 162, 0.1); padding: 10px; margin: 8px 0; border-radius: 8px; border-left: 4px solid #5e4fa2;">',
                             '<p style="margin: 0;"><strong>', cat_name, ':</strong> ', freq, ' observasi (', round(prop*100, 1), '%)</p>',
                             '</div>')
      }
      
      # Add interpretation
      max_freq <- max(freq_table)
      min_freq <- min(freq_table)
      dominant_cat <- names(freq_table)[which.max(freq_table)]
      
      stats_html <- paste0(stats_html, 
                           '<div style="margin-top: 20px; padding: 15px; background: rgba(94, 79, 162, 0.1); border-radius: 10px; border-left: 4px solid #5e4fa2;">',
                           '<h5 style="color: #3b0f70; margin-bottom: 10px;">💡 Interpretasi:</h5>',
                           '<p style="color: #5e4fa2; margin: 5px 0;">🏆 <strong>Kategori Dominan:</strong> ', dominant_cat, ' (', round(max_freq/total*100, 1), '%)</p>',
                           '<p style="color: #5e4fa2; margin: 5px 0;">📊 <strong>Distribusi:</strong> ', 
                           ifelse((max_freq - min_freq)/total < 0.2, "Relatif merata", "Tidak merata"), '</p>',
                           '</div>')
      
      stats_html <- paste0(stats_html, '</div></div>')
      HTML(stats_html)
    }
  })
  
  # TRANSFORMASI - HANYA UPDATE SAAT TOMBOL DITEKAN
  # Before transformation plot - hanya update saat tombol ditekan
  trans_before_data <- eventReactive(input$apply_transformation, {
    req(input$trans_var)
    values$data[[input$trans_var]]
  })
  
  output$trans_boxplot_before <- renderPlotly({
    var_data <- trans_before_data()
    
    # Calculate statistics for annotation
    q1 <- quantile(var_data, 0.25, na.rm = TRUE)
    q3 <- quantile(var_data, 0.75, na.rm = TRUE)
    median_val <- median(var_data, na.rm = TRUE)
    outliers <- var_data[var_data < (q1 - 1.5*(q3-q1)) | var_data > (q3 + 1.5*(q3-q1))]
    
    p <- ggplot(data.frame(x = "Original", y = var_data), aes(x = x, y = y)) +
      geom_boxplot(fill = "#c2a5cf", color = "#3b0f70", alpha = 0.8, outlier.color = "#ff6b6b", outlier.size = 2) +
      geom_jitter(width = 0.2, alpha = 0.3, color = "#5e4fa2", size = 1) +
      labs(title = paste("📦 Sebelum Transformasi:", input$trans_var), 
           x = "", 
           y = input$trans_var,
           subtitle = paste("Median:", round(median_val, 3), "| Outliers:", length(outliers))) +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#3b0f70", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#5e4fa2", size = 12),
        axis.title = element_text(color = "#5e4fa2", size = 12, face = "bold"),
        panel.grid.major = element_line(color = "grey90", size = 0.5)
      )
    
    ggplotly(p)
  })
  
  # FIXED transformation logic
  observeEvent(input$apply_transformation, {
    req(input$trans_var, input$trans_method)
    
    tryCatch({
      valid_methods <- c("log", "sqrt", "scale", "minmax", "boxcox")
      if (is.null(input$trans_method) || !(input$trans_method %in% valid_methods)) {
        stop("Metode transformasi tidak valid atau belum dipilih.")
      }
      
      var_data <- values$data[[input$trans_var]]
      
      transformed_data <- switch(input$trans_method,
                                 "log" = {
                                   # Handle zero and negative values properly
                                   min_val <- min(var_data, na.rm = TRUE)
                                   if(min_val <= 0) {
                                     log(var_data - min_val + 1)
                                   } else {
                                     log(var_data)
                                   }
                                 },
                                 "sqrt" = sqrt(abs(var_data)),
                                 "scale" = as.numeric(scale(var_data)),
                                 "minmax" = {
                                   min_val <- min(var_data, na.rm = TRUE)
                                   max_val <- max(var_data, na.rm = TRUE)
                                   (var_data - min_val) / (max_val - min_val)
                                 },
                                 "boxcox" = {
                                   lambda <- ifelse(is.null(input$lambda_boxcox), 0, input$lambda_boxcox)
                                   if(lambda == 0) {
                                     min_val <- min(var_data, na.rm = TRUE)
                                     if(min_val <= 0) {
                                       log(var_data - min_val + 1)
                                     } else {
                                       log(var_data)
                                     }
                                   } else {
                                     min_val <- min(var_data, na.rm = TRUE)
                                     if(min_val <= 0) {
                                       adjusted_data <- var_data - min_val + 1
                                       (adjusted_data^lambda - 1) / lambda
                                     } else {
                                       (var_data^lambda - 1) / lambda
                                     }
                                   }
                                 }
      )
      
      trans_var_name <- paste0(input$trans_var, "_", input$trans_method)
      values$data[[trans_var_name]] <- transformed_data
      values$transformed_vars <- c(values$transformed_vars, trans_var_name)
      values$last_update <- Sys.time()
      
      output$transformation_applied <- reactive({ TRUE })
      outputOptions(output, "transformation_applied", suspendWhenHidden = FALSE)
      
      showNotification("✅ Transformasi berhasil diterapkan!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("❌ Error transformasi:", e$message), type = "error", duration = 5)
    })
  })
  
  # Enhanced after transformation plot
  output$trans_boxplot_after <- renderPlotly({
    req(input$apply_transformation)
    valid_methods <- c("log", "sqrt", "scale", "minmax", "boxcox")
    if (is.null(input$trans_method) || !(input$trans_method %in% valid_methods)) {
      showNotification("❌ Silakan pilih metode transformasi yang valid!", type = "error", duration = 4)
      return()
    }
    
    trans_var_name <- paste0(input$trans_var, "_", input$trans_method)
    
    if(trans_var_name %in% names(values$data)) {
      var_data <- values$data[[trans_var_name]]
      
      q1 <- quantile(var_data, 0.25, na.rm = TRUE)
      q3 <- quantile(var_data, 0.75, na.rm = TRUE)
      median_val <- median(var_data, na.rm = TRUE)
      outliers <- var_data[var_data < (q1 - 1.5*(q3-q1)) | var_data > (q3 + 1.5*(q3-q1))]
      
      p <- ggplot(data.frame(x = "Transformed", y = var_data), aes(x = x, y = y)) +
        geom_boxplot(fill = "#9970ab", color = "#3b0f70", alpha = 0.8, outlier.color = "#ff6b6b", outlier.size = 2) +
        geom_jitter(width = 0.2, alpha = 0.3, color = "#5e4fa2", size = 1) +
        labs(title = paste("✨ Setelah Transformasi:", trans_var_name), 
             x = "", 
             y = trans_var_name,
             subtitle = paste("Median:", round(median_val, 3), "| Outliers:", length(outliers))) +
        theme_minimal() +
        theme(
          plot.title = element_text(color = "#3b0f70", size = 16, face = "bold"),
          plot.subtitle = element_text(color = "#5e4fa2", size = 12),
          axis.title = element_text(color = "#5e4fa2", size = 12, face = "bold"),
          panel.grid.major = element_line(color = "grey90", size = 0.5)
        )
      
      ggplotly(p)
    }
  })
  
  # ENHANCED TRANSFORMATION STATISTICS
  output$trans_statistics <- renderUI({
    req(input$apply_transformation, input$trans_method)
    trans_var_name <- paste0(input$trans_var, "_", input$trans_method)
    
    if(trans_var_name %in% names(values$data)) {
      original_data <- values$data[[input$trans_var]]
      transformed_data <- values$data[[trans_var_name]]
      
      # Calculate comprehensive statistics
      stats_orig <- c(
        round(mean(original_data, na.rm = TRUE), 4),
        round(sd(original_data, na.rm = TRUE), 4),
        round(min(original_data, na.rm = TRUE), 4),
        round(quantile(original_data, 0.25, na.rm = TRUE), 4),
        round(median(original_data, na.rm = TRUE), 4),
        round(quantile(original_data, 0.75, na.rm = TRUE), 4),
        round(max(original_data, na.rm = TRUE), 4),
        round(skewness(original_data, na.rm = TRUE), 4),
        round(kurtosis(original_data, na.rm = TRUE), 4)
      )
      
      stats_trans <- c(
        round(mean(transformed_data, na.rm = TRUE), 4),
        round(sd(transformed_data, na.rm = TRUE), 4),
        round(min(transformed_data, na.rm = TRUE), 4),
        round(quantile(transformed_data, 0.25, na.rm = TRUE), 4),
        round(median(transformed_data, na.rm = TRUE), 4),
        round(quantile(transformed_data, 0.75, na.rm = TRUE), 4),
        round(max(transformed_data, na.rm = TRUE), 4),
        round(skewness(transformed_data, na.rm = TRUE), 4),
        round(kurtosis(transformed_data, na.rm = TRUE), 4)
      )
      
      stat_names <- c("Mean", "Std Dev", "Min", "Q1", "Median", "Q3", "Max", "Skewness", "Kurtosis")
      stat_icons <- c("📊", "📏", "⬇️", "📉", "🎯", "📈", "⬆️", "↗️", "📐")
      
      HTML(paste0(
        '<div class="stats-card">',
        '<div class="stats-header">📊 Perbandingan Statistik Komprehensif</div>',
        '<div class="stats-content">',
        '<table class="enhanced-transposed-table">',
        '<tr><th>📋 Statistik</th><th>📦 Sebelum</th><th>✨ Setelah</th><th>🔄 Perubahan</th></tr>',
        paste0(mapply(function(icon, name, orig, trans) {
          change <- round(((trans - orig) / orig) * 100, 1)
          change_color <- ifelse(abs(change) > 10, "#ff6b6b", ifelse(abs(change) > 5, "#ffa500", "#28a745"))
          paste0('<tr><td><strong>', icon, ' ', name, '</strong></td><td>', orig, '</td><td>', trans, '</td>',
                 '<td style="color: ', change_color, '; font-weight: bold;">', 
                 ifelse(is.finite(change), paste0(ifelse(change > 0, "+", ""), change, "%"), "N/A"), '</td></tr>')
        }, stat_icons, stat_names, stats_orig, stats_trans), collapse = ""),
        '</table>',
        '<div style="margin-top: 20px; padding: 15px; background: rgba(94, 79, 162, 0.1); border-radius: 10px; border-left: 4px solid #5e4fa2;">',
        '<h5 style="color: #3b0f70; margin-bottom: 10px;">💡 Interpretasi Transformasi:</h5>',
        '<p style="color: #5e4fa2; margin: 5px 0;">',
        
        switch(input$trans_method,
               "log" = "Transformasi logaritma mengurangi skewness dan menstabilkan varians",
               "sqrt" = "Transformasi akar kuadrat mengurangi pengaruh outliers",
               "scale" = "Standardisasi menghasilkan mean=0 dan std=1",
               "minmax" = "Normalisasi menghasilkan range 0-1",
               "boxcox" = "Transformasi Box-Cox mengoptimalkan normalitas"),
        '</p>',
        '<p style="color: #5e4fa2; margin: 5px 0;">',
        '🎯 Skewness: ', ifelse(abs(stats_trans[8]) < abs(stats_orig[8]), "Membaik ✅", "Tidak berubah signifikan ⚠️"),
        '</p>',
        '<p style="color: #5e4fa2; margin: 5px 0;">',
        '📐 Kurtosis: ', ifelse(abs(stats_trans[9] - 3) < abs(stats_orig[9] - 3), "Mendekati normal ✅", "Masih jauh dari normal ⚠️"),
        '</p>',
        '</div>',
        '</div></div>'
      ))
    }
  })
  
  # DESCRIPTIVE STATISTICS - HANYA UPDATE SAAT TOMBOL DITEKAN
  desc_stats_data <- eventReactive(input$refresh_desc, {
    req(input$desc_var)
    values$data[[input$desc_var]]
  })
  
  # Set reactive flag for descriptive stats availability
  observeEvent(input$refresh_desc, {
    output$desc_stats_available <- reactive({ TRUE })
    outputOptions(output, "desc_stats_available", suspendWhenHidden = FALSE)
  })
  
  output$desc_stats_display <- renderUI({
    var_data <- desc_stats_data()
    
    # Calculate comprehensive statistics
    stats <- list(
      "Mean" = round(mean(var_data, na.rm = TRUE), 4),
      "Median" = round(median(var_data, na.rm = TRUE), 4),
      "Mode" = {
        tbl <- table(round(var_data, 2))
        as.numeric(names(tbl)[which.max(tbl)])
      },
      "Std Dev" = round(sd(var_data, na.rm = TRUE), 4),
      "Variance" = round(var(var_data, na.rm = TRUE), 4),
      "Min" = round(min(var_data, na.rm = TRUE), 4),
      "Q1" = round(quantile(var_data, 0.25, na.rm = TRUE), 4),
      "Q3" = round(quantile(var_data, 0.75, na.rm = TRUE), 4),
      "Max" = round(max(var_data, na.rm = TRUE), 4),
      "IQR" = round(IQR(var_data, na.rm = TRUE), 4),
      "Range" = round(max(var_data, na.rm = TRUE) - min(var_data, na.rm = TRUE), 4),
      "Skewness" = round(skewness(var_data, na.rm = TRUE), 4),
      "Kurtosis" = round(kurtosis(var_data, na.rm = TRUE), 4),
      "CV" = round(sd(var_data, na.rm = TRUE) / mean(var_data, na.rm = TRUE) * 100, 2)
    )
    
    stat_icons <- c("📊", "🎯", "🔝", "📏", "📐", "⬇️", "📉", "📈", "⬆️", "📊", "↔️", "↗️", "📐", "📊")
    
    stats_html <- '<div class="enhanced-stats-grid">'
    
    for(i in seq_along(stats)) {
      stat_name <- names(stats)[i]
      stat_value <- stats[[i]]
      stat_icon <- stat_icons[i]
      
      stats_html <- paste0(stats_html, 
                           '<div class="enhanced-stat-item">',
                           '<div class="enhanced-stat-label">', stat_icon, ' ', stat_name, '</div>',
                           '<div class="enhanced-stat-value">', stat_value, '</div>',
                           '</div>'
      )
    }
    
    stats_html <- paste0(stats_html, '</div>')
    
    # Add interpretation
    interpretation <- paste0(
      '<div style="margin-top: 20px; padding: 20px; background: linear-gradient(135deg, #f8f9ff 0%, #e7d4e8 100%); border-radius: 15px; border: 2px solid #5e4fa2;">',
      '<h5 style="color: #3b0f70; margin-bottom: 15px;">💡 Interpretasi Statistik:</h5>',
      '<p style="color: #5e4fa2; margin: 8px 0;">📊 <strong>Distribusi:</strong> ',
      ifelse(abs(stats$Skewness) < 0.5, "Relatif simetris", 
             ifelse(stats$Skewness > 0.5, "Condong ke kanan (positif)", "Condong ke kiri (negatif)")), '</p>',
      '<p style="color: #5e4fa2; margin: 8px 0;">📏 <strong>Variabilitas:</strong> ',
      ifelse(stats$CV < 15, "Rendah", ifelse(stats$CV < 30, "Sedang", "Tinggi")), 
      ' (CV = ', stats$CV, '%)</p>',
      '<p style="color: #5e4fa2; margin: 8px 0;">🎯 <strong>Outliers:</strong> ',
      ifelse(abs(stats$Kurtosis) > 3, "Kemungkinan ada outliers", "Distribusi normal"), '</p>',
      '<p style="color: #5e4fa2; margin: 8px 0;">📈 <strong>Tendensi Sentral:</strong> ',
      ifelse(abs(stats$Mean - stats$Median) / stats$Mean < 0.1, "Mean dan median hampir sama", "Mean dan median berbeda signifikan"), '</p>',
      '</div>'
    )
    
    HTML(paste0(stats_html, interpretation))
  })
  
  # Enhanced histogram with more features - hanya update saat tombol ditekan
  output$desc_histogram <- renderPlotly({
    var_data <- desc_stats_data()
    
    p <- ggplot(data.frame(x = var_data), aes(x = x)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "#5e4fa2", alpha = 0.7, color = "white", size = 0.5) +
      geom_density(color = "#3b0f70", size = 1.5, alpha = 0.8) +
      geom_vline(xintercept = mean(var_data, na.rm = TRUE), color = "#ff6b6b", linetype = "dashed", size = 1) +
      geom_vline(xintercept = median(var_data, na.rm = TRUE), color = "#4ecdc4", linetype = "dashed", size = 1) +
      labs(title = paste("📊 Distribusi", input$desc_var), 
           x = input$desc_var, 
           y = "Density",
           subtitle = paste("Mean (merah) =", round(mean(var_data, na.rm = TRUE), 3), 
                            "| Median (biru) =", round(median(var_data, na.rm = TRUE), 3))) +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#3b0f70", size = 16, face = "bold"),
        axis.title = element_text(color = "#5e4fa2", size = 12, face = "bold"),
        panel.grid.major = element_line(color = "grey90", size = 0.5)
      )
    
    ggplotly(p)
  })
  
  # NEW: Histogram interpretation
  output$histogram_interpretation <- renderUI({
    var_data <- desc_stats_data()
    
    mean_val <- mean(var_data, na.rm = TRUE)
    median_val <- median(var_data, na.rm = TRUE)
    skew_val <- skewness(var_data, na.rm = TRUE)
    
    shape_desc <- ifelse(abs(skew_val) < 0.5, "simetris", 
                         ifelse(skew_val > 0.5, "condong kanan", "condong kiri"))
    
    central_tendency <- ifelse(abs(mean_val - median_val) / mean_val < 0.05, 
                               "Mean dan median hampir sama, menunjukkan distribusi yang relatif simetris",
                               ifelse(mean_val > median_val, 
                                      "Mean > Median, menunjukkan adanya outliers di sisi kanan",
                                      "Mean < Median, menunjukkan adanya outliers di sisi kiri"))
    
    HTML(paste0(
      '<p style="color: #5e4fa2; margin: 5px 0;">📊 <strong>Bentuk Distribusi:</strong> ', shape_desc, '</p>',
      '<p style="color: #5e4fa2; margin: 5px 0;">🎯 <strong>Tendensi Sentral:</strong> ', central_tendency, '</p>',
      '<p style="color: #5e4fa2; margin: 5px 0;">📈 <strong>Kurva Densitas:</strong> Menunjukkan probabilitas relatif setiap nilai</p>'
    ))
  })
  
  # Enhanced boxplot - hanya update saat tombol ditekan
  output$desc_boxplot <- renderPlotly({
    var_data <- desc_stats_data()
    
    # Identify outliers
    q1 <- quantile(var_data, 0.25, na.rm = TRUE)
    q3 <- quantile(var_data, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    outliers <- var_data[var_data < (q1 - 1.5*iqr) | var_data > (q3 + 1.5*iqr)]
    
    p <- ggplot(data.frame(x = "", y = var_data), aes(x = x, y = y)) +
      geom_boxplot(fill = "#c2a5cf", color = "#3b0f70", alpha = 0.8, outlier.color = "#ff6b6b", outlier.size = 2) +
      geom_jitter(width = 0.2, alpha = 0.4, color = "#5e4fa2", size = 1) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "#ffff99", color = "#3b0f70") +
      labs(title = paste("📦 Box Plot", input$desc_var), 
           x = "", 
           y = input$desc_var,
           subtitle = paste("Outliers:", length(outliers), "| IQR:", round(iqr, 3))) +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#3b0f70", size = 16, face = "bold"),
        axis.title = element_text(color = "#5e4fa2", size = 12, face = "bold"),
        panel.grid.major = element_line(color = "grey90", size = 0.5)
      )
    
    ggplotly(p)
  })
  
  # NEW: Boxplot interpretation
  output$boxplot_interpretation <- renderUI({
    var_data <- desc_stats_data()
    
    q1 <- quantile(var_data, 0.25, na.rm = TRUE)
    q3 <- quantile(var_data, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    outliers <- var_data[var_data < (q1 - 1.5*iqr) | var_data > (q3 + 1.5*iqr)]
    median_val <- median(var_data, na.rm = TRUE)
    
    outlier_desc <- ifelse(length(outliers) == 0, "Tidak ada outliers terdeteksi",
                           paste("Terdeteksi", length(outliers), "outliers"))
    
    spread_desc <- ifelse(iqr / median_val < 0.5, "Variabilitas rendah", 
                          ifelse(iqr / median_val < 1, "Variabilitas sedang", "Variabilitas tinggi"))
    
    HTML(paste0(
      '<p style="color: #fd7e14; margin: 5px 0;">📦 <strong>Outliers:</strong> ', outlier_desc, '</p>',
      '<p style="color: #fd7e14; margin: 5px 0;">📊 <strong>Variabilitas:</strong> ', spread_desc, ' (IQR = ', round(iqr, 3), ')</p>',
      '<p style="color: #fd7e14; margin: 5px 0;">🎯 <strong>Titik Kuning:</strong> Menunjukkan nilai rata-rata (mean)</p>',
      '<p style="color: #fd7e14; margin: 5px 0;">📈 <strong>Interpretasi:</strong> Box plot membantu mengidentifikasi distribusi dan outliers dalam data</p>'
    ))
  })
  
  # DESCRIPTIVE CATEGORICAL TAB - HANYA UPDATE SAAT TOMBOL DITEKAN
  desc_cat_data <- eventReactive(input$refresh_desc_cat, {
    req(input$desc_cat_var)
    if(input$desc_cat_var %in% names(values$data)) {
      values$data[[input$desc_cat_var]]
    }
  })
  
  # Set reactive flag for categorical descriptive stats availability
  observeEvent(input$refresh_desc_cat, {
    output$desc_cat_available <- reactive({ TRUE })
    outputOptions(output, "desc_cat_available", suspendWhenHidden = FALSE)
  })
  
  output$desc_freq_table <- renderDT({
    cat_data <- desc_cat_data()
    if(!is.null(cat_data)) {
      freq_table <- table(cat_data, useNA = "ifany")
      prop_table <- prop.table(freq_table)
      
      result <- data.frame(
        Kategori = names(freq_table),
        Frekuensi = as.numeric(freq_table),
        Proporsi = round(as.numeric(prop_table), 4),
        Persentase = paste0(round(as.numeric(prop_table) * 100, 2), "%")
      )
      
      datatable(result, options = list(pageLength = 10, dom = 't',
                                       initComplete = JS(
                                         "function(settings, json) {",
                                         "$(this.api().table().header()).css({'background-color': '#5e4fa2', 'color': '#fff'});",
                                         "}")), rownames = FALSE)
    }
  })
  
  # NEW: Frequency table interpretation
  output$freq_table_interpretation <- renderUI({
    cat_data <- desc_cat_data()
    if(!is.null(cat_data)) {
      freq_table <- table(cat_data, useNA = "ifany")
      total <- sum(freq_table)
      n_categories <- length(freq_table)
      
      max_freq <- max(freq_table)
      dominant_cat <- names(freq_table)[which.max(freq_table)]
      
      evenness <- sd(as.numeric(freq_table)) / mean(as.numeric(freq_table))
      distribution_desc <- ifelse(evenness < 0.3, "sangat merata", 
                                  ifelse(evenness < 0.6, "cukup merata", "tidak merata"))
      
      HTML(paste0(
        '<p style="color: #4a90e2; margin: 5px 0;">📊 <strong>Total Kategori:</strong> ', n_categories, ' kategori</p>',
        '<p style="color: #4a90e2; margin: 5px 0;">🏆 <strong>Kategori Dominan:</strong> ', dominant_cat, ' (', round(max_freq/total*100, 1), '%)</p>',
        '<p style="color: #4a90e2; margin: 5px 0;">📈 <strong>Distribusi:</strong> ', distribution_desc, '</p>',
        '<p style="color: #4a90e2; margin: 5px 0;">📋 <strong>Interpretasi:</strong> Tabel menunjukkan frekuensi dan proporsi setiap kategori dalam data</p>'
      ))
    }
  })
  
  # Enhanced bar chart - hanya update saat tombol ditekan
  output$desc_bar_chart <- renderPlotly({
    cat_data <- desc_cat_data()
    if(!is.null(cat_data)) {
      freq_data <- as.data.frame(table(cat_data))
      names(freq_data) <- c("Category", "Frequency")
      freq_data$Percentage <- round(freq_data$Frequency / sum(freq_data$Frequency) * 100, 1)
      
      p <- ggplot(freq_data, aes(x = Category, y = Frequency, fill = Category, 
                                 text = paste("Kategori:", Category, "<br>Frekuensi:", Frequency, "<br>Persentase:", Percentage, "%"))) +
        geom_bar(stat = "identity", alpha = 0.8, color = "white", size = 0.5) +
        scale_fill_manual(values = c("#3b0f70", "#5e4fa2", "#9970ab", "#c2a5cf", "#e7d4e8")[1:nrow(freq_data)]) +
        labs(title = paste("📊 Distribusi", input$desc_cat_var), 
             x = input$desc_cat_var, 
             y = "Frekuensi") +
        theme_minimal() +
        theme(
          plot.title = element_text(color = "#3b0f70", size = 16, face = "bold"),
          axis.title = element_text(color = "#5e4fa2", size = 12, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, color = "#5e4fa2"),
          legend.position = "none"
        )
      
      ggplotly(p, tooltip = "text")
    }
  })
  
  # NEW: Bar chart interpretation
  output$bar_chart_interpretation <- renderUI({
    cat_data <- desc_cat_data()
    if(!is.null(cat_data)) {
      freq_data <- as.data.frame(table(cat_data))
      names(freq_data) <- c("Category", "Frequency")
      
      max_freq <- max(freq_data$Frequency)
      min_freq <- min(freq_data$Frequency)
      range_ratio <- (max_freq - min_freq) / max_freq
      
      pattern_desc <- ifelse(range_ratio < 0.3, "relatif seimbang", 
                             ifelse(range_ratio < 0.7, "cukup bervariasi", "sangat bervariasi"))
      
      HTML(paste0(
        '<p style="color: #3b0f70; margin: 5px 0;">📊 <strong>Pola Distribusi:</strong> ', pattern_desc, '</p>',
        '<p style="color: #3b0f70; margin: 5px 0;">📈 <strong>Rentang Frekuensi:</strong> ', min_freq, ' - ', max_freq, '</p>',
        '<p style="color: #3b0f70; margin: 5px 0;">🎯 <strong>Kegunaan:</strong> Bar chart memudahkan perbandingan frekuensi antar kategori</p>'
      ))
    }
  })
  
  # Enhanced pie chart - hanya update saat tombol ditekan
  output$desc_pie_chart <- renderPlotly({
    cat_data <- desc_cat_data()
    if(!is.null(cat_data)) {
      freq_data <- as.data.frame(table(cat_data))
      names(freq_data) <- c("Category", "Frequency")
      freq_data$Percentage <- round(freq_data$Frequency / sum(freq_data$Frequency) * 100, 1)
      
      plot_ly(freq_data, labels = ~Category, values = ~Frequency, type = 'pie',
              textposition = 'inside', textinfo = 'label+percent',
              marker = list(colors = c("#3b0f70", "#5e4fa2", "#9970ab", "#c2a5cf", "#e7d4e8")[1:nrow(freq_data)],
                            line = list(color = '#FFFFFF', width = 2))) %>%
        layout(title = list(text = paste("🥧 Pie Chart", input$desc_cat_var), 
                            font = list(color = "#3b0f70", size = 16)),
               showlegend = TRUE,
               legend = list(orientation = "v", x = 1, y = 0.5))
    }
  })
  
  # NEW: Pie chart interpretation
  output$pie_chart_interpretation <- renderUI({
    cat_data <- desc_cat_data()
    if(!is.null(cat_data)) {
      freq_data <- as.data.frame(table(cat_data))
      names(freq_data) <- c("Category", "Frequency")
      freq_data$Percentage <- round(freq_data$Frequency / sum(freq_data$Frequency) * 100, 1)
      
      dominant_pct <- max(freq_data$Percentage)
      dominant_cat <- freq_data$Category[which.max(freq_data$Percentage)]
      
      dominance_desc <- ifelse(dominant_pct > 50, "sangat dominan", 
                               ifelse(dominant_pct > 30, "cukup dominan", "tidak ada yang dominan"))
      
      HTML(paste0(
        '<p style="color: #fd7e14; margin: 5px 0;">🥧 <strong>Proporsi Terbesar:</strong> ', dominant_cat, ' (', dominant_pct, '%)</p>',
        '<p style="color: #fd7e14; margin: 5px 0;">📊 <strong>Dominasi:</strong> ', dominance_desc, '</p>',
        '<p style="color: #fd7e14; margin: 5px 0;">🎯 <strong>Kegunaan:</strong> Pie chart menunjukkan proporsi relatif setiap kategori terhadap keseluruhan</p>'
      ))
    }
  })
  
  # MAP TAB FUNCTIONALITY
  output$choropleth_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 118, lat = -2, zoom = 5)
  })
  
  vars_selected <- reactive({
    req(input$map_vars)
    if (length(input$map_vars) > 2) {
      showNotification("Pilih maksimal 2 variabel saja!", type = "error")
      return(input$map_vars[1:2])
    }
    input$map_vars
  })
  
  filtered_map_data <- eventReactive(input$update_choropleth, {
    vars <- vars_selected()
    var1 <- vars[1]
    var2 <- ifelse(length(vars) == 2, vars[2], "")
    
    map_data <- peta_lengkap
    
    if (var2 != "") {
      map_data$bivar_class <- get_bivariate_class(map_data[[var1]], map_data[[var2]])
      map_data$fill_color <- bivariate_colors[map_data$bivar_class]
    } else {
      pal <- colorNumeric("viridis", domain = map_data[[var1]], na.color = "#cccccc")
      map_data$fill_color <- pal(map_data[[var1]])
    }
    list(data = map_data, var1 = var1, var2 = var2)
  })
  
  # Set reactive flag for map update
  observeEvent(input$update_choropleth, {
    output$map_updated <- reactive({ TRUE })
    outputOptions(output, "map_updated", suspendWhenHidden = FALSE)
  })
  
  # Update map when button is clicked
  observeEvent(input$update_choropleth, {
    req(input$map_vars)
    
    if (length(input$map_vars) == 1) {
      if (length(input$map_vars) != 1 && length(input$map_vars) != 2) {
        showNotification("❗ Harap pilih 1 atau 2 variabel saja.", type = "warning", duration = 4)
        return()
      }
      
      var_name <- input$map_vars[1]
      map_res <- filtered_map_data()
      map_data <- map_res$data
      var1 <- map_res$var1
      
      # Buat label HTML
      map_data$label_html <- paste0(
        "<strong>", map_data$nmkab, "</strong><br>",
        var1, ": ", round(map_data[[var1]], 3)
      )
      
      if (!is.null(peta_lengkap) && var_name %in% names(peta_lengkap)) {
        var_data <- peta_lengkap[[var_name]]
        pal <- colorNumeric("viridis", var_data, na.color = "transparent")
        
        leafletProxy("choropleth_map") %>%
          clearShapes() %>%
          addPolygons(data = map_data,
                      fillColor = ~pal(map_data[[var1]]),
                      weight = 1,
                      opacity = 1,
                      color = "white",
                      dashArray = "3",
                      fillOpacity = 0.9,
                      label = lapply(map_data$label_html, HTML),
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "5px 8px"),
                        direction = "auto",
                        textsize = "13px",
                        opacity = 0.9
                      )) %>%
          addLegend(pal = pal, values = map_data[[var1]], opacity = 0.7, title = var1,
                    position = "bottomright")
      }
    } else {
      # Bivariate map
      map_res <- filtered_map_data()
      map_data <- map_res$data
      var1 <- map_res$var1
      var2 <- map_res$var2
      
      # Buat kolom label popup HTML
      map_data$label_html <- paste0(
        "<strong>", map_data$nmkab, "</strong><br>",
        var1, ": ", round(map_data[[var1]], 3), "<br>",
        var2, ": ", round(map_data[[var2]], 3)
      )
      
      leafletProxy("choropleth_map") %>%
        clearShapes() %>%
        addPolygons(data = map_data,
                    fillColor = ~fill_color,
                    weight = 1,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.9,
                    label = lapply(map_data$label_html, HTML),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "5px 8px"),
                      direction = "auto",
                      textsize = "13px",
                      opacity = 0.9
                    ))
    }
  })
  
  # Legend box for bivariate map
  output$legend_box <- renderUI({
    req(input$update_choropleth)
    
    res <- filtered_map_data()
    var1 <- res$var1
    var2 <- res$var2
    map_data <- res$data
    
    if (var2 == "" || is.null(var2)) {
      vals <- map_data[[var1]]
      vals <- vals[!is.na(vals)]
      rng <- range(vals, na.rm = TRUE)
      pal <- colorNumeric("viridis", domain = rng)
      breaks <- seq(rng[1], rng[2], length.out = 5)
      colors <- pal(breaks)
      
      return(
        box(
          title = paste("Legend:", var1), status = "warning", solidHeader = TRUE, width = 12,
          tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 5px;",
            lapply(seq_along(colors), function(i) {
              tags$div(
                style = sprintf("background:%s; width: 18%%; height: 25px; border-radius: 4px; border: 1px solid #bbb;", colors[i]),
                title = round(breaks[i], 3)
              )
            })
          ),
          tags$div(
            style = "display: flex; justify-content: space-between; font-size: 11px; color: #555;",
            lapply(round(breaks, 2), function(x) tags$span(as.character(x)))
          )
        )
      )
    }
    
    cell_size <- 30
    bivariate_colors <- c(
      "1-1" = "#e8e8e8", "1-2" = "#b5c0da", "1-3" = "#6c83b5",
      "2-1" = "#b8d6be", "2-2" = "#90b2b3", "2-3" = "#567994",
      "3-1" = "#73ae80", "3-2" = "#5a9178", "3-3" = "#2a5a5b"
    )
    
    box(
      title = "Bivariate Legend", status = "warning", solidHeader = TRUE, width = 12,
      style = "font-family: Arial, sans-serif;",
      tags$div(
        style = "font-weight: bold; text-align: center; margin-bottom: 10px;",
        paste0(var1, " (→) vs ", var2, " (↑)")
      ),
      tags$div(
        style = "position: relative; width: 120px; height: 150px; margin: 0 auto;",
        tags$table(
          style = "border-collapse: collapse; margin: 0 auto;",
          lapply(3:1, function(i) {
            tags$tr(
              lapply(1:3, function(j) {
                col <- bivariate_colors[paste0(j, "-", i)]
                tags$td(
                  style = paste0(
                    "background:", col, "; width:", cell_size, "px; height:", cell_size, 
                    "px; border: 1px solid #bbb;"
                  )
                )
              })
            )
          })
        )
      )
    )
  })
  
  # NEW: Map interpretation
  output$map_interpretation <- renderUI({
    req(input$update_choropleth)
    
    res <- filtered_map_data()
    var1 <- res$var1
    var2 <- res$var2
    map_data <- res$data
    
    if (var2 == "" || is.null(var2)) {
      # Univariate map interpretation
      var_data <- map_data[[var1]]
      var_data <- var_data[!is.na(var_data)]
      
      high_regions <- sum(var_data > quantile(var_data, 0.75, na.rm = TRUE), na.rm = TRUE)
      low_regions <- sum(var_data < quantile(var_data, 0.25, na.rm = TRUE), na.rm = TRUE)
      
      HTML(paste0(
        '<p style="color: #4a90e2; margin: 5px 0;">🗺️ <strong>Peta Univariat:</strong> Menampilkan distribusi spasial ', var1, '</p>',
        '<p style="color: #4a90e2; margin: 5px 0;">📊 <strong>Wilayah Tinggi:</strong> ', high_regions, ' wilayah (kuartil atas)</p>',
        '<p style="color: #4a90e2; margin: 5px 0;">📉 <strong>Wilayah Rendah:</strong> ', low_regions, ' wilayah (kuartil bawah)</p>',
        '<p style="color: #4a90e2; margin: 5px 0;">🎯 <strong>Interpretasi:</strong> Warna gelap menunjukkan nilai tinggi, warna terang menunjukkan nilai rendah</p>'
      ))
    } else {
      # Bivariate map interpretation
      HTML(paste0(
        '<p style="color: #4a90e2; margin: 5px 0;">🗺️ <strong>Peta Bivariat:</strong> Menampilkan hubungan spasial antara ', var1, ' dan ', var2, '</p>',
        '<p style="color: #4a90e2; margin: 5px 0;">📊 <strong>Sumbu Horizontal:</strong> ', var1, ' (kiri = rendah, kanan = tinggi)</p>',
        '<p style="color: #4a90e2; margin: 5px 0;">📈 <strong>Sumbu Vertikal:</strong> ', var2, ' (bawah = rendah, atas = tinggi)</p>',
        '<p style="color: #4a90e2; margin: 5px 0;">🎯 <strong>Interpretasi:</strong> Warna menunjukkan kombinasi kedua variabel - hijau gelap = tinggi-tinggi, abu-abu = rendah-rendah</p>'
      ))
    }
  })
  
  # NEW: NORMALITY TEST FUNCTIONS
  # Update category choices for normality test
  observe({
    if(input$norm_cat_var != "" && input$norm_cat_var %in% names(values$data)) {
      categories <- unique(values$data[[input$norm_cat_var]])
      updateSelectInput(session, "norm_category", choices = categories)
    }
  })
  
  # Run normality test
  observeEvent(input$run_norm_test, {
    req(input$norm_var)
    
    tryCatch({
      if(input$norm_cat_var == "" || is.null(input$norm_category)) {
        # Test on all data
        test_data <- values$data[[input$norm_var]]
        group_name <- "Semua Data"
      } else {
        # Test on specific category
        test_data <- values$data[[input$norm_var]][values$data[[input$norm_cat_var]] == input$norm_category]
        group_name <- paste(input$norm_cat_var, "=", input$norm_category)
      }
      
      test_data <- test_data[!is.na(test_data)]
      
      if(length(test_data) < 3) {
        stop("Data terlalu sedikit untuk uji normalitas (minimum 3 observasi)")
      }
      
      # Perform normality test
      test_result <- switch(input$norm_test_method,
                            "shapiro" = shapiro.test(test_data),
                            "ad" = ad.test(test_data),
                            "ks" = ks.test(test_data, "pnorm", mean(test_data), sd(test_data)),
                            "lillie" = lillie.test(test_data)
      )
      
      output$norm_test_done <- reactive({ TRUE })
      outputOptions(output, "norm_test_done", suspendWhenHidden = FALSE)
      
      # Store test data for plots
      values$norm_test_data <- test_data
      values$norm_group_name <- group_name
      
      # Display results
      test_name <- switch(input$norm_test_method,
                          "shapiro" = "Shapiro-Wilk",
                          "ad" = "Anderson-Darling",
                          "ks" = "Kolmogorov-Smirnov",
                          "lillie" = "Lilliefors")
      
      stat_name <- switch(input$norm_test_method,
                          "shapiro" = "W",
                          "ad" = "A",
                          "ks" = "D",
                          "lillie" = "D")
      
      output$norm_test_results <- renderUI({
        HTML(paste0(
          '<div class="hypothesis-card">',
          '<div class="hypothesis-header">🧪 Uji Normalitas - ', test_name, '</div>',
          '<div class="hypothesis-content">',
          '<p><strong>Grup:</strong> ', group_name, '</p>',
          '<p><strong>n:</strong> ', length(test_data), '</p>',
          '<p><strong>H₀:</strong> Data berdistribusi normal</p>',
          '<p><strong>H₁:</strong> Data tidak berdistribusi normal</p>',
          '<p><strong>', stat_name, '-statistik:</strong> ', round(test_result$statistic, 4), '</p>',
          '<p><strong>p-value:</strong> ', round(test_result$p.value, 6), '</p>',
          '<div class="', ifelse(test_result$p.value < input$alpha_norm, 'decision-reject', 'decision-accept'), '">',
          '<strong>Keputusan:</strong> ', 
          ifelse(test_result$p.value < input$alpha_norm, 
                 paste("Tolak H₀ - Data TIDAK berdistribusi normal (p-value =", round(test_result$p.value, 6), "< α =", input$alpha_norm, ")"),
                 paste("Terima H₀ - Data berdistribusi normal (p-value =", round(test_result$p.value, 6), "≥ α =", input$alpha_norm, ")")),
          '</div>',
          '<div style="margin-top: 15px; padding: 15px; background: rgba(94, 79, 162, 0.1); border-radius: 10px; border-left: 4px solid #5e4fa2;">',
          '<h5 style="color: #3b0f70; margin-bottom: 10px;">💡 Interpretasi:</h5>',
          '<p style="color: #5e4fa2; margin: 5px 0;">',
          switch(input$norm_test_method,
                 "shapiro" = "Shapiro-Wilk cocok untuk sampel kecil (n < 50) dan sensitif terhadap outliers",
                 "ad" = "Anderson-Darling lebih sensitif pada ekor distribusi dibanding Shapiro-Wilk",
                 "ks" = "Kolmogorov-Smirnov cocok untuk sampel besar tapi kurang sensitif",
                 "lillie" = "Lilliefors adalah modifikasi KS yang tidak memerlukan parameter populasi"),
          '</p>',
          '<p style="color: #5e4fa2; margin: 5px 0;">🎯 <strong>Rekomendasi:</strong> ',
          ifelse(test_result$p.value < input$alpha_norm,
                 "Pertimbangkan transformasi data atau gunakan uji non-parametrik",
                 "Data dapat digunakan untuk uji parametrik"),
          '</p>',
          '</div>',
          '</div></div>'
        ))
      })
      
    }, error = function(e) {
      output$norm_test_results <- renderUI({
        HTML(paste0('<div class="hypothesis-card"><div class="hypothesis-header">❌ Error</div>',
                    '<div class="hypothesis-content"><p>Error dalam uji normalitas: ', e$message, '</p></div></div>'))
      })
    })
  })
  
  # Q-Q Plot for normality
  output$norm_qq_plot <- renderPlotly({
    req(values$norm_test_data)
    
    test_data <- values$norm_test_data
    
    qq_data <- data.frame(
      theoretical = qqnorm(test_data, plot.it = FALSE)$x,
      sample = qqnorm(test_data, plot.it = FALSE)$y
    )
    
    p <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
      geom_point(color = "#5e4fa2", alpha = 0.6, size = 2) +
      geom_abline(slope = 1, intercept = 0, color = "#3b0f70", linetype = "dashed", size = 1) +
      labs(title = paste("📊 Q-Q Plot -", values$norm_group_name), 
           x = "Theoretical Quantiles", 
           y = "Sample Quantiles") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#3b0f70", size = 16, face = "bold"),
        axis.title = element_text(color = "#5e4fa2", size = 12, face = "bold")
      )
    
    ggplotly(p)
  })
  
  # NEW: Q-Q Plot interpretation
  output$qq_plot_interpretation <- renderUI({
    req(values$norm_test_data)
    
    test_data <- values$norm_test_data
    qq_data <- qqnorm(test_data, plot.it = FALSE)
    
    # Calculate correlation between theoretical and sample quantiles
    qq_cor <- cor(qq_data$x, qq_data$y)
    
    linearity_desc <- ifelse(qq_cor > 0.99, "sangat linear (mendekati normal)", 
                             ifelse(qq_cor > 0.95, "cukup linear (mendekati normal)", 
                                    "tidak linear (tidak normal)"))
    
    HTML(paste0(
      '<p style="color: #fd7e14; margin: 5px 0;">📊 <strong>Korelasi Q-Q:</strong> ', round(qq_cor, 4), '</p>',
      '<p style="color: #fd7e14; margin: 5px 0;">📈 <strong>Linearitas:</strong> ', linearity_desc, '</p>',
      '<p style="color: #fd7e14; margin: 5px 0;">🎯 <strong>Interpretasi:</strong> Titik yang mengikuti garis diagonal menunjukkan distribusi normal</p>',
      '<p style="color: #fd7e14; margin: 5px 0;">⚠️ <strong>Perhatikan:</strong> Penyimpangan di ujung-ujung menunjukkan masalah pada ekor distribusi</p>'
    ))
  })
  
  # Histogram with normal curve for normality test
  output$norm_histogram <- renderPlotly({
    req(values$norm_test_data)
    
    test_data <- values$norm_test_data
    mean_val <- mean(test_data)
    sd_val <- sd(test_data)
    
    # Create normal curve data
    x_seq <- seq(min(test_data), max(test_data), length.out = 100)
    normal_curve <- dnorm(x_seq, mean_val, sd_val)
    
    p <- ggplot(data.frame(x = test_data), aes(x = x)) +
      geom_histogram(aes(y = ..density..), bins = 20, fill = "#5e4fa2", alpha = 0.7, color = "white") +
      geom_line(data = data.frame(x = x_seq, y = normal_curve), aes(x = x, y = y), 
                color = "#ff6b6b", size = 2) +
      labs(title = paste("📈 Histogram vs Normal -", values$norm_group_name), 
           x = input$norm_var, 
           y = "Density") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#3b0f70", size = 16, face = "bold"),
        axis.title = element_text(color = "#5e4fa2", size = 12, face = "bold")
      )
    
    ggplotly(p)
  })
  
  # NEW: Normal histogram interpretation
  output$norm_histogram_interpretation <- renderUI({
    req(values$norm_test_data)
    
    test_data <- values$norm_test_data
    skew_val <- skewness(test_data)
    kurt_val <- kurtosis(test_data)
    
    skew_desc <- ifelse(abs(skew_val) < 0.5, "simetris", 
                        ifelse(skew_val > 0.5, "condong kanan", "condong kiri"))
    
    kurt_desc <- ifelse(abs(kurt_val - 3) < 0.5, "normal (mesokurtik)", 
                        ifelse(kurt_val > 3, "runcing (leptokurtik)", "datar (platykurtik)"))
    
    HTML(paste0(
      '<p style="color: #28a745; margin: 5px 0;">📊 <strong>Skewness:</strong> ', round(skew_val, 3), ' (', skew_desc, ')</p>',
      '<p style="color: #28a745; margin: 5px 0;">📐 <strong>Kurtosis:</strong> ', round(kurt_val, 3), ' (', kurt_desc, ')</p>',
      '<p style="color: #28a745; margin: 5px 0;">📈 <strong>Kurva Merah:</strong> Distribusi normal teoretis dengan mean dan SD yang sama</p>',
      '<p style="color: #28a745; margin: 5px 0;">🎯 <strong>Interpretasi:</strong> Semakin dekat histogram dengan kurva merah, semakin normal distribusinya</p>'
    ))
  })
  
  # NEW: HOMOGENEITY TEST FUNCTIONS
  # Run homogeneity test
  observeEvent(input$run_homog_test, {
    req(input$homog_var, input$homog_cat_var)
    
    tryCatch({
      numeric_data <- values$data[[input$homog_var]]
      group_data <- values$data[[input$homog_cat_var]]
      
      # Remove missing values
      complete_cases <- !is.na(numeric_data) & !is.na(group_data)
      numeric_data <- numeric_data[complete_cases]
      group_data <- group_data[complete_cases]
      
      if(length(unique(group_data)) < 2) {
        stop("Perlu minimal 2 grup untuk uji homogenitas")
      }
      
      # Perform homogeneity test
      test_result <- switch(input$homog_test_method,
                            "levene" = leveneTest(numeric_data, group_data),
                            "bartlett" = bartlett.test(numeric_data, group_data),
                            "fligner" = fligner.test(numeric_data, group_data)
      )
      
      output$homog_test_done <- reactive({ TRUE })
      outputOptions(output, "homog_test_done", suspendWhenHidden = FALSE)
      
      # Store data for plots
      values$homog_numeric_data <- numeric_data
      values$homog_group_data <- group_data
      
      # Display results
      test_name <- switch(input$homog_test_method,
                          "levene" = "Levene Test",
                          "bartlett" = "Bartlett Test",
                          "fligner" = "Fligner-Killeen Test")
      
      stat_name <- switch(input$homog_test_method,
                          "levene" = "F",
                          "bartlett" = "K-squared",
                          "fligner" = "med χ²")
      
      stat_value <- if(input$homog_test_method == "levene") {
        test_result$`F value`[1]
      } else {
        test_result$statistic
      }
      
      p_value <- if(input$homog_test_method == "levene") {
        test_result$`Pr(>F)`[1]
      } else {
        test_result$p.value
      }
      
      output$homog_test_results <- renderUI({
        HTML(paste0(
          '<div class="hypothesis-card">',
          '<div class="hypothesis-header">🧪 Uji Homogenitas - ', test_name, '</div>',
          '<div class="hypothesis-content">',
          '<p><strong>Variabel:</strong> ', input$homog_var, ' berdasarkan ', input$homog_cat_var, '</p>',
          '<p><strong>Jumlah Grup:</strong> ', length(unique(group_data)), '</p>',
          '<p><strong>Total n:</strong> ', length(numeric_data), '</p>',
          '<p><strong>H₀:</strong> Varians semua grup sama (homogen)</p>',
          '<p><strong>H₁:</strong> Minimal ada satu varians grup yang berbeda</p>',
          '<p><strong>', stat_name, '-statistik:</strong> ', round(stat_value, 4), '</p>',
          '<p><strong>p-value:</strong> ', round(p_value, 6), '</p>',
          '<div class="', ifelse(p_value < input$alpha_homog, 'decision-reject', 'decision-accept'), '">',
          '<strong>Keputusan:</strong> ', 
          ifelse(p_value < input$alpha_homog, 
                 paste("Tolak H₀ - Varians TIDAK homogen (p-value =", round(p_value, 6), "< α =", input$alpha_homog, ")"),
                 paste("Terima H₀ - Varians homogen (p-value =", round(p_value, 6), "≥ α =", input$alpha_homog, ")")),
          '</div>',
          '<div style="margin-top: 15px; padding: 15px; background: rgba(94, 79, 162, 0.1); border-radius: 10px; border-left: 4px solid #5e4fa2;">',
          '<h5 style="color: #3b0f70; margin-bottom: 10px;">💡 Interpretasi:</h5>',
          '<p style="color: #5e4fa2; margin: 5px 0;">',
          switch(input$homog_test_method,
                 "levene" = "Levene test robust terhadap non-normalitas dan cocok untuk berbagai distribusi",
                 "bartlett" = "Bartlett test sensitif terhadap non-normalitas tapi powerful untuk data normal",
                 "fligner" = "Fligner-Killeen test non-parametrik dan robust terhadap outliers"),
          '</p>',
          '<p style="color: #5e4fa2; margin: 5px 0;">🎯 <strong>Implikasi:</strong> ',
          ifelse(p_value < input$alpha_homog,
                 "Gunakan uji yang tidak mengasumsikan varians sama (seperti Welch t-test)",
                 "Dapat menggunakan uji yang mengasumsikan varians sama (seperti pooled t-test)"),
          '</p>',
          '</div>',
          '</div></div>'
        ))
      })
      
    }, error = function(e) {
      output$homog_test_results <- renderUI({
        HTML(paste0('<div class="hypothesis-card"><div class="hypothesis-header">❌ Error</div>',
                    '<div class="hypothesis-content"><p>Error dalam uji homogenitas: ', e$message, '</p></div></div>'))
      })
    })
  })
  
  # Box plot for homogeneity test
  output$homog_boxplot <- renderPlotly({
    req(values$homog_numeric_data, values$homog_group_data)
    
    plot_data <- data.frame(
      value = values$homog_numeric_data,
      group = values$homog_group_data
    )
    
    p <- ggplot(plot_data, aes(x = group, y = value, fill = group)) +
      geom_boxplot(alpha = 0.8, outlier.color = "#ff6b6b", outlier.size = 2) +
      geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
      scale_fill_manual(values = c("#3b0f70", "#5e4fa2", "#9970ab", "#c2a5cf", "#e7d4e8")[1:length(unique(plot_data$group))]) +
      labs(title = paste("📦 Box Plot per Grup -", input$homog_var), 
           x = input$homog_cat_var, 
           y = input$homog_var) +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#3b0f70", size = 16, face = "bold"),
        axis.title = element_text(color = "#5e4fa2", size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
    
    ggplotly(p)
  })
  
  # NEW: Homogeneity boxplot interpretation
  output$homog_boxplot_interpretation <- renderUI({
    req(values$homog_numeric_data, values$homog_group_data)
    
    plot_data <- data.frame(
      value = values$homog_numeric_data,
      group = values$homog_group_data
    )
    
    # Calculate IQR for each group
    group_stats <- plot_data %>%
      group_by(group) %>%
      summarise(
        iqr = IQR(value, na.rm = TRUE),
        median = median(value, na.rm = TRUE),
        .groups = 'drop'
      )
    
    max_iqr <- max(group_stats$iqr)
    min_iqr <- min(group_stats$iqr)
    iqr_ratio <- max_iqr / min_iqr
    
    homogeneity_desc <- ifelse(iqr_ratio < 2, "relatif homogen", 
                               ifelse(iqr_ratio < 4, "cukup heterogen", "sangat heterogen"))
    
    HTML(paste0(
      '<p style="color: #fd7e14; margin: 5px 0;">📦 <strong>Rasio IQR:</strong> ', round(iqr_ratio, 2), ' (', homogeneity_desc, ')</p>',
      '<p style="color: #fd7e14; margin: 5px 0;">📊 <strong>IQR Terbesar:</strong> ', round(max_iqr, 3), '</p>',
      '<p style="color: #fd7e14; margin: 5px 0;">📉 <strong>IQR Terkecil:</strong> ', round(min_iqr, 3), '</p>',
      '<p style="color: #fd7e14; margin: 5px 0;">🎯 <strong>Interpretasi:</strong> Box dengan ukuran serupa menunjukkan varians yang homogen</p>'
    ))
  })
  
  # Descriptive statistics for homogeneity test
  output$homog_descriptive_stats <- renderUI({
    req(values$homog_numeric_data, values$homog_group_data)
    
    plot_data <- data.frame(
      value = values$homog_numeric_data,
      group = values$homog_group_data
    )
    
    # Calculate detailed statistics for each group
    group_stats <- plot_data %>%
      group_by(group) %>%
      summarise(
        n = n(),
        mean = round(mean(value, na.rm = TRUE), 4),
        sd = round(sd(value, na.rm = TRUE), 4),
        var = round(var(value, na.rm = TRUE), 4),
        cv = round(sd(value, na.rm = TRUE) / mean(value, na.rm = TRUE) * 100, 2),
        .groups = 'drop'
      )
    
    stats_html <- '<table class="enhanced-transposed-table">'
    stats_html <- paste0(stats_html, '<tr><th>Grup</th><th>n</th><th>Mean</th><th>SD</th><th>Variance</th><th>CV (%)</th></tr>')
    
    for(i in 1:nrow(group_stats)) {
      stats_html <- paste0(stats_html, '<tr>',
                           '<td><strong>', group_stats$group[i], '</strong></td>',
                           '<td>', group_stats$n[i], '</td>',
                           '<td>', group_stats$mean[i], '</td>',
                           '<td>', group_stats$sd[i], '</td>',
                           '<td>', group_stats$var[i], '</td>',
                           '<td>', group_stats$cv[i], '</td>',
                           '</tr>')
    }
    stats_html <- paste0(stats_html, '</table>')
    
    HTML(stats_html)
  })
  
  # NEW: Homogeneity statistics interpretation
  output$homog_stats_interpretation <- renderUI({
    req(values$homog_numeric_data, values$homog_group_data)
    
    plot_data <- data.frame(
      value = values$homog_numeric_data,
      group = values$homog_group_data
    )
    
    group_stats <- plot_data %>%
      group_by(group) %>%
      summarise(
        var = var(value, na.rm = TRUE),
        cv = sd(value, na.rm = TRUE) / mean(value, na.rm = TRUE) * 100,
        .groups = 'drop'
      )
    
    max_var <- max(group_stats$var)
    min_var <- min(group_stats$var)
    var_ratio <- max_var / min_var
    
    max_cv <- max(group_stats$cv)
    min_cv <- min(group_stats$cv)
    
    HTML(paste0(
      '<p style="color: #28a745; margin: 5px 0;">📊 <strong>Rasio Varians:</strong> ', round(var_ratio, 2), '</p>',
      '<p style="color: #28a745; margin: 5px 0;">📈 <strong>CV Range:</strong> ', round(min_cv, 1), '% - ', round(max_cv, 1), '%</p>',
      '<p style="color: #28a745; margin: 5px 0;">🎯 <strong>Rule of Thumb:</strong> Rasio varians < 4 menunjukkan homogenitas yang dapat diterima</p>',
      '<p style="color: #28a745; margin: 5px 0;">💡 <strong>Interpretasi:</strong> CV yang serupa antar grup menunjukkan variabilitas relatif yang konsisten</p>'
    ))
  })
  
  # HYPOTHESIS TESTING - MEAN TESTS
  # Update category choices for mean test
  observe({
    if(input$mean_cat_var != "" && input$mean_cat_var %in% names(values$data)) {
      categories <- unique(values$data[[input$mean_cat_var]])
      updateSelectInput(session, "mean_categories", choices = categories)
    }
  })
  
  # Run mean test
  observe({
    if (input$mean_cat_var != "" && input$mean_cat_var %in% names(values$data)) {
      categories <- unique(values$data[[input$mean_cat_var]])
      updateSelectInput(session, "mean_categories", choices = categories)
    }
  })
  # Sebelum renderUI
  
  # Preview hypothesis
  output$mean_hypothesis_preview <- renderUI({
    req(input$mean_alternative)
    
    alt <- input$mean_alternative
    alt_sym <- ifelse(alt == "two.sided", "≠", ifelse(alt == "greater", ">", "<"))
    cat_var <- input$mean_cat_var
    cat_sel <- input$mean_categories
    mu0 <- input$mu_0_mean
    
    if (cat_var == "" || is.null(cat_sel)) {
      # One-sample (seluruh data)
      HTML(paste0(
        '<div style="font-size: 20px;">',
        '<b>Uji t Satu Sampel</b> &emsp; | &emsp; ',
        '<b>H₀:</b> μ = ', mu0, ' &emsp; ⟶ &emsp; ',
        '<b>H₁:</b> μ ', alt_sym, ' ', mu0,
        '</div>'
      ))
    } else if (length(cat_sel) == 1) {
      # One-sample (satu kategori)
      HTML(paste0(
        '<div style="font-size: 20px;">',
        '<b>Uji t Satu Sampel - Kategori:</b> ', cat_sel[1], ' &emsp; | &emsp; ',
        '<b>H₀:</b> μ = ', mu0, ' &emsp; ⟶ &emsp; ',
        '<b>H₁:</b> μ ', alt_sym, ' ', mu0,
        '</div>'
      ))
    } else if (length(cat_sel) == 2) {
      # Two-sample t-test
      HTML(paste0(
        '<div style="font-size: 20px;">',
        '<b>Uji t Dua Sampel</b> &emsp; | &emsp; ',
        '<b>H₀:</b> μ<sub>', cat_sel[1], '</sub> = μ<sub>', cat_sel[2], '</sub> &emsp; ⟶ &emsp; ',
        '<b>H₁:</b> μ<sub>', cat_sel[1], '</sub> ', alt_sym, ' μ<sub>', cat_sel[2], '</sub>',
        '</div>'
      ))
    } else if (cat_var != "" && cat_var2 != "") {
      # Two-Way ANOVA
      HTML(paste0(
        '<div style="font-size: 20px;">',
        '<b>Two-Way ANOVA</b> &emsp; | &emsp; <b>Faktor:</b> ', cat_var, ' dan ', cat_var2, '<br>',
        if (interaksi) {
          paste0(
            '<b>H₀:</b> Tidak ada pengaruh dari ', cat_var, ', ', cat_var2, ', dan interaksi keduanya.<br>',
            '<b>H₁:</b> Minimal salah satu faktor atau interaksinya berpengaruh terhadap rata-rata.'
          )
        } else {
          paste0(
            '<b>H₀:</b> Tidak ada pengaruh dari ', cat_var, ' dan ', cat_var2, '.<br>',
            '<b>H₁:</b> Minimal salah satu faktor berpengaruh terhadap rata-rata.'
          )
        },
        '</div>'
      ))
      
    } else {
      HTML('<p style="font-size: 20px;"><i>Belum ada konfigurasi hipotesis yang valid.</i></p>')
    }
  })
  
  
  
  # Run mean test
  observeEvent(input$run_mean_test, {
    req(input$mean_numeric_var)
    
    tryCatch({
      numeric_data <- values$data[[input$mean_numeric_var]]
      
      output$mean_test_done <- reactive({ TRUE })
      outputOptions(output, "mean_test_done", suspendWhenHidden = FALSE)
      
      if(input$mean_cat_var == "" || is.null(input$mean_categories)) {
        # One-sample t-test
        test_result <- t.test(numeric_data, mu = input$mu_0_mean, conf.level = 1 - input$alpha_mean)
        
        output$mean_test_results <- renderUI({
          HTML(paste0(
            '<div class="hypothesis-card">',
            '<div class="hypothesis-header">🧪 Uji t Satu Sampel</div>',
            '<div class="hypothesis-content">',
            '<p><strong>H₀:</strong> μ = ', input$mu_0_mean, '</p>',
            '<p><strong>H₁:</strong> μ ≠ ', input$mu_0_mean, '</p>',
            '<p><strong>Statistik t:</strong> ', round(test_result$statistic, 4), '</p>',
            '<p><strong>df:</strong> ', test_result$parameter, '</p>',
            '<p><strong>p-value:</strong> ', round(test_result$p.value, 6), '</p>',
            '<p><strong>Confidence Interval (', (1-input$alpha_mean)*100, '%):</strong> [', 
            round(test_result$conf.int[1], 4), ', ', round(test_result$conf.int[2], 4), ']</p>',
            '<div class="', ifelse(test_result$p.value < input$alpha_mean, 'decision-reject', 'decision-accept'), '">',
            '<strong>Keputusan:</strong> ', 
            ifelse(test_result$p.value < input$alpha_mean, 
                   paste("Tolak H₀ (p-value =", round(test_result$p.value, 6), "< α =", input$alpha_mean, ")"),
                   paste("Terima H₀ (p-value =", round(test_result$p.value, 6), "≥ α =", input$alpha_mean, ")")),
            '</div>',
            '</div></div>'
          ))
        })
        
      } else if(length(input$mean_categories) == 1) {
        # One-sample t-test for specific category
        cat_data <- numeric_data[values$data[[input$mean_cat_var]] == input$mean_categories[1]]
        test_result <- t.test(cat_data, mu = input$mu_0_mean, conf.level = 1 - input$alpha_mean)
        
        output$mean_test_results <- renderUI({
          HTML(paste0(
            '<div class="hypothesis-card">',
            '<div class="hypothesis-header">🧪 Uji t Satu Sampel - Kategori: ', input$mean_categories[1], '</div>',
            '<div class="hypothesis-content">',
            '<p><strong>H₀:</strong> μ = ', input$mu_0_mean, '</p>',
            '<p><strong>H₁:</strong> μ ≠ ', input$mu_0_mean, '</p>',
            '<p><strong>n:</strong> ', length(cat_data), '</p>',
            '<p><strong>Statistik t:</strong> ', round(test_result$statistic, 4), '</p>',
            '<p><strong>df:</strong> ', test_result$parameter, '</p>',
            '<p><strong>p-value:</strong> ', round(test_result$p.value, 6), '</p>',
            '<div class="', ifelse(test_result$p.value < input$alpha_mean, 'decision-reject', 'decision-accept'), '">',
            '<strong>Keputusan:</strong> ', 
            ifelse(test_result$p.value < input$alpha_mean, 
                   paste("Tolak H₀ (p-value =", round(test_result$p.value, 6), "< α =", input$alpha_mean, ")"),
                   paste("Terima H₀ (p-value =", round(test_result$p.value, 6), "≥ α =", input$alpha_mean, ")")),
            '</div>',
            '</div></div>'
          ))
        })
        
      } else if(length(input$mean_categories) == 2) {
        # Two-sample t-test
        group1_data <- numeric_data[values$data[[input$mean_cat_var]] == input$mean_categories[1]]
        group2_data <- numeric_data[values$data[[input$mean_cat_var]] == input$mean_categories[2]]
        test_result <- t.test(group1_data, group2_data, conf.level = 1 - input$alpha_mean)
        
        output$mean_test_results <- renderUI({
          HTML(paste0(
            '<div class="hypothesis-card">',
            '<div class="hypothesis-header">🧪 Uji t Dua Sampel</div>',
            '<div class="hypothesis-content">',
            '<p><strong>Grup 1:</strong> ', input$mean_categories[1], ' (n = ', length(group1_data), ')</p>',
            '<p><strong>Grup 2:</strong> ', input$mean_categories[2], ' (n = ', length(group2_data), ')</p>',
            '<p><strong>H₀:</strong> μ₁ = μ₂</p>',
            '<p><strong>H₁:</strong> μ₁ ≠ μ₂</p>',
            '<p><strong>Statistik t:</strong> ', round(test_result$statistic, 4), '</p>',
            '<p><strong>df:</strong> ', round(test_result$parameter, 2), '</p>',
            '<p><strong>p-value:</strong> ', round(test_result$p.value, 6), '</p>',
            '<div class="', ifelse(test_result$p.value < input$alpha_mean, 'decision-reject', 'decision-accept'), '">',
            '<strong>Keputusan:</strong> ', 
            ifelse(test_result$p.value < input$alpha_mean, 
                   paste("Tolak H₀ (p-value =", round(test_result$p.value, 6), "< α =", input$alpha_mean, ")"),
                   paste("Terima H₀ (p-value =", round(test_result$p.value, 6), "≥ α =", input$alpha_mean, ")")),
            '</div>',
            '</div></div>'
          ))
        })
        
      } else if (input$mean_cat_var != "" && input$mean_cat_var2 != "") {
        cat_var1 <- input$mean_cat_var
        cat_var2 <- input$mean_cat_var2
        interaksi <- input$include_interaction
        
        df <- values$data
        df[[cat_var1]] <- as.factor(df[[cat_var1]])
        df[[cat_var2]] <- as.factor(df[[cat_var2]])
        df[[input$mean_numeric_var]] <- as.numeric(df[[input$mean_numeric_var]])
        
        formula_str <- if (interaksi) {
          paste(input$mean_numeric_var, "~", cat_var1, "*", cat_var2)
        } else {
          paste(input$mean_numeric_var, "~", cat_var1, "+", cat_var2)
        }
        
        model <- aov(as.formula(formula_str), data = df)
        anova_result <- summary(model)
        
        rows <- rownames(anova_result[[1]])
        fs <- round(anova_result[[1]]$`F value`, 4)
        ps <- round(anova_result[[1]]$`Pr(>F)`, 6)
        
        result_lines <- c()
        for (i in seq_along(rows)) {
          if (!is.na(fs[i]) && !is.na(ps[i])) {
            signif <- ifelse(ps[i] < input$alpha_mean, "Tolak H₀", "Terima H₀")
            result_lines <- c(result_lines, paste0(
              "<p><strong>", rows[i], ":</strong> F = ", fs[i],
              ", p-value = ", ps[i], " ⟶ ", signif, "</p>"
            ))
          }
        }
        
        output$mean_test_results <- renderUI({
          HTML(paste0(
            '<div class="hypothesis-card">',
            '<div class="hypothesis-header">🧪 Two-Way ANOVA</div>',
            '<div class="hypothesis-content">',
            paste0(result_lines, collapse = "\n"),
            '<div class="', ifelse(any(ps < input$alpha_mean, na.rm = TRUE), 'decision-reject', 'decision-accept'), '">',
            '<strong>Kesimpulan Umum:</strong> ',
            ifelse(any(ps < input$alpha_mean, na.rm = TRUE),
                   "Ada pengaruh signifikan dari faktor atau interaksi.",
                   "Tidak ditemukan pengaruh signifikan."),
            '</div>',
            '</div></div>'
          ))
        })
        
      } else {
        # ANOVA for multiple groups
        cat_factor <- factor(values$data[[input$mean_cat_var]])
        selected_data <- numeric_data[cat_factor %in% input$mean_categories]
        selected_factor <- factor(cat_factor[cat_factor %in% input$mean_categories])
        
        anova_result <- aov(selected_data ~ selected_factor)
        anova_summary <- summary(anova_result)
        
        output$mean_test_results <- renderUI({
          f_stat <- anova_summary[[1]][["F value"]][1]
          p_value <- anova_summary[[1]][["Pr(>F)"]][1]
          
          HTML(paste0(
            '<div class="hypothesis-card">',
            '<div class="hypothesis-header">🧪 ANOVA - Uji Beda Rata-rata Multiple Groups</div>',
            '<div class="hypothesis-content">',
            '<p><strong>Groups:</strong> ', paste(input$mean_categories, collapse = ", "), '</p>',
            '<p><strong>H₀:</strong> μ₁ = μ₂ = ... = μₖ</p>',
            '<p><strong>H₁:</strong> Minimal ada satu μᵢ yang berbeda</p>',
            '<p><strong>F-statistik:</strong> ', round(f_stat, 4), '</p>',
            '<p><strong>p-value:</strong> ', round(p_value, 6), '</p>',
            '<div class="', ifelse(p_value < input$alpha_mean, 'decision-reject', 'decision-accept'), '">',
            '<strong>Keputusan:</strong> ', 
            ifelse(p_value < input$alpha_mean, 
                   paste("Tolak H₀ (p-value =", round(p_value, 6), "< α =", input$alpha_mean, ")"),
                   paste("Terima H₀ (p-value =", round(p_value, 6), "≥ α =", input$alpha_mean, ")")),
            '</div>',
            '</div></div>'
          ))
        })
      }
      
    }, error = function(e) {
      output$mean_test_results <- renderUI({
        HTML(paste0('<div class="hypothesis-card"><div class="hypothesis-header">❌ Error</div>',
                    '<div class="hypothesis-content"><p>Error dalam uji hipotesis: ', e$message, '</p></div></div>'))
      })
    })
  })
  
  
  # VARIANCE TESTS
  observe({
    if(input$var_cat_var != "" && input$var_cat_var %in% names(values$data)) {
      categories <- unique(values$data[[input$var_cat_var]])
      updateSelectInput(session, "var_categories", choices = categories)
    }
  })
  
  observeEvent(input$run_var_test, {
    req(input$var_numeric_var)
    
    tryCatch({
      numeric_data <- values$data[[input$var_numeric_var]]
      
      output$var_test_done <- reactive({ TRUE })
      outputOptions(output, "var_test_done", suspendWhenHidden = FALSE)
      
      if(input$var_cat_var == "" || is.null(input$var_categories)) {
        # Chi-square test for single variance
        n <- length(numeric_data)
        sample_var <- var(numeric_data, na.rm = TRUE)
        chi_stat <- (n - 1) * sample_var / input$sigma2_0_var
        p_value <- 2 * min(pchisq(chi_stat, n-1), 1 - pchisq(chi_stat, n-1))
        
        output$var_test_results <- renderUI({
          HTML(paste0(
            '<div class="hypothesis-card">',
            '<div class="hypothesis-header">🧪 Uji Chi-Square untuk Varians</div>',
            '<div class="hypothesis-content">',
            '<p><strong>H₀:</strong> σ² = ', input$sigma2_0_var, '</p>',
            '<p><strong>H₁:</strong> σ² ≠ ', input$sigma2_0_var, '</p>',
            '<p><strong>n:</strong> ', n, '</p>',
            '<p><strong>Sample Variance:</strong> ', round(sample_var, 4), '</p>',
            '<p><strong>Chi-square statistik:</strong> ', round(chi_stat, 4), '</p>',
            '<p><strong>df:</strong> ', n-1, '</p>',
            '<p><strong>p-value:</strong> ', round(p_value, 6), '</p>',
            '<div class="', ifelse(p_value < input$alpha_var, 'decision-reject', 'decision-accept'), '">',
            '<strong>Keputusan:</strong> ', 
            ifelse(p_value < input$alpha_var, 
                   paste("Tolak H₀ (p-value =", round(p_value, 6), "< α =", input$alpha_var, ")"),
                   paste("Terima H₀ (p-value =", round(p_value, 6), "≥ α =", input$alpha_var, ")")),
            '</div>',
            '</div></div>'
          ))
        })
        
      } else if(length(input$var_categories) == 2) {
        # F-test for equality of variances
        group1_data <- numeric_data[values$data[[input$var_cat_var]] == input$var_categories[1]]
        group2_data <- numeric_data[values$data[[input$var_cat_var]] == input$var_categories[2]]
        test_result <- var.test(group1_data, group2_data, conf.level = 1 - input$alpha_var)
        
        output$var_test_results <- renderUI({
          HTML(paste0(
            '<div class="hypothesis-card">',
            '<div class="hypothesis-header">🧪 Uji F untuk Kesamaan Varians</div>',
            '<div class="hypothesis-content">',
            '<p><strong>Grup 1:</strong> ', input$var_categories[1], ' (n = ', length(group1_data), ')</p>',
            '<p><strong>Grup 2:</strong> ', input$var_categories[2], ' (n = ', length(group2_data), ')</p>',
            '<p><strong>H₀:</strong> σ₁² = σ₂²</p>',
            '<p><strong>H₁:</strong> σ₁² ≠ σ₂²</p>',
            '<p><strong>F-statistik:</strong> ', round(test_result$statistic, 4), '</p>',
            '<p><strong>df:</strong> ', test_result$parameter[1], ', ', test_result$parameter[2], '</p>',
            '<p><strong>p-value:</strong> ', round(test_result$p.value, 6), '</p>',
            '<div class="', ifelse(test_result$p.value < input$alpha_var, 'decision-reject', 'decision-accept'), '">',
            '<strong>Keputusan:</strong> ', 
            ifelse(test_result$p.value < input$alpha_var, 
                   paste("Tolak H₀ (p-value =", round(test_result$p.value, 6), "< α =", input$alpha_var, ")"),
                   paste("Terima H₀ (p-value =", round(test_result$p.value, 6), "≥ α =", input$alpha_var, ")")),
            '</div>',
            '</div></div>'
          ))
        })
      }
      
    }, error = function(e) {
      output$var_test_results <- renderUI({
        HTML(paste0('<div class="hypothesis-card"><div class="hypothesis-header">❌ Error</div>',
                    '<div class="hypothesis-content"><p>Error dalam uji varians: ', e$message, '</p></div></div>'))
      })
    })
  })
  
  
  
  
  # PROPORTION TESTS
  observe({
    if(input$prop_test_var != "" && input$prop_test_var %in% names(values$data)) {
      categories <- unique(values$data[[input$prop_test_var]])
      updateSelectInput(session, "prop_success", choices = categories)
    }
    
    if(input$prop_cat_var != "" && input$prop_cat_var %in% names(values$data)) {
      categories <- unique(values$data[[input$prop_cat_var]])
      updateSelectInput(session, "prop_categories", choices = categories)
    }
  })
  
  observeEvent(input$run_prop_test, {
    req(input$prop_test_var, input$prop_success)
    
    tryCatch({
      output$prop_test_done <- reactive({ TRUE })
      outputOptions(output, "prop_test_done", suspendWhenHidden = FALSE)
      
      if(input$prop_cat_var == "" || is.null(input$prop_categories)) {
        # One-sample proportion test
        success_count <- sum(values$data[[input$prop_test_var]] == input$prop_success, na.rm = TRUE)
        total_count <- sum(!is.na(values$data[[input$prop_test_var]]))
        test_result <- prop.test(success_count, total_count, p = input$p_0_prop, conf.level = 1 - input$alpha_prop)
        
        output$prop_test_results <- renderUI({
          HTML(paste0(
            '<div class="hypothesis-card">',
            '<div class="hypothesis-header">🧪 Uji Proporsi Satu Sampel</div>',
            '<div class="hypothesis-content">',
            '<p><strong>H₀:</strong> p = ', input$p_0_prop, '</p>',
            '<p><strong>H₁:</strong> p ≠ ', input$p_0_prop, '</p>',
            '<p><strong>Sukses:</strong> ', success_count, '</p>',
            '<p><strong>Total:</strong> ', total_count, '</p>',
            '<p><strong>Sample Proportion:</strong> ', round(success_count/total_count, 4), '</p>',
            '<p><strong>Chi-square statistik:</strong> ', round(test_result$statistic, 4), '</p>',
            '<p><strong>p-value:</strong> ', round(test_result$p.value, 6), '</p>',
            '<div class="', ifelse(test_result$p.value < input$alpha_prop, 'decision-reject', 'decision-accept'), '">',
            '<strong>Keputusan:</strong> ', 
            ifelse(test_result$p.value < input$alpha_prop, 
                   paste("Tolak H₀ (p-value =", round(test_result$p.value, 6), "< α =", input$alpha_prop, ")"),
                   paste("Terima H₀ (p-value =", round(test_result$p.value, 6), "≥ α =", input$alpha_prop, ")")),
            '</div>',
            '</div></div>'
          ))
        })
        
      } else if(length(input$prop_categories) == 2) {
        # Two-sample proportion test
        group1_data <- values$data[[input$prop_test_var]][values$data[[input$prop_cat_var]] == input$prop_categories[1]]
        group2_data <- values$data[[input$prop_test_var]][values$data[[input$prop_cat_var]] == input$prop_categories[2]]
        
        success1 <- sum(group1_data == input$prop_success, na.rm = TRUE)
        total1 <- sum(!is.na(group1_data))
        success2 <- sum(group2_data == input$prop_success, na.rm = TRUE)
        total2 <- sum(!is.na(group2_data))
        
        test_result <- prop.test(c(success1, success2), c(total1, total2), conf.level = 1 - input$alpha_prop)
        
        output$prop_test_results <- renderUI({
          HTML(paste0(
            '<div class="hypothesis-card">',
            '<div class="hypothesis-header">🧪 Uji Proporsi Dua Sampel</div>',
            '<div class="hypothesis-content">',
            '<p><strong>Grup 1:</strong> ', input$prop_categories[1], ' (', success1, '/', total1, ' = ', round(success1/total1, 4), ')</p>',
            '<p><strong>Grup 2:</strong> ', input$prop_categories[2], ' (', success2, '/', total2, ' = ', round(success2/total2, 4), ')</p>',
            '<p><strong>H₀:</strong> p₁ = p₂</p>',
            '<p><strong>H₁:</strong> p₁ ≠ p₂</p>',
            '<p><strong>Chi-square statistik:</strong> ', round(test_result$statistic, 4), '</p>',
            '<p><strong>p-value:</strong> ', round(test_result$p.value, 6), '</p>',
            '<div class="', ifelse(test_result$p.value < input$alpha_prop, 'decision-reject', 'decision-accept'), '">',
            '<strong>Keputusan:</strong> ', 
            ifelse(test_result$p.value < input$alpha_prop, 
                   paste("Tolak H₀ (p-value =", round(test_result$p.value, 6), "< α =", input$alpha_prop, ")"),
                   paste("Terima H₀ (p-value =", round(test_result$p.value, 6), "≥ α =", input$alpha_prop, ")")),
            '</div>',
            '</div></div>'
          ))
        })
      }
      
    }, error = function(e) {
      output$prop_test_results <- renderUI({
        HTML(paste0('<div class="hypothesis-card"><div class="hypothesis-header">❌ Error</div>',
                    '<div class="hypothesis-content"><p>Error dalam uji proporsi: ', e$message, '</p></div></div>'))
      })
    })
  })
  
  # OLS REGRESSION
  observeEvent(input$run_ols, {
    req(input$ols_y, input$ols_x)
    
    tryCatch({
      # Prepare data
      y_data <- values$data[[input$ols_y]]
      x_data <- values$data[input$ols_x]
      
      # Create formula
      formula_str <- paste(input$ols_y, "~", paste(input$ols_x, collapse = " + "))
      model_formula <- as.formula(formula_str)
      
      # Fit model
      ols_model <- lm(model_formula, data = values$data)
      values$ols_model <- ols_model
      
      # Model summary
      model_summary <- summary(ols_model)
      
      output$assumption_tests_done <- reactive({ TRUE })
      outputOptions(output, "assumption_tests_done", suspendWhenHidden = FALSE)
      
      output$ols_results <- renderUI({
        coef_table <- model_summary$coefficients
        
        coef_html <- '<table class="enhanced-transposed-table">'
        coef_html <- paste0(coef_html, '<tr><th>Variable</th><th>Estimate</th><th>Std. Error</th><th>t value</th><th>Pr(>|t|)</th><th>Signif.</th></tr>')
        
        for(i in 1:nrow(coef_table)) {
          var_name <- rownames(coef_table)[i]
          estimate <- round(coef_table[i, 1], 4)
          std_error <- round(coef_table[i, 2], 4)
          t_value <- round(coef_table[i, 3], 4)
          p_value <- round(coef_table[i, 4], 6)
          signif <- ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", ifelse(p_value < 0.1, ".", ""))))
          
          coef_html <- paste0(coef_html, '<tr><td><strong>', var_name, '</strong></td><td>', estimate, '</td><td>', std_error, '</td><td>', t_value, '</td><td>', p_value, '</td><td>', signif, '</td></tr>')
        }
        coef_html <- paste0(coef_html, '</table>')
        
        HTML(paste0(
          '<div class="stats-card">',
          '<div class="stats-header">📈 Hasil Regresi OLS</div>',
          '<div class="stats-content">',
          '<p><strong>Formula:</strong> ', formula_str, '</p>',
          '<p><strong>R-squared:</strong> ', round(model_summary$r.squared, 4), '</p>',
          '<p><strong>Adjusted R-squared:</strong> ', round(model_summary$adj.r.squared, 4), '</p>',
          '<p><strong>F-statistic:</strong> ', round(model_summary$fstatistic[1], 4), ' on ', model_summary$fstatistic[2], ' and ', model_summary$fstatistic[3], ' DF</p>',
          '<p><strong>p-value:</strong> ', round(pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), 6), '</p>',
          '<h5 style="color: #3b0f70; margin: 20px 0 10px 0;">Coefficients:</h5>',
          coef_html,
          '<p style="margin-top: 15px; font-size: 12px; color: #666;">Signif. codes: 0 \'***\' 0.001 \'**\' 0.01 \'*\' 0.05 \'.\' 0.1 \' \' 1</p>',
          '</div></div>'
        ))
      })
      
    }, error = function(e) {
      output$ols_results <- renderUI({
        HTML(paste0('<div class="stats-card"><div class="stats-header">❌ Error</div>',
                    '<div class="stats-content"><p>Error dalam regresi OLS: ', e$message, '</p></div></div>'))
      })
    })
  })
  
  # OLS Diagnostic Plot
  output$ols_diagnostic <- renderPlotly({
    req(values$ols_model)
    
    residuals <- residuals(values$ols_model)
    fitted_values <- fitted(values$ols_model)
    
    p <- ggplot(data.frame(fitted = fitted_values, residuals = residuals), aes(x = fitted, y = residuals)) +
      geom_point(color = "#5e4fa2", alpha = 0.6) +
      geom_hline(yintercept = 0, color = "#3b0f70", linetype = "dashed") +
      geom_smooth(method = "loess", color = "#ff6b6b", se = FALSE) +
      labs(title = "📈 Residuals vs Fitted", 
           x = "Fitted Values", 
           y = "Residuals") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#3b0f70", size = 16, face = "bold"),
        axis.title = element_text(color = "#5e4fa2", size = 12, face = "bold")
      )
    
    ggplotly(p)
  })
  
  # OLS Interpretation
  output$ols_interpretation <- renderUI({
    req(values$ols_model)
    
    model_summary <- summary(values$ols_model)
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    f_pvalue <- pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)
    
    interpretation <- paste0(
      '<div class="stats-card">',
      '<div class="stats-header">💡 Interpretasi Model</div>',
      '<div class="stats-content">',
      '<h5 style="color: #3b0f70; margin-bottom: 15px;">📊 Goodness of Fit:</h5>',
      '<p>• Model menjelaskan <strong>', round(r_squared * 100, 2), '%</strong> variasi dalam ', input$ols_y, '</p>',
      '<p>• Adjusted R² = <strong>', round(adj_r_squared, 4), '</strong></p>',
      '<h5 style="color: #3b0f70; margin: 20px 0 15px 0;">🧪 Model Significance:</h5>',
      '<p>• F-test p-value = <strong>', round(f_pvalue, 6), '</strong></p>',
      '<p>• Model secara keseluruhan <strong>', 
      ifelse(f_pvalue < 0.05, 'signifikan', 'tidak signifikan'), '</strong></p>',
      '<h5 style="color: #3b0f70; margin: 20px 0 15px 0;">📈 Model Quality:</h5>',
      '<p>• ', ifelse(r_squared > 0.7, 'Model sangat baik (R² > 0.7)', 
                      ifelse(r_squared > 0.5, 'Model baik (R² > 0.5)', 
                             ifelse(r_squared > 0.3, 'Model cukup (R² > 0.3)', 'Model lemah (R² ≤ 0.3)'))), '</p>',
      '</div></div>'
    )
    
    HTML(interpretation)
  })
  
  
  # NEW: OLS ASSUMPTION TESTS
  observeEvent(input$test_ols_assumptions, {
    req(values$ols_model)
    
    tryCatch({
      model <- values$ols_model
      residuals <- residuals(model)
      fitted_values <- fitted(model)
      
      # 1. Normality Test (Shapiro-Wilk)
      normality_test <- shapiro.test(residuals)
      
      # 2. Homoscedasticity Test (Breusch-Pagan)
      bp_test <- bptest(model)
      
      # 3. Autocorrelation Test (Durbin-Watson)
      dw_test <- dwtest(model)
      
      # 4. Multicollinearity Test (VIF)
      vif_values <- NULL
      if(length(input$ols_x) > 1) {
        vif_values <- vif(model)
      }
      
      output$assumption_tests_done <- reactive({ TRUE })
      outputOptions(output, "assumption_tests_done", suspendWhenHidden = FALSE)
      
      output$assumption_test_results <- renderUI({
        results_html <- '<div class="stats-card">'
        results_html <- paste0(results_html, '<div class="stats-header">🔍 Hasil Uji Asumsi Klasik</div>')
        results_html <- paste0(results_html, '<div class="stats-content">')
        
        # Normality
        results_html <- paste0(results_html, 
                               '<div class="hypothesis-card" style="margin: 10px 0;">',
                               '<div class="hypothesis-header">📊 1. Uji Normalitas (Shapiro-Wilk)</div>',
                               '<div class="hypothesis-content">',
                               '<p><strong>H₀:</strong> Residual berdistribusi normal</p>',
                               '<p><strong>W-statistic:</strong> ', round(normality_test$statistic, 4), '</p>',
                               '<p><strong>p-value:</strong> ', round(normality_test$p.value, 6), '</p>',
                               '<div class="', ifelse(normality_test$p.value < 0.05, 'decision-reject', 'decision-accept'), '">',
                               '<strong>Kesimpulan:</strong> ', 
                               ifelse(normality_test$p.value < 0.05, 
                                      'Asumsi normalitas DILANGGAR ❌', 
                                      'Asumsi normalitas TERPENUHI ✅'),
                               '</div></div></div>')
        
        # Homoscedasticity
        results_html <- paste0(results_html,
                               '<div class="hypothesis-card" style="margin: 10px 0;">',
                               '<div class="hypothesis-header">📏 2. Uji Homoskedastisitas (Breusch-Pagan)</div>',
                               '<div class="hypothesis-content">',
                               '<p><strong>H₀:</strong> Varians residual konstan (homoskedastis)</p>',
                               '<p><strong>BP-statistic:</strong> ', round(bp_test$statistic, 4), '</p>',
                               '<p><strong>p-value:</strong> ', round(bp_test$p.value, 6), '</p>',
                               '<div class="', ifelse(bp_test$p.value < 0.05, 'decision-reject', 'decision-accept'), '">',
                               '<strong>Kesimpulan:</strong> ', 
                               ifelse(bp_test$p.value < 0.05, 
                                      'Asumsi homoskedastisitas DILANGGAR (ada heteroskedastisitas) ❌', 
                                      'Asumsi homoskedastisitas TERPENUHI ✅'),
                               '</div></div></div>')
        
        # Autocorrelation
        results_html <- paste0(results_html,
                               '<div class="hypothesis-card" style="margin: 10px 0;">',
                               '<div class="hypothesis-header">🔄 3. Uji Autokorelasi (Durbin-Watson)</div>',
                               '<div class="hypothesis-content">',
                               '<p><strong>H₀:</strong> Tidak ada autokorelasi</p>',
                               '<p><strong>DW-statistic:</strong> ', round(dw_test$statistic, 4), '</p>',
                               '<p><strong>p-value:</strong> ', round(dw_test$p.value, 6), '</p>',
                               '<div class="', ifelse(dw_test$p.value < 0.05, 'decision-reject', 'decision-accept'), '">',
                               '<strong>Kesimpulan:</strong> ', 
                               ifelse(dw_test$p.value < 0.05, 
                                      'Ada indikasi autokorelasi ❌', 
                                      'Tidak ada autokorelasi ✅'),
                               '</div></div></div>')
        
        # Multicollinearity
        if(!is.null(vif_values)) {
          results_html <- paste0(results_html,
                                 '<div class="hypothesis-card" style="margin: 10px 0;">',
                                 '<div class="hypothesis-header">🔗 4. Uji Multikolinieritas (VIF)</div>',
                                 '<div class="hypothesis-content">',
                                 '<p><strong>Kriteria:</strong> VIF < 10 (tidak ada multikolinieritas)</p>')
          
          for(i in 1:length(vif_values)) {
            var_name <- names(vif_values)[i]
            vif_val <- vif_values[i]
            results_html <- paste0(results_html,
                                   '<p><strong>', var_name, ':</strong> VIF = ', round(vif_val, 3), ' ',
                                   ifelse(vif_val < 10, '✅', '❌'), '</p>')
          }
          
          max_vif <- max(vif_values)
          results_html <- paste0(results_html,
                                 '<div class="', ifelse(max_vif >= 10, 'decision-reject', 'decision-accept'), '">',
                                 '<strong>Kesimpulan:</strong> ', 
                                 ifelse(max_vif >= 10, 
                                        'Ada multikolinieritas (VIF ≥ 10) ❌', 
                                        'Tidak ada multikolinieritas ✅'),
                                 '</div></div></div>')
        }
        
        results_html <- paste0(results_html, '</div></div>')
        HTML(results_html)
      })
      
      # Create diagnostic plots
      output$assumption_plots <- renderPlotly({
        # Create 2x2 diagnostic plots
        
        # Q-Q Plot for normality
        qq_data <- data.frame(
          theoretical = qqnorm(residuals, plot.it = FALSE)$x,
          sample = qqnorm(residuals, plot.it = FALSE)$y
        )
        
        p1 <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
          geom_point(color = "#5e4fa2", alpha = 0.6) +
          geom_abline(slope = 1, intercept = 0, color = "#3b0f70", linetype = "dashed") +
          labs(title = "Q-Q Plot (Normalitas)", x = "Theoretical Quantiles", y = "Sample Quantiles") +
          theme_minimal() +
          theme(plot.title = element_text(color = "#3b0f70", size = 12, face = "bold"))
        
        # Residuals vs Fitted (Homoscedasticity & Linearity)
        p2 <- ggplot(data.frame(fitted = fitted_values, residuals = residuals), aes(x = fitted, y = residuals)) +
          geom_point(color = "#5e4fa2", alpha = 0.6) +
          geom_hline(yintercept = 0, color = "#3b0f70", linetype = "dashed") +
          geom_smooth(method = "loess", color = "#ff6b6b", se = FALSE) +
          labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
          theme_minimal() +
          theme(plot.title = element_text(color = "#3b0f70", size = 12, face = "bold"))
        
        # Scale-Location Plot
        sqrt_abs_resid <- sqrt(abs(residuals))
        p3 <- ggplot(data.frame(fitted = fitted_values, sqrt_resid = sqrt_abs_resid), aes(x = fitted, y = sqrt_resid)) +
          geom_point(color = "#5e4fa2", alpha = 0.6) +
          geom_smooth(method = "loess", color = "#ff6b6b", se = FALSE) +
          labs(title = "Scale-Location", x = "Fitted Values", y = "√|Residuals|") +
          theme_minimal() +
          theme(plot.title = element_text(color = "#3b0f70", size = 12, face = "bold"))
        
        # Histogram of Residuals
        p4 <- ggplot(data.frame(residuals = residuals), aes(x = residuals)) +
          geom_histogram(bins = 20, fill = "#5e4fa2", alpha = 0.7, color = "white") +
          geom_density(aes(y = ..density.. * length(residuals) * diff(range(residuals))/20), 
                       color = "#3b0f70", size = 1) +
          labs(title = "Histogram Residuals", x = "Residuals", y = "Frequency") +
          theme_minimal() +
          theme(plot.title = element_text(color = "#3b0f70", size = 12, face = "bold"))
        
        # Combine plots using subplot
        subplot(
          list(
            ggplotly(p1, tooltip = c("x", "y")),
            ggplotly(p2, tooltip = c("x", "y")),
            ggplotly(p3, tooltip = c("x", "y")),
            ggplotly(p4, tooltip = c("x", "y"))
          ),
          nrows = 2, margin = 0.05,
          subplot_titles = c("Q-Q Plot", "Residuals vs Fitted", "Scale-Location", "Histogram")
        ) %>%
          layout(title = list(text = "🔍 Diagnostic Plots", font = list(color = "#3b0f70", size = 16)))
      })
      
    }, error = function(e) {
      output$assumption_test_results <- renderUI({
        HTML(paste0('<div class="stats-card"><div class="stats-header">❌ Error</div>',
                    '<div class="stats-content"><p>Error dalam uji asumsi: ', e$message, '</p></div></div>'))
      })
    })
  })
  
  observeEvent(input$run_spatial, {
    req(input$spatial_y, input$spatial_x)
    
    tryCatch({
      # Check if spatial data is available
      if(is.null(peta_lengkap)) {
        stop("Data spasial tidak tersedia. Pastikan file geojson telah dimuat dengan benar.")
      }
      
      # Prepare spatial data
      spatial_data <- peta_lengkap
      
      # Remove rows with missing values in selected variables
      complete_vars <- c(input$spatial_y, input$spatial_x)
      spatial_data <- spatial_data[complete.cases(st_drop_geometry(spatial_data)[complete_vars]), ]
      
      if(nrow(spatial_data) < 10) {
        stop("Data yang lengkap terlalu sedikit untuk analisis spasial (minimum 10 observasi).")
      }
      if (st_is_longlat(spatial_data)) {
        spatial_data <- st_transform(spatial_data, 3857)  # Web Mercator, atau bisa 23830 untuk Indonesia
      }
      coords <- st_coordinates(st_point_on_surface(spatial_data))
      
      # Create spatial weights matrix
      if(input$spatial_contiguity == "queen") {
        nb <- poly2nb(spatial_data, queen = TRUE)
      } else if(input$spatial_contiguity == "rook") {
        nb <- poly2nb(spatial_data, queen = FALSE)
      } else if(input$spatial_contiguity == "knn") {
        coords <- st_coordinates(st_centroid(spatial_data))
        nb <- knn2nb(knearneigh(coords, k = input$spatial_k))
      } else if(input$spatial_contiguity == "distance") {
        coords <- st_coordinates(st_centroid(spatial_data))
        nb <- dnearneigh(coords, 0, input$spatial_distance)
      }
      
      # Convert to spatial weights
      if(length(nb) == 0 || all(sapply(nb, length) == 0)) {
        stop("Tidak dapat membuat matriks bobot spasial. Coba ubah parameter ketetanggaan.")
      }
      
      listw <- nb2listw(nb, style = "W", zero.policy = TRUE)
      values$spatial_weights <- listw
      
      # Create formula
      formula_str <- paste(input$spatial_y, "~", paste(input$spatial_x, collapse = " + "))
      model_formula <- as.formula(formula_str)
      
      # Fit spatial model
      if(input$spatial_model_type == "lag") {
        spatial_model <- lagsarlm(model_formula, data = spatial_data, listw = listw, zero.policy = TRUE)
      } else if(input$spatial_model_type == "error") {
        spatial_model <- errorsarlm(model_formula, data = spatial_data, listw = listw, zero.policy = TRUE)
      } else if(input$spatial_model_type == "durbin") {
        spatial_model <- lagsarlm(model_formula, data = spatial_data, listw = listw, type = "mixed", zero.policy = TRUE)
      }
      
      values$spatial_model <- spatial_model
      
      output$spatial_results_available <- reactive({ TRUE })
      outputOptions(output, "spatial_results_available", suspendWhenHidden = FALSE)
      
      # Display results
      output$spatial_results <- renderUI({
        model_summary <- summary(spatial_model)
        
        # Extract coefficients
        coef_table <- model_summary$Coef
        
        coef_html <- '<table class="enhanced-transposed-table">'
        coef_html <- paste0(coef_html, '<tr><th>Variable</th><th>Estimate</th><th>Std. Error</th><th>z value</th><th>Pr(>|z|)</th><th>Signif.</th></tr>')
        
        for(i in 1:nrow(coef_table)) {
          var_name <- rownames(coef_table)[i]
          estimate <- round(coef_table[i, 1], 4)
          std_error <- round(coef_table[i, 2], 4)
          z_value <- round(coef_table[i, 3], 4)
          p_value <- round(coef_table[i, 4], 6)
          signif <- ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", ifelse(p_value < 0.1, ".", ""))))
          
          coef_html <- paste0(coef_html, '<tr><td><strong>', var_name, '</strong></td><td>', estimate, '</td><td>', std_error, '</td><td>', z_value, '</td><td>', p_value, '</td><td>', signif, '</td></tr>')
        }
        coef_html <- paste0(coef_html, '</table>')
        
        # Spatial parameter
        spatial_param_html <- ""
        if(input$spatial_model_type == "lag") {
          rho <- spatial_model$rho
          rho_se <- sqrt(spatial_model$rho.se^2)
          spatial_param_html <- paste0('<p><strong>Spatial Parameter (ρ):</strong> ', round(rho, 4), ' (SE: ', round(rho_se, 4), ')</p>')
        } else if(input$spatial_model_type == "error") {
          lambda <- spatial_model$lambda
          lambda_se <- sqrt(spatial_model$lambda.se^2)
          spatial_param_html <- paste0('<p><strong>Spatial Parameter (λ):</strong> ', round(lambda, 4), ' (SE: ', round(lambda_se, 4), ')</p>');
        }
        
        HTML(paste0(
          '<div class="stats-card">',
          '<div class="stats-header">🗺️ Hasil Regresi Spasial</div>',
          '<div class="stats-content">',
          '<p><strong>Model Type:</strong> ', 
          switch(input$spatial_model_type,
                 "lag" = "Spatial Lag Model (SAR)",
                 "error" = "Spatial Error Model (SEM)", 
                 "durbin" = "Spatial Durbin Model (SDM)"), '</p>',
          '<p><strong>Contiguity Method:</strong> ', 
          switch(input$spatial_contiguity,
                 "queen" = "Queen's Contiguity",
                 "rook" = "Rook's Contiguity",
                 "knn" = paste("K-Nearest Neighbors (k =", input$spatial_k, ")"),
                 "distance" = paste("Distance-based (threshold =", input$spatial_distance, ")")), '</p>',
          '<p><strong>Formula:</strong> ', formula_str, '</p>',
          '<p><strong>Log-Likelihood:</strong> ', round(spatial_model$LL, 4), '</p>',
          '<p><strong>AIC:</strong> ', round(AIC(spatial_model), 4), '</p>',
          spatial_param_html,
          '<h5 style="color: #3b0f70; margin: 20px 0 10px 0;">Coefficients:</h5>',
          coef_html,
          '<p style="margin-top: 15px; font-size: 12px; color: #666;">Signif. codes: 0 \'***\' 0.001 \'**\' 0.01 \'*\' 0.05 \'.\' 0.1 \' \' 1</p>',
          '</div></div>'
        ))
      })
      
      # Interpretation
      output$spatial_interpretation <- renderUI({
        model_summary <- summary(spatial_model)
        
        interpretation_html <- '<div class="stats-card">'
        interpretation_html <- paste0(interpretation_html, '<div class="stats-header">💡 Interpretasi Model Spasial</div>')
        interpretation_html <- paste0(interpretation_html, '<div class="stats-content">')
        
        if(input$spatial_model_type == "lag") {
          rho <- spatial_model$rho
          interpretation_html <- paste0(interpretation_html,
                                        '<h5 style="color: #3b0f70; margin-bottom: 15px;">🗺️ Spatial Lag Model (SAR):</h5>',
                                        '<p>• Parameter spasial ρ = <strong>', round(rho, 4), '</strong></p>',
                                        '<p>• ', ifelse(abs(rho) > 0.1, 
                                                        paste('Ada dependensi spasial yang', ifelse(abs(rho) > 0.3, 'kuat', 'sedang')), 
                                                        'Dependensi spasial lemah'), '</p>',
                                        '<p>• Model menangkap efek spillover antar wilayah tetangga</p>')
        } else if(input$spatial_model_type == "error") {
          lambda <- spatial_model$lambda
          interpretation_html <- paste0(interpretation_html,
                                        '<h5 style="color: #3b0f70; margin-bottom: 15px;">🗺️ Spatial Error Model (SEM):</h5>',
                                        '<p>• Parameter spasial λ = <strong>', round(lambda, 4), '</strong></p>',
                                        '<p>• ', ifelse(abs(lambda) > 0.1, 
                                                        'Ada autokorelasi spasial dalam error term', 
                                                        'Autokorelasi spasial dalam error lemah'), '</p>',
                                        '<p>• Model mengoreksi bias akibat omitted spatial variables</p>')
        } else if(input$spatial_model_type == "durbin") {
          interpretation_html <- paste0(interpretation_html,
                                        '<h5 style="color: #3b0f70; margin-bottom: 15px;">🗺️ Spatial Durbin Model (SDM):</h5>',
                                        '<p>• Model ini menggabungkan efek langsung dan tidak langsung dari variabel independen pada wilayah tetangga.</p>',
                                        '<p>• Interpretasi koefisien mencerminkan efek lokal dan spillover</p>')
        }
        
        
        interpretation_html <- paste0(interpretation_html,
                                      '<h5 style="color: #3b0f70; margin: 20px 0 15px 0;">📊 Model Comparison:</h5>',
                                      '<p>• AIC = <strong>', round(AIC(spatial_model), 4), '</strong></p>',
                                      '<p>• Log-Likelihood = <strong>', round(spatial_model$LL, 4), '</strong></p>',
                                      '<p>• Bandingkan dengan model OLS untuk menilai improvement</p>')
        
        interpretation_html <- paste0(interpretation_html, '</div></div>')
        HTML(interpretation_html)
      })
      
    }, error = function(e) {
      output$spatial_results <- renderUI({
        HTML(paste0('<div class="stats-card"><div class="stats-header">❌ Error</div>',
                    '<div class="stats-content"><p>Error dalam regresi spasial: ', e$message, '</p></div></div>'))
      })
    })
    
  })
  
  # Load reactive diagnosis ----
  `%||%` <- function(x, y) if (!is.null(x) && !is.na(x)) x else y
  
  spatial_diag_data <- eventReactive(input$run_spatial_diag, {
    print("===> Tombol run_spatial ditekan")
    req(input$spatial_y, input$spatial_x)
    validate(need(!is.null(peta_lengkap), "Data spasial belum dimuat."))
    
    spatial_data <- peta_lengkap
    complete_vars <- c(input$spatial_y, input$spatial_x)
    spatial_data <- spatial_data[complete.cases(st_drop_geometry(spatial_data)[complete_vars]), ]
    validate(need(nrow(spatial_data) >= 10, "Terlalu sedikit observasi."))
    
    coords <- st_coordinates(st_point_on_surface(spatial_data))
    nb <- switch(input$spatial_contiguity,
                 "queen"    = poly2nb(spatial_data, queen = TRUE),
                 "rook"     = poly2nb(spatial_data, queen = FALSE),
                 "knn"      = knn2nb(knearneigh(coords, k = input$spatial_k)),
                 "distance" = dnearneigh(coords, 0, input$spatial_distance))
    validate(need(length(nb) > 0 && !all(sapply(nb, length) == 0),
                  "Matriks ketetanggaan gagal dibuat."))
    
    listw <- nb2listw(nb, style = "W", zero.policy = TRUE)
    
    formula_str <- paste(input$spatial_y, "~", paste(input$spatial_x, collapse = " + "))
    model_formula <- as.formula(formula_str)
    ols_model <- lm(model_formula, data = spatial_data)
    
    moran_test <- tryCatch(lm.morantest(ols_model, listw), error = function(e) NULL)
    moran_val <- if (!is.null(moran_test)) round(moran_test$estimate["Moran I statistic"], 4) else NA
    moran_p <- if (!is.null(moran_test)) round(moran_test$p.value, 6) else NA
    
    lm_tests <- tryCatch(lm.RStests(ols_model, listw), error = function(e) NULL)
    
    label_map <- c(
      "lag"         = "LM - Spatial Lag",
      "error"       = "LM - Spatial Error",
      "robust.lag"  = "Robust LM - Lag",
      "robust.error"= "Robust LM - Error",
      "sarma"       = "SARMA Test"
    )
    
    best_model <- "LM tests tidak tersedia."
    lm_table <- "<p>LM tests gagal dihitung.</p>"
    
    if (!is.null(lm_tests)) {
      signif_tests <- sapply(lm_tests, function(t) t$p.value)
      signif_tests <- signif_tests[signif_tests < input$alpha_spatial]
      
      best_model <- if (length(signif_tests) == 0) {
        "Tidak ada model spasial yang direkomendasikan pada tingkat signifikansi ini."
      } else {
        best <- names(which.min(signif_tests))
        switch(best,
               "lag"         = "Model Spatial Lag (SAR) direkomendasikan.",
               "error"       = "Model Spatial Error (SEM) direkomendasikan.",
               "robust.lag"  = "Model Spatial Lag (SAR) direkomendasikan (robust).",
               "robust.error"= "Model Spatial Error (SEM) direkomendasikan (robust).",
               "sarma"       = "Model SARMA direkomendasikan.",
               "Model spasial direkomendasikan.")
      }
      
      lm_table <- '<table class="enhanced-transposed-table">'
      lm_table <- paste0(lm_table, '<tr><th>Test</th><th>Statistic</th><th>p-value</th></tr>')
      for (testname in names(lm_tests)) {
        test <- lm_tests[[testname]]
        stat <- round(test$statistic, 4)
        pval <- round(test$p.value, 6)
        label <- label_map[testname] %||% testname
        highlight <- if (!is.null(signif_tests) && testname == names(which.min(signif_tests))) {
          " style='background-color: #fffae6; font-weight: bold;'"
        } else {
          ""
        }
        lm_table <- paste0(lm_table, '<tr', highlight, '><td>', label, '</td><td>', stat, '</td><td>', pval, '</td></tr>')
      }
      lm_table <- paste0(lm_table, '</table>')
    }
    
    list(
      moran_val = moran_val,
      moran_p = moran_p,
      best_model = best_model,
      lm_table = lm_table
    )
  })
  
  output$spatial_diagnostics <- renderUI({
    result <- spatial_diag_data()
    
    HTML(paste0(
      '<div class="stats-card">',
      '<div class="stats-header">🧪 Diagnosis Spasial</div>',
      '<div class="stats-content">',
      '<h5>Moran\'s I dari Residual OLS</h5>',
      if (is.na(result$moran_val)) {
        '<p><strong>Moran\'s I:</strong> <span style="color:red;">Tidak dapat dihitung</span></p>'
      } else {
        paste0('<p><strong>Moran\'s I:</strong> ', result$moran_val, '</p>',
               '<p><strong>p-value:</strong> ', result$moran_p, '</p>')
      },
      '<h5 style="margin-top:15px;">Lagrange Multiplier Test</h5>',
      result$lm_table,
      '<p style="margin-top: 15px; color: #3b0f70;"><strong>Rekomendasi:</strong> ', result$best_model, '</p>',
      '<p style="margin-top: 10px; font-size: 12px; color: #666;">Gunakan hasil ini untuk memilih jenis model spasial yang sesuai.</p>',
      '</div></div>'
    ))
  })
  
  output$spatial_diagnostics_available <- reactive({
    !is.null(spatial_diag_data())
  })
  outputOptions(output, "spatial_diagnostics_available", suspendWhenHidden = FALSE)
  
  
  #ALL DOWNLOAD
  # DOWNLOAD HANDLERS FOR HOME
  output$download_home_table <- downloadHandler(
    filename = function() paste0("metadata_table_", Sys.Date(), ".csv"),
    content = function(file) {
      metadata <- data.frame(
        Variabel = c("DISTRICTCODE", "FAMILYSIZE", "LOWEDU", "POVERTY", "ILLITERATE", "NOSEWER", "NOELECTRIC", "TAPWATER", "wilayah"),
        Deskripsi = c("Kode Kabupaten/Kota", "Ukuran Keluarga Rata-rata", "Pendidikan Rendah (%)", "Kemiskinan (%)", 
                      "Buta Huruf (%)", "Tanpa Saluran Pembuangan (%)", "Tanpa Listrik (%)", "Air Keran (%)", "Wilayah Regional"),
        Tipe = c("Karakter", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Kategorik"),
        Satuan = c("-", "Orang", "%", "%", "%", "%", "%", "%", "-")
      )
      write.csv(metadata, file, row.names = FALSE)
    }
  )
  
  output$download_home_pdf <- downloadHandler(
    filename = function() paste0("laporan_home_", Sys.Date(), ".pdf"),
    content = function(file) {
      content_list <- c(
        "# Dashboard Grid Gap - Laporan Home",
        "",
        "## Ringkasan Data",
        paste("- Total Observasi:", nrow(data_raw)),
        paste("- Rata-rata Kemiskinan:", round(mean(data_raw$POVERTY, na.rm = TRUE), 2), "%"),
        paste("- Rata-rata Tanpa Listrik:", round(mean(data_raw$NOELECTRIC, na.rm = TRUE), 2), "%"),
        "",
        "## Metadata Variabel",
        "",
        "| Variabel | Deskripsi | Tipe | Satuan |",
        "|----------|-----------|------|--------|",
        "| DISTRICTCODE | Kode Kabupaten/Kota | Karakter | - |",
        "| FAMILYSIZE | Ukuran Keluarga Rata-rata | Numerik | Orang |",
        "| LOWEDU | Pendidikan Rendah | Numerik | % |",
        "| POVERTY | Kemiskinan | Numerik | % |",
        "| ILLITERATE | Buta Huruf | Numerik | % |",
        "| NOSEWER | Tanpa Saluran Pembuangan | Numerik | % |",
        "| NOELECTRIC | Tanpa Listrik | Numerik | % |",
        "| TAPWATER | Air Keran | Numerik | % |",
        "| wilayah | Wilayah Regional | Kategorik | - |"
      )
      
      create_pdf_report(content_list, file, "Dashboard Grid Gap - Laporan Home")
    }
  )
  # DOWNLOAD HANDLERS FOR KATEGORISASI
  output$download_cat_histogram <- downloadHandler(
    filename = function() paste0("kategorisasi_histogram_", input$cat_var, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(values$categorized_data)
      p <- ggplot(data.frame(x = values$categorized_data$original), aes(x = x)) +
        geom_histogram(bins = 30, fill = "#5e4fa2", alpha = 0.7, color = "white") +
        geom_vline(xintercept = values$categorized_data$breaks, color = "#fd7e14", linetype = "dashed", size = 1) +
        labs(title = paste("Distribusi", input$cat_var, "dengan Batas Kategori"),
             x = input$cat_var, y = "Frekuensi") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, color = "#3b0f70", size = 14, face = "bold"))
      save_ggplot(p, file)
    }
  )
  
  output$download_cat_barchart <- downloadHandler(
    filename = function() paste0("kategorisasi_barchart_", input$cat_var, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(values$categorized_data)
      freq_data <- table(values$categorized_data$categorized)
      p <- ggplot(data.frame(kategori = names(freq_data), frekuensi = as.numeric(freq_data)), 
                  aes(x = kategori, y = frekuensi)) +
        geom_col(fill = c("#3b0f70", "#5e4fa2", "#9970ab", "#ffff99")[1:length(freq_data)], alpha = 0.8) +
        geom_text(aes(label = frekuensi), vjust = -0.5, color = "#3b0f70", fontface = "bold") +
        labs(title = "Distribusi Hasil Kategorisasi", x = "Kategori", y = "Frekuensi") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, color = "#3b0f70", size = 14, face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1))
      save_ggplot(p, file)
    }
  )
  
  output$download_cat_table <- downloadHandler(
    filename = function() paste0("kategorisasi_statistik_", input$cat_var, "_", Sys.Date(), ".csv"),
    content = function(file) {
      req(values$categorized_data)
      freq_table <- table(values$categorized_data$categorized)
      prop_table <- prop.table(freq_table) * 100
      result_table <- data.frame(
        Kategori = names(freq_table),
        Frekuensi = as.numeric(freq_table),
        Persentase = round(as.numeric(prop_table), 2)
      )
      write.csv(result_table, file, row.names = FALSE)
    }
  )
  
  output$download_cat_pdf <- downloadHandler(
    filename = function() paste0("laporan_kategorisasi_", input$cat_var, "_", Sys.Date(), ".pdf"),
    content = function(file) {
      req(values$categorized_data)
      
      # Save plots temporarily
      temp_hist <- tempfile(fileext = ".png")
      temp_bar <- tempfile(fileext = ".png")
      
      # Create histogram
      p1 <- ggplot(data.frame(x = values$categorized_data$original), aes(x = x)) +
        geom_histogram(bins = 30, fill = "#5e4fa2", alpha = 0.7, color = "white") +
        geom_vline(xintercept = values$categorized_data$breaks, color = "#fd7e14", linetype = "dashed", size = 1) +
        labs(title = paste("Distribusi", input$cat_var, "dengan Batas Kategori"),
             x = input$cat_var, y = "Frekuensi") +
        theme_minimal()
      save_ggplot(p1, temp_hist)
      
      # Create bar chart
      freq_data <- table(values$categorized_data$categorized)
      p2 <- ggplot(data.frame(kategori = names(freq_data), frekuensi = as.numeric(freq_data)), 
                   aes(x = kategori, y = frekuensi)) +
        geom_col(fill = c("#3b0f70", "#5e4fa2", "#9970ab", "#ffff99")[1:length(freq_data)], alpha = 0.8) +
        geom_text(aes(label = frekuensi), vjust = -0.5, color = "#3b0f70", fontface = "bold") +
        labs(title = "Distribusi Hasil Kategorisasi", x = "Kategori", y = "Frekuensi") +
        theme_minimal()
      save_ggplot(p2, temp_bar)
      
      # Create content
      freq_table <- table(values$categorized_data$categorized)
      prop_table <- prop.table(freq_table) * 100
      
      content_list <- c(
        paste("# Laporan Kategorisasi Variabel", input$cat_var),
        "",
        "## Pengaturan Kategorisasi",
        paste("- **Variabel:** ", input$cat_var),
        paste("- **Metode:** ", input$cat_method),
        paste("- **Jumlah Kategori:** ", input$cat_n_classes),
        "",
        "## Histogram Distribusi",
        paste0("![Histogram](", temp_hist, ")"),
        "",
        "## Bar Chart Hasil Kategorisasi", 
        paste0("![Bar Chart](", temp_bar, ")"),
        "",
        "## Statistik Kategorisasi",
        "",
        "| Kategori | Frekuensi | Persentase |",
        "|----------|-----------|------------|",
        paste0("| ", names(freq_table), " | ", freq_table, " | ", round(prop_table, 1), "% |", collapse = "\n")
      )
      
      create_pdf_report(content_list, file, paste("Laporan Kategorisasi -", input$cat_var))
      
      # Clean up temp files
      file.remove(temp_hist, temp_bar)
    }
  )
  output$download_trans_before <- downloadHandler(
    filename = function() paste0("transformasi_sebelum_", values$transformed_data$variable, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(values$transformed_data)
      p <- ggplot(data.frame(x = values$transformed_data$original), aes(y = x)) +
        geom_boxplot(fill = "#5e4fa2", alpha = 0.7, color = "#3b0f70") +
        labs(title = paste("Sebelum Transformasi:", values$transformed_data$variable),
             y = values$transformed_data$variable) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, color = "#3b0f70", size = 12, face = "bold"))
      save_ggplot(p, file)
    }
  )
  
  output$download_trans_after <- downloadHandler(
    filename = function() paste0("transformasi_sesudah_", values$transformed_data$variable, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(values$transformed_data)
      p <- ggplot(data.frame(x = values$transformed_data$transformed), aes(y = x)) +
        geom_boxplot(fill = "#fd7e14", alpha = 0.7, color = "#e67e22") +
        labs(title = paste("Setelah", values$transformed_data$method),
             y = paste(values$transformed_data$method, "dari", values$transformed_data$variable)) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, color = "#fd7e14", size = 12, face = "bold"))
      save_ggplot(p, file)
    }
  )
  
  output$download_trans_table <- downloadHandler(
    filename = function() paste0("transformasi_statistik_", values$transformed_data$variable, "_", Sys.Date(), ".csv"),
    content = function(file) {
      req(values$transformed_data)
      
      original_stats <- c(
        mean = mean(values$transformed_data$original, na.rm = TRUE),
        median = median(values$transformed_data$original, na.rm = TRUE),
        sd = sd(values$transformed_data$original, na.rm = TRUE),
        min = min(values$transformed_data$original, na.rm = TRUE),
        max = max(values$transformed_data$original, na.rm = TRUE),
        skewness = skewness(values$transformed_data$original, na.rm = TRUE),
        kurtosis = kurtosis(values$transformed_data$original, na.rm = TRUE)
      )
      
      transformed_stats <- c(
        mean = mean(values$transformed_data$transformed, na.rm = TRUE),
        median = median(values$transformed_data$transformed, na.rm = TRUE),
        sd = sd(values$transformed_data$transformed, na.rm = TRUE),
        min = min(values$transformed_data$transformed, na.rm = TRUE),
        max = max(values$transformed_data$transformed, na.rm = TRUE),
        skewness = skewness(values$transformed_data$transformed, na.rm = TRUE),
        kurtosis = kurtosis(values$transformed_data$transformed, na.rm = TRUE)
      )
      
      comparison_table <- data.frame(
        Statistik = c("Mean", "Median", "Std Dev", "Min", "Max", "Skewness", "Kurtosis"),
        Sebelum = round(original_stats, 4),
        Sesudah = round(transformed_stats, 4),
        Perubahan = round(transformed_stats - original_stats, 4)
      )
      
      write.csv(comparison_table, file, row.names = FALSE)
    }
  )
  
  output$download_trans_pdf <- downloadHandler(
    filename = function() paste0("laporan_transformasi_", values$transformed_data$variable, "_", Sys.Date(), ".pdf"),
    content = function(file) {
      req(values$transformed_data)
      
      # Save plots temporarily
      temp_before <- tempfile(fileext = ".png")
      temp_after <- tempfile(fileext = ".png")
      
      # Create before plot
      p1 <- ggplot(data.frame(x = values$transformed_data$original), aes(y = x)) +
        geom_boxplot(fill = "#5e4fa2", alpha = 0.7, color = "#3b0f70") +
        labs(title = paste("Sebelum Transformasi:", values$transformed_data$variable),
             y = values$transformed_data$variable) +
        theme_minimal()
      save_ggplot(p1, temp_before)
      
      # Create after plot
      p2 <- ggplot(data.frame(x = values$transformed_data$transformed), aes(y = x)) +
        geom_boxplot(fill = "#fd7e14", alpha = 0.7, color = "#e67e22") +
        labs(title = paste("Setelah", values$transformed_data$method),
             y = paste(values$transformed_data$method, "dari", values$transformed_data$variable)) +
        theme_minimal()
      save_ggplot(p2, temp_after)
      
      # Create statistics table
      original_stats <- c(
        mean = mean(values$transformed_data$original, na.rm = TRUE),
        median = median(values$transformed_data$original, na.rm = TRUE),
        sd = sd(values$transformed_data$original, na.rm = TRUE),
        skewness = skewness(values$transformed_data$original, na.rm = TRUE),
        kurtosis = kurtosis(values$transformed_data$original, na.rm = TRUE)
      )
      
      transformed_stats <- c(
        mean = mean(values$transformed_data$transformed, na.rm = TRUE),
        median = median(values$transformed_data$transformed, na.rm = TRUE),
        sd = sd(values$transformed_data$transformed, na.rm = TRUE),
        skewness = skewness(values$transformed_data$transformed, na.rm = TRUE),
        kurtosis = kurtosis(values$transformed_data$transformed, na.rm = TRUE)
      )
      
      content_list <- c(
        paste("# Laporan Transformasi Variabel", values$transformed_data$variable),
        "",
        "## Pengaturan Transformasi",
        paste("- **Variabel:** ", values$transformed_data$variable),
        paste("- **Metode:** ", values$transformed_data$method),
        "",
        "## Box Plot Sebelum Transformasi",
        paste0("![Sebelum](", temp_before, ")"),
        "",
        "## Box Plot Setelah Transformasi", 
        paste0("![Sesudah](", temp_after, ")"),
        "",
        "## Perbandingan Statistik",
        "",
        "| Statistik | Sebelum | Sesudah | Perubahan |",
        "|-----------|---------|---------|-----------|",
        paste0("| Mean | ", round(original_stats["mean"], 4), " | ", round(transformed_stats["mean"], 4), " | ", round(transformed_stats["mean"] - original_stats["mean"], 4), " |"),
        paste0("| Median | ", round(original_stats["median"], 4), " | ", round(transformed_stats["median"], 4), " | ", round(transformed_stats["median"] - original_stats["median"], 4), " |"),
        paste0("| Std Dev | ", round(original_stats["sd"], 4), " | ", round(transformed_stats["sd"], 4), " | ", round(transformed_stats["sd"] - original_stats["sd"], 4), " |"),
        paste0("| Skewness | ", round(original_stats["skewness"], 4), " | ", round(transformed_stats["skewness"], 4), " | ", round(transformed_stats["skewness"] - original_stats["skewness"], 4), " |"),
        paste0("| Kurtosis | ", round(original_stats["kurtosis"], 4), " | ", round(transformed_stats["kurtosis"], 4), " | ", round(transformed_stats["kurtosis"] - original_stats["kurtosis"], 4), " |")
      )
      
      create_pdf_report(content_list, file, paste("Laporan Transformasi -", values$transformed_data$variable))
      
      # Clean up temp files
      file.remove(temp_before, temp_after)
    }
  )
  output$download_desc_histogram <- downloadHandler(
    filename = function() paste0("deskriptif_histogram_", input$desc_var, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(input$desc_var)
      var_data <- data_raw[[input$desc_var]]
      p <- ggplot(data.frame(x = var_data), aes(x = x)) +
        geom_histogram(aes(y = ..density..), bins = 30, fill = "#5e4fa2", alpha = 0.7, color = "white") +
        geom_density(color = "#fd7e14", size = 1.2) +
        geom_vline(aes(xintercept = mean(x, na.rm = TRUE)), color = "#28a745", linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = median(x, na.rm = TRUE)), color = "#dc3545", linetype = "dotted", size = 1) +
        labs(title = paste("Histogram dan Kurva Densitas:", input$desc_var),
             x = input$desc_var, y = "Densitas") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, color = "#3b0f70", size = 14, face = "bold"))
      save_ggplot(p, file)
    }
  )
  
  output$download_desc_boxplot <- downloadHandler(
    filename = function() paste0("deskriptif_boxplot_", input$desc_var, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(input$desc_var)
      var_data <- data_raw[[input$desc_var]]
      p <- ggplot(data.frame(x = var_data), aes(y = x)) +
        geom_boxplot(fill = "#fd7e14", alpha = 0.7, color = "#e67e22", outlier.color = "#dc3545", outlier.size = 2) +
        labs(title = paste("Box Plot dengan Outliers:", input$desc_var),
             y = input$desc_var) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, color = "#fd7e14", size = 14, face = "bold"))
      save_ggplot(p, file)
    }
  )
  
  output$download_desc_stats <- downloadHandler(
    filename = function() paste0("deskriptif_statistik_", input$desc_var, "_", Sys.Date(), ".csv"),
    content = function(file) {
      req(input$desc_var)
      var_data <- data_raw[[input$desc_var]]
      var_data <- var_data[!is.na(var_data)]
      
      stats_table <- data.frame(
        Statistik = c("N", "Mean", "Median", "Modus", "Std Deviasi", "Varians", "Minimum", "Maksimum", 
                      "Q1", "Q3", "IQR", "Range", "Skewness", "Kurtosis", "CV (%)"),
        Nilai = c(
          length(var_data),
          round(mean(var_data), 4),
          round(median(var_data), 4),
          round(as.numeric(names(sort(table(var_data), decreasing = TRUE))[1]), 4),
          round(sd(var_data), 4),
          round(var(var_data), 4),
          round(min(var_data), 4),
          round(max(var_data), 4),
          round(quantile(var_data, 0.25), 4),
          round(quantile(var_data, 0.75), 4),
          round(IQR(var_data), 4),
          round(max(var_data) - min(var_data), 4),
          round(skewness(var_data), 4),
          round(kurtosis(var_data), 4),
          round(sd(var_data) / mean(var_data) * 100, 2)
        )
      )
      write.csv(stats_table, file, row.names = FALSE)
    }
  )
  
  output$download_desc_numeric_pdf <- downloadHandler(
    filename = function() paste0("laporan_deskriptif_numerik_", input$desc_var, "_", Sys.Date(), ".pdf"),
    content = function(file) {
      req(input$desc_var)
      
      # Save plots temporarily
      temp_hist <- tempfile(fileext = ".png")
      temp_box <- tempfile(fileext = ".png")
      
      var_data <- data_raw[[input$desc_var]]
      
      # Create histogram
      p1 <- ggplot(data.frame(x = var_data), aes(x = x)) +
        geom_histogram(aes(y = ..density..), bins = 30, fill = "#5e4fa2", alpha = 0.7, color = "white") +
        geom_density(color = "#fd7e14", size = 1.2) +
        geom_vline(aes(xintercept = mean(x, na.rm = TRUE)), color = "#28a745", linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = median(x, na.rm = TRUE)), color = "#dc3545", linetype = "dotted", size = 1) +
        labs(title = paste("Histogram dan Kurva Densitas:", input$desc_var),
             x = input$desc_var, y = "Densitas") +
        theme_minimal()
      save_ggplot(p1, temp_hist)
      
      # Create boxplot
      p2 <- ggplot(data.frame(x = var_data), aes(y = x)) +
        geom_boxplot(fill = "#fd7e14", alpha = 0.7, color = "#e67e22", outlier.color = "#dc3545", outlier.size = 2) +
        labs(title = paste("Box Plot dengan Outliers:", input$desc_var),
             y = input$desc_var) +
        theme_minimal()
      save_ggplot(p2, temp_box)
      
      # Create statistics
      var_data_clean <- var_data[!is.na(var_data)]
      stats <- list(
        n = length(var_data_clean),
        mean = mean(var_data_clean),
        median = median(var_data_clean),
        sd = sd(var_data_clean),
        min = min(var_data_clean),
        max = max(var_data_clean),
        q1 = quantile(var_data_clean, 0.25),
        q3 = quantile(var_data_clean, 0.75),
        skewness = skewness(var_data_clean),
        kurtosis = kurtosis(var_data_clean)
      )
      
      content_list <- c(
        paste("# Laporan Analisis Deskriptif Numerik -", input$desc_var),
        "",
        "## Histogram dan Kurva Densitas",
        paste0("![Histogram](", temp_hist, ")"),
        "",
        "## Box Plot dengan Outliers", 
        paste0("![Box Plot](", temp_box, ")"),
        "",
        "## Statistik Deskriptif",
        "",
        "| Statistik | Nilai |",
        "|-----------|-------|",
        paste0("| N | ", stats$n, " |"),
        paste0("| Mean | ", round(stats$mean, 4), " |"),
        paste0("| Median | ", round(stats$median, 4), " |"),
        paste0("| Std Deviasi | ", round(stats$sd, 4), " |"),
        paste0("| Minimum | ", round(stats$min, 4), " |"),
        paste0("| Maksimum | ", round(stats$max, 4), " |"),
        paste0("| Q1 | ", round(stats$q1, 4), " |"),
        paste0("| Q3 | ", round(stats$q3, 4), " |"),
        paste0("| Skewness | ", round(stats$skewness, 4), " |"),
        paste0("| Kurtosis | ", round(stats$kurtosis, 4), " |")
      )
      
      create_pdf_report(content_list, file, paste("Laporan Deskriptif Numerik -", input$desc_var))
      
      # Clean up temp files
      file.remove(temp_hist, temp_box)
    }
  )
  output$download_desc_bar <- downloadHandler(
    filename = function() paste0("deskriptif_barchart_", input$desc_cat_var, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(input$desc_cat_var)
      var_data <- data_raw[[input$desc_cat_var]]
      freq_data <- as.data.frame(table(var_data))
      names(freq_data) <- c("Kategori", "Frekuensi")
      colors <- c("#3b0f70", "#5e4fa2", "#9970ab", "#ffff99", "#4a90e2", "#28a745", "#fd7e14", "#dc3545")
      
      p <- ggplot(freq_data, aes(x = reorder(Kategori, -Frekuensi), y = Frekuensi)) +
        geom_col(fill = colors[1:nrow(freq_data)], alpha = 0.8) +
        geom_text(aes(label = Frekuensi), vjust = -0.5, color = "#3b0f70", fontface = "bold") +
        labs(title = paste("Distribusi Frekuensi:", input$desc_cat_var),
             x = input$desc_cat_var, y = "Frekuensi") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, color = "#3b0f70", size = 14, face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1))
      save_ggplot(p, file)
    }
  )
  
  output$download_desc_pie <- downloadHandler(
    filename = function() paste0("deskriptif_piechart_", input$desc_cat_var, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(input$desc_cat_var)
      var_data <- data_raw[[input$desc_cat_var]]
      freq_data <- as.data.frame(table(var_data))
      names(freq_data) <- c("Kategori", "Frekuensi")
      
      # Create pie chart using ggplot
      p <- ggplot(freq_data, aes(x = "", y = Frekuensi, fill = Kategori)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = paste("Distribusi Proporsi:", input$desc_cat_var)) +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5, color = "#3b0f70", size = 14, face = "bold")) +
        scale_fill_manual(values = c("#3b0f70", "#5e4fa2", "#9970ab", "#ffff99", "#4a90e2", "#28a745", "#fd7e14", "#dc3545"))
      save_ggplot(p, file)
    }
  )
  
  output$download_desc_freq <- downloadHandler(
    filename = function() paste0("deskriptif_frekuensi_", input$desc_cat_var, "_", Sys.Date(), ".csv"),
    content = function(file) {
      req(input$desc_cat_var)
      var_data <- data_raw[[input$desc_cat_var]]
      freq_table <- table(var_data, useNA = "ifany")
      prop_table <- prop.table(freq_table) * 100
      
      result_table <- data.frame(
        Kategori = names(freq_table),
        Frekuensi = as.numeric(freq_table),
        Persentase = round(as.numeric(prop_table), 2),
        Persentase_Kumulatif = round(cumsum(as.numeric(prop_table)), 2)
      )
      write.csv(result_table, file, row.names = FALSE)
    }
  )
  
  output$download_desc_cat_pdf <- downloadHandler(
    filename = function() paste0("laporan_deskriptif_kategorik_", input$desc_cat_var, "_", Sys.Date(), ".pdf"),
    content = function(file) {
      req(input$desc_cat_var)
      
      # Save plots temporarily
      temp_bar <- tempfile(fileext = ".png")
      temp_pie <- tempfile(fileext = ".png")
      
      var_data <- data_raw[[input$desc_cat_var]]
      freq_data <- as.data.frame(table(var_data))
      names(freq_data) <- c("Kategori", "Frekuensi")
      colors <- c("#3b0f70", "#5e4fa2", "#9970ab", "#ffff99", "#4a90e2", "#28a745", "#fd7e14", "#dc3545")
      
      # Create bar chart
      p1 <- ggplot(freq_data, aes(x = reorder(Kategori, -Frekuensi), y = Frekuensi)) +
        geom_col(fill = colors[1:nrow(freq_data)], alpha = 0.8) +
        geom_text(aes(label = Frekuensi), vjust = -0.5, color = "#3b0f70", fontface = "bold") +
        labs(title = paste("Distribusi Frekuensi:", input$desc_cat_var),
             x = input$desc_cat_var, y = "Frekuensi") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      save_ggplot(p1, temp_bar)
      
      # Create pie chart
      p2 <- ggplot(freq_data, aes(x = "", y = Frekuensi, fill = Kategori)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = paste("Distribusi Proporsi:", input$desc_cat_var)) +
        theme_void() +
        scale_fill_manual(values = colors)
      save_ggplot(p2, temp_pie)
      
      # Create frequency table
      freq_table <- table(var_data, useNA = "ifany")
      prop_table <- prop.table(freq_table) * 100
      
      content_list <- c(
        paste("# Laporan Analisis Deskriptif Kategorik -", input$desc_cat_var),
        "",
        "## Bar Chart Distribusi Frekuensi",
        paste0("![Bar Chart](", temp_bar, ")"),
        "",
        "## Pie Chart Distribusi Proporsi", 
        paste0("![Pie Chart](", temp_pie, ")"),
        "",
        "## Tabel Frekuensi",
        "",
        "| Kategori | Frekuensi | Persentase | Persentase Kumulatif |",
        "|----------|-----------|------------|---------------------|",
        paste0("| ", names(freq_table), " | ", freq_table, " | ", round(prop_table, 2), "% | ", round(cumsum(prop_table), 2), "% |", collapse = "\n"),
        "",
        "## Ringkasan",
        paste("- **Total Kategori:** ", length(freq_table)),
        paste("- **Kategori Terbanyak:** ", names(which.max(freq_table)), " (", max(freq_table), " observasi)"),
        paste("- **Kategori Tersedikit:** ", names(which.min(freq_table)), " (", min(freq_table), " observasi)")
      )
      
      create_pdf_report(content_list, file, paste("Laporan Deskriptif Kategorik -", input$desc_cat_var))
      
      # Clean up temp files
      file.remove(temp_bar, temp_pie)
    }
  )
  
}

shinyApp(ui, server)

