# WindAnalyzer — ponto de entrada da aplicacao Shiny.
# Carrega dependencias, bases estaticas e os arquivos de R/ (utils -> components -> modules),
# e monta a interface com tres abas: Home, Analysis e About.

library(shiny)
library(leaflet)
library(shinyjs)
library(feather)
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(plotly)

# --- Codigo-fonte ------------------------------------------------------------
source_dir <- function(path) {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  invisible(lapply(files, source))
}

source_dir("R/utils")       # algoritmos: clusterizacao, KDE, Monte Carlo, MERRA-2, Dropbox
source_dir("R/components")  # elementos visuais reutilizaveis (graficos, upload, banner)
source_dir("R/modules")     # paginas e macrofuncionalidades (Shiny modules)

# --- Credenciais e dados estaticos -------------------------------------------
tokens <- load_dropbox_credentials()

lonlat <- as.data.frame(read_feather(file.path("data", "lonlat_modificado.feather")))
load(file.path("data", "Base_estacoes.RData"))        # -> Base_estacoes (inventario INMET)
load(file.path("data", "Base_Dados_Estacoes.RData"))  # -> Base_Dados_Estacoes (series INMET)

# --- Interface ---------------------------------------------------------------
ui <- fluidPage(
  lang = "en",
  tags$script(src = "mapcontainer.js"),

  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$title("WindAnalyzer - Wind Energy Analysis"),

    tags$link(rel = "stylesheet", type = "text/css", href = "main.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "analysis.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "map_input.css"),

    tags$style(HTML(".nav-tabs { display: none; }"))
  ),

  navBarUI("nav_bar"),

  tabsetPanel(
    id = "main_tabs",
    type = "hidden",

    tabPanel("Home", value = "home_tab",
      heroSectionUI("hero"),
      cardsSectionUI("cards"),
      tags$div(class = "wind-particles")
    ),

    tabPanel("Analysis", value = "analysis_tab",
      analysisUI(id = "analysis")
    ),

    tabPanel("About", value = "about_tab",
      aboutUI("about")
    )
  )
)

# --- Servidor ----------------------------------------------------------------
server <- function(input, output, session) {
  active_tab <- reactiveVal("home_tab")

  go_to <- function(tab) {
    active_tab(tab)
    updateTabsetPanel(session, "main_tabs", selected = tab)
  }

  nav_clicks <- navBarServer("nav_bar", active_tab = active_tab)
  cta_button_click <- heroSectionServer("hero")
  cardsSectionServer("cards")

  analysisServer(
    id = "analysis",
    lonlat_data = lonlat,
    estacoes_data = Base_estacoes,
    dados_estacoes_data = Base_Dados_Estacoes,
    tokens = tokens
  )

  aboutServer("about")

  observeEvent(nav_clicks$home(),    go_to("home_tab"))
  observeEvent(nav_clicks$analyse(), go_to("analysis_tab"))
  observeEvent(nav_clicks$about(),   go_to("about_tab"))
  observeEvent(cta_button_click(), {
    req(cta_button_click() > 0)
    go_to("analysis_tab")
  })
}

shinyApp(ui, server)
