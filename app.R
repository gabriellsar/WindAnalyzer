library(shiny)                
library(leaflet) 
library(shinyjs)
library(feather)
library(ggplot2)

source("config_token.R")    

tokens <- list(   
  refresh = refresh_token,
  client_id = client_id,
  client_secret = client_secret
)

lonlat <- as.data.frame(read_feather("lonlat_modificado.feather"))
load("Base_estacoes.RData")
load("Base_Dados_Estacoes.RData")


module_files <- list.files("R/modules", pattern = "\\.R$", full.names = TRUE)
sapply(module_files, source) 

component_files <- list.files("R/components", pattern = "\\.R$", full.names = TRUE)   
sapply(component_files, source)   

component_files <- list.files("R/utils", pattern = "\\.R$", full.names = TRUE)   
sapply(component_files, source) 
           
ui <- fluidPage(     
  lang = "en",
  tags$script(src = "particles.js"),
  tags$script(src = "mapcontainer.js"),          
   
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$title("WindAnalyzer - Wind Energy Analysis"),
    
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css"), 
    tags$link(rel = "stylesheet", type = "text/css", href = "analysis.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "map_input.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "charts.css"),
    
    tags$style(HTML("
      .nav-tabs {
        display: none;
      }
    "))
  ),
  
  navBarUI("nav_bar"),
                                         
  tabsetPanel(
    id = "main_tabs",  
    type = "hidden", 
     
    tabPanel("Home", value = "home_tab", 
      heroSectionUI("hero"),
      cardsSectionUI("cards"),
      tags$div(class = "wind-particles"),  
    ),
    
    tabPanel("Analysis", value = "analysis_tab",         
      analysisUI(id = "analysis")   
    ),
     
    tabPanel("About", value = "about_tab",
      aboutUI("about")
    ), 
  )
)

server <- function(input, output, session) {
  active_tab <- reactiveVal("home_tab")
  
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
     
  observeEvent(nav_clicks$home(), {
    active_tab("home_tab")
    updateTabsetPanel(session, "main_tabs", selected = "home_tab")
  })
  
  observeEvent(nav_clicks$analyse(), {
    active_tab("analysis_tab")
    updateTabsetPanel(session, "main_tabs", selected = "analysis_tab")
  })
  
  observeEvent(cta_button_click(), {
    req(cta_button_click() > 0)
    active_tab("analysis_tab")
    updateTabsetPanel(session, "main_tabs", selected = "analysis_tab")
  })
  
  observeEvent(nav_clicks$about(), {
    active_tab("about_tab")
    updateTabsetPanel(session, "main_tabs", selected = "about_tab")
  })
}

shinyApp(ui, server)