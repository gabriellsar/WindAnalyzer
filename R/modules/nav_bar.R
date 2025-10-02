navBarUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("container"))
}

navBarServer <- function(id, active_tab = reactive("home_tab")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$container <- renderUI({
      
      current_tab <- active_tab()
      
      tags$nav(class = "navbar",
               
               tags$div(class = "nav-items",
                        tags$img(src = "./images/puclogo.png", class = "nav-logo", width = "100px"),
                        
                        tags$div(class = "nav-links",
                                 actionLink(
                                   inputId = ns("nav_home"),
                                   label = "Home",
                                   class = paste("nav-item", ifelse(current_tab == "home_tab", "highlight", ""))
                                 ),
                                 actionLink(
                                   inputId = ns("nav_analyse"),
                                   label = "Analyse",
                                   class = paste("nav-item", ifelse(current_tab == "analysis_tab", "highlight", ""))
                                 ),
                                 actionLink(
                                   inputId = ns("nav_about"),
                                   label = "About",
                                   class = paste("nav-item", ifelse(current_tab == "about_tab", "highlight", ""))
                                 )
                        ),
                        
                        tags$img(src = "./images/frog_puc_logo.png", class = "nav-logo", width="100px")
                      
               )
      )
    })
    
    return(
      list(
        home = reactive(input$nav_home),
        analyse = reactive(input$nav_analyse),
        about = reactive(input$nav_about)
      )
    )
  })
}