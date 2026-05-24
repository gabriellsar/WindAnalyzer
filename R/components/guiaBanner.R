guiaBannerUI <- function(id) {
  ns <- NS(id)
  div(
    id = ns("banner_container"),
    class = "banner-container",
    
    div(
      class = "banner-content-wrapper",
      icon("circle-info", class = "banner-icon"),
      div(
        class = "banner-text-wrapper",
        tags$strong("First time using WindAnalyzer?", class = "banner-title"),
        tags$span("Download our quick guide to understand how to set up your data and generate your analyses", class = "banner-subtitle")
      )
    ),
    
    div(
      class = "banner-actions",
      downloadButton(ns("download_guia"), "Download Guide", class = "btn-banner-download"),
      actionButton(ns("fechar_banner"), label = NULL, icon = icon("xmark"), class = "btn-banner-close")
    )
  )
}

guiaBannerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Lógica para ocultar o banner ao clicar no X
    observeEvent(input$fechar_banner, {
      shinyjs::hide("banner_container", anim = TRUE, animType = "fade")
    })
    
    # Lógica de download do PDF
    output$download_guia <- downloadHandler(
      filename = function() {
        "WindAnalyzer_Guide.pdf" 
      },
      content = function(file) {
        caminho_arquivo <- "www/WindAnalyzer_Guide.pdf"
        
        if(file.exists(caminho_arquivo)) {
          file.copy(caminho_arquivo, file)
        } else {
          writeLines("O arquivo PDF do guia ainda não foi adicionado à pasta www.", file)
        }
      }
    )
  })
}