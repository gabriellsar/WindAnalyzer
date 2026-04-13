densityPlotUI <- function(id) {
  ns <- NS(id)
  tags$div(class = "chart-card",
           tags$div(class = "chart-header",
                    tags$h4("Power Density Graph by Cluster", class = "chart-title"),
                    downloadButton(ns("download_density"), "Export", class = "btn-download")
           ),
           
           tags$div(class = "chart-controls",
                    
                    tags$div(class = "toggle-group",
                             tags$span("View Type:", class = "control-label"),
                             radioButtons(ns("plot_type"), label = NULL,
                                          choices = c("Single" = "single", "Grid" = "facet"),
                                          selected = "facet", inline = TRUE)
                    ),
                    
                    conditionalPanel(
                      condition = "input.plot_type == 'single'", ns = ns,
                      tags$div(class = "pagination-group", style = "display: flex; align-items: center; gap: 8px;",
                               tags$span("Cluster:", class = "control-label"),
                               numericInput(ns("page_num"), label = NULL, value = 1, min = 1, step = 1, width = "65px"),
                               uiOutput(ns("warning_msg"), inline = TRUE)
                      )
                    )
           ),
           
           tags$div(class = "chart-body",
                    shinycssloaders::withSpinner(
                      plotOutput(ns("densityChart"), height = "400px"),
                      type = 4, color = "#16a34a"
                    )
           )
  )
}

densityPlotServer <- function(id, dados_para_plotar) {
  moduleServer(id, function(input, output, session) {
    
    total_clusters_reativo <- reactive({
      plot_data <- dados_para_plotar()
      req(plot_data)
      length(levels(plot_data$cluster))
    })
    
    observe({
      req(total_clusters_reativo())
      updateNumericInput(session, "page_num", max = total_clusters_reativo())
    })
    
    output$warning_msg <- renderUI({
      req(!is.null(input$page_num), !is.na(input$page_num)) 
      
      tot <- total_clusters_reativo()
      
      if (input$page_num < 1 || input$page_num > tot) {
        tags$span(
          style = "color: #dc2626; font-size: 0.85rem; display: inline-flex; align-items: center; gap: 4px; font-weight: 500;",
          shiny::icon("exclamation-triangle"),
          paste0("Out of bounds (Max: ", tot, ")")
        )
      } else {
        NULL 
      }
    })
    
    dados_paginados <- reactive({
      plot_data <- dados_para_plotar()
      req(plot_data)
      
      cluster_levels <- levels(plot_data$cluster)
      total_clusters <- length(cluster_levels)
      
      req(is.numeric(input$page_num), input$page_num >= 1, input$page_num <= total_clusters)
      
      cluster_selecionado <- cluster_levels[input$page_num]
      
      plot_data %>% dplyr::filter(cluster == cluster_selecionado)
    })
    
    plot_obj <- reactive({
      req(dados_para_plotar())
      if (input$plot_type == "facet") {
        ggplot2::ggplot(dados_para_plotar(), ggplot2::aes(x = power, fill = cluster, color = cluster)) +
          ggplot2::geom_density(alpha = 0.5) +
          ggplot2::facet_wrap(~ cluster, ncol = 4, scales = "free_y") +
          ggplot2::labs(x = "Power (kW)", y = "Density") +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "none")
      } else {
        req(nrow(dados_paginados()) > 0) 
        
        cluster_atual <- unique(dados_paginados()$cluster)
        
        ggplot2::ggplot(dados_paginados(), ggplot2::aes(x = power, fill = cluster, color = cluster)) +
          ggplot2::geom_density(alpha = 0.5) +
          ggplot2::labs(
            x = "Power (kW)", 
            y = "Density", 
            fill = "Cluster", 
            color = "Cluster"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "bottom")
      }
    })
    
    output$densityChart <- renderPlot({ plot_obj() }, res = 96)
    
    output$download_density <- downloadHandler(
      filename = function() { paste0("densidade_clusters_", Sys.Date(), ".png") },
      content = function(file) { ggplot2::ggsave(file, plot = plot_obj(), width = 10, height = 6, dpi = 300) }
    )
  })
}