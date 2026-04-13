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
                                          choices = c("Overlay" = "overlay", "Grid" = "facet"),
                                          selected = "facet", inline = TRUE)
                    ),
                    
                    conditionalPanel(
                      condition = "input.plot_type == 'overlay'", ns = ns,
                      tags$div(class = "pagination-group",
                               tags$span("Cluster:", class = "control-label"),
                               numericInput(ns("page_num"), label = NULL, value = 1, min = 1, step = 1, width = "65px")
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
    
    dados_paginados <- reactive({
      plot_data <- dados_para_plotar()
      req(plot_data)
      clusters_por_pagina <- 1
      
      cluster_levels <- levels(plot_data$cluster)
      total_clusters <- length(cluster_levels)
      total_paginas <- ceiling(total_clusters / clusters_por_pagina)
      
      req(input$page_num > 0, input$page_num <= total_paginas)
      start_index <- (input$page_num - 1) * clusters_por_pagina + 1
      end_index <- min(input$page_num * clusters_por_pagina, total_clusters)
      
      clusters_na_pagina <- cluster_levels[start_index:end_index]
      plot_data %>% dplyr::filter(cluster %in% clusters_na_pagina)
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
        ggplot2::ggplot(dados_paginados(), ggplot2::aes(x = power, fill = cluster, color = cluster)) +
          ggplot2::geom_density(alpha = 0.5) +
          ggplot2::labs(x = "Power (kW)", y = "Density", fill = "Cluster", color = "Cluster") +
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