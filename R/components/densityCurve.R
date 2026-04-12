densityPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        tags$h4("Power Density Graph by Cluster", style = "margin-top:10px; margin-bottom:0;"),
        downloadButton(ns("download_density"), "Exportar Gráfico", class = "btn-sm")
    ),
    
    div(style = "display: flex; align-items: center; gap: 30px; margin-bottom: 10px;",
        radioButtons(ns("plot_type"), "Tipo de Visualização:",
                     choices = c("Sobreposto" = "overlay", "Grade" = "facet"),
                     selected = "facet", inline = TRUE),
        
        conditionalPanel(
          condition = "input.plot_type == 'overlay'", ns = ns,
          div(style = "display: flex; align-items: center; gap: 10px;",
              tags$b("Página:"),
              numericInput(ns("page_num"), label = NULL, value = 1, min = 1, step = 1, width = "80px")
          )
        )
    ),
    
    shinycssloaders::withSpinner(
      plotOutput(ns("densityChart"), height = "400px"),
      type = 4,
      color = "#286090"
    )
  )
}

densityPlotServer <- function(id, dados_para_plotar) {
  moduleServer(id, function(input, output, session) {
    dados_paginados <- reactive({
      plot_data <- dados_para_plotar()
      req(plot_data)
      clusters_por_pagina <- 3
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