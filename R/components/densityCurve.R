densityPlotUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$h4("Gráfico de Densidade da Potência por Cluster"),
    
    div(style = "display: flex; align-items: center; gap: 30px; margin-bottom: 10px;",
        radioButtons(ns("plot_type"), "Tipo de Visualização:",
                     choices = c("Sobreposto" = "overlay", "Grade" = "facet"),
                     selected = "facet", inline = TRUE),
        
        conditionalPanel(
          condition = "input.plot_type == 'overlay'",
          ns = ns,
          div(style = "display: flex; align-items: center; gap: 10px;",
              tags$b("Página:"),
              numericInput(ns("page_num"), label = NULL, value = 1, min = 1, step = 1, width = "80px")
          )
        )
    ),
    
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(ns("densityChart")),
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
      
      plot_data %>% 
        dplyr::filter(cluster %in% clusters_na_pagina)
    })
    
    output$densityChart <- plotly::renderPlotly({
      
      req(dados_para_plotar())
      
      if (input$plot_type == "facet") {
        plot_data <- dados_para_plotar()
        
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = power, fill = cluster, color = cluster)) +
          ggplot2::geom_density(alpha = 0.5) +
          ggplot2::facet_wrap(~ cluster, ncol = 4, scales = "free_y") +
          ggplot2::labs(title = "Distribuição de Potência por Cluster (Visualização em Grade)", x = "Potência (kW)", y = "Densidade") +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "none")
        
      } else {
        plot_data <- dados_paginados()
        
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = power, fill = cluster, color = cluster)) +
          ggplot2::geom_density(alpha = 0.5) +
          ggplot2::labs(
            title = paste0("Distribuição de Potência por Cluster (Página ", input$page_num, ")"),
            x = "Potência (kW)",
            y = "Densidade",
            fill = "Cluster",
            color = "Cluster"
          ) +
          ggplot2::theme_minimal()
      }
      
      plotly::ggplotly(p)
    })
    
  })
}