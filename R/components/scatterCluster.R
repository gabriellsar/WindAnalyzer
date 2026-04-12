scatterplotUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        tags$h4("Cluster Distribution (Wind Speed vs. Power)"),
        downloadButton(ns("download_scatter_cluster"), "Exportar Gráfico", class = "btn-sm")
    ),
    shinycssloaders::withSpinner(
      plotOutput(ns("cluster_scatterplot"), height = "400px"),
      type = 4,
      color = "#286090"
    )
  )
}

scatterplotServer <- function(id, dados_para_plotar) {
  moduleServer(id, function(input, output, session) {
    
    plot_obj <- reactive({
      plot_data <- dados_para_plotar()
      req(plot_data)
      validate(need(nrow(plot_data) > 0, "Não há dados para exibir no gráfico. Execute a análise primeiro."))
      
      ggplot2::ggplot(plot_data, ggplot2::aes(x = speed, y = power, color = cluster)) +
        ggplot2::geom_point(alpha = 0.6, size = 1.5) +
        ggplot2::labs(x = "Wind Speed (m/s)", y = "Power (kW)", color = "Cluster") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "right")
    })
    
    output$cluster_scatterplot <- renderPlot({ plot_obj() }, res = 96)
    
    output$download_scatter_cluster <- downloadHandler(
      filename = function() { paste0("clusters_dispersao_", Sys.Date(), ".png") },
      content = function(file) { ggplot2::ggsave(file, plot = plot_obj(), width = 8, height = 6, dpi = 300) }
    )
  })
}