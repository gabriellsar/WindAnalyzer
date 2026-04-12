validationDensityUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        tags$h4("Density Distribution Comparison"),
        downloadButton(ns("download_val_density"), "Exportar Gráfico", class = "btn-sm")
    ),
    shinycssloaders::withSpinner(
      plotOutput(ns("validationDensity"), height = "400px"),
      type = 4,
      color = "#286090"
    )
  )
}

validationDensityServer <- function(id, dados_comparacao) {
  moduleServer(id, function(input, output, session) {
    
    plot_obj <- reactive({
      df <- dados_comparacao()
      req(df)
      
      ggplot2::ggplot(df, ggplot2::aes(x = power, color = origin, fill = origin)) +
        # Uso do alpha no fill para preenchimento transparente, fica muito mais estético
        ggplot2::geom_density(linewidth = 1, alpha = 0.2) + 
        ggplot2::labs(
          x = "Power (kW)", 
          y = "Density",
          color = "Source",
          fill = "Source"
        ) +
        ggplot2::scale_color_manual(values = c("Historical" = "#3b82f6", "Estimated" = "black")) +
        ggplot2::scale_fill_manual(values = c("Historical" = "#3b82f6", "Estimated" = "black")) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")
    })
    
    output$validationDensity <- renderPlot({ plot_obj() }, res = 96)
    
    output$download_val_density <- downloadHandler(
      filename = function() { paste0("validacao_densidade_", Sys.Date(), ".png") },
      content = function(file) { ggplot2::ggsave(file, plot = plot_obj(), width = 8, height = 6, dpi = 300) }
    )
  })
}