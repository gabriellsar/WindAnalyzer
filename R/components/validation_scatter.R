validationScatterUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        tags$h4("Validation: Historical vs. Estimated Power"),
        # Botão de exportação em alta resolução
        downloadButton(ns("download_val_scatter"), "Exportar Gráfico", class = "btn-sm")
    ),
    shinycssloaders::withSpinner(
      # Mudança de plotlyOutput para plotOutput
      plotOutput(ns("validationScatter"), height = "400px"),
      type = 4,
      color = "#286090"
    )
  )
}

validationScatterServer <- function(id, dados_comparacao) {
  moduleServer(id, function(input, output, session) {
    
    # Construção do objeto gráfico de forma reativa
    plot_obj <- reactive({
      df <- dados_comparacao()
      req(df)
      validate(need(nrow(df) > 0, "No data available for validation."))
      
      # Ordena para garantir que os dados "Estimated" fiquem por cima dos "Historical"
      df$origin <- factor(df$origin, levels = c("Historical", "Estimated"))
      df <- df[order(df$origin), ]
      
      ggplot2::ggplot(df, ggplot2::aes(x = speed, y = power, color = origin, alpha = origin)) +
        ggplot2::geom_point(size = 0.8) +
        ggplot2::labs(
          x = "Wind Speed (m/s)", 
          y = "Power (kW)",
          color = "Source"
        ) +
        ggplot2::scale_color_manual(values = c("Historical" = "gray70", "Estimated" = "#059669")) +
        ggplot2::scale_alpha_manual(values = c("Historical" = 0.1, "Estimated" = 0.8)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "bottom",
        ) + 
        ggplot2::guides(
          color = ggplot2::guide_legend(override.aes = list(size = 4, alpha = 1)),
          alpha = "none"
        )
    })
    output$validationScatter <- renderPlot({ plot_obj() }, res = 96)
    output$download_val_scatter <- downloadHandler(
      filename = function() { paste0("validacao_historico_vs_estimado_", Sys.Date(), ".png") },
      content = function(file) { 
        ggplot2::ggsave(file, plot = plot_obj(), width = 10, height = 7, dpi = 300) 
      }
    )
  })
}