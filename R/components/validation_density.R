validationDensityUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h4("Density Distribution Comparison"),
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(ns("validationDensity")),
      type = 4,
      color = "#286090"
    )
  )
}

validationDensityServer <- function(id, dados_comparacao) {
  moduleServer(id, function(input, output, session) {
    
    output$validationDensity <- plotly::renderPlotly({
      df <- dados_comparacao()
      req(df)
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = power, color = origin)) +
        ggplot2::geom_density(size = 1) + 
        ggplot2::labs(
          x = "Power (kW)", 
          y = "Density",
          color = "Source"
        ) +
        ggplot2::scale_color_manual(values = c("Historical" = "blue", "Estimated" = "black")) +
        ggplot2::theme_minimal()
      
      plotly::ggplotly(p) %>%
        plotly::layout(legend = list(orientation = "h", y = -0.2))
    })
  })
}