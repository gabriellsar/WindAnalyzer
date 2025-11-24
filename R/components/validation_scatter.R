validationScatterUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h4("Validation: Historical vs. Estimated Power"),
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(ns("validationScatter")),
      type = 4,
      color = "#286090"
    )
  )
}

validationScatterServer <- function(id, dados_comparacao) {
  moduleServer(id, function(input, output, session) {
    
    output$validationScatter <- plotly::renderPlotly({
      df <- dados_comparacao()
      req(df)
      validate(need(nrow(df) > 0, "No data available for validation."))
      
      df$origin <- factor(df$origin, levels = c("Historical", "Estimated"))
      df <- df[order(df$origin), ]
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = speed, y = power, color = origin, alpha = origin)) +
        ggplot2::geom_point(size = 1.5) +
        ggplot2::labs(
          x = "Wind Speed (m/s)", 
          y = "Power (kW)",
          color = "Source"
        ) +
        ggplot2::scale_color_manual(values = c("Historical" = "gray70", "Estimated" = "#059669")) +
        ggplot2::scale_alpha_manual(values = c("Historical" = 0.3, "Estimated" = 1.0)) +
        ggplot2::theme_minimal()
      
      plotly::ggplotly(p) %>%
        plotly::style(hoverinfo = "skip") %>%
        plotly::layout(legend = list(orientation = "h", y = -0.2)) %>%
        plotly::config(
          displaylogo = FALSE,
          modeBarButtons = list(list("toImage"))
        )
    })
  })
}