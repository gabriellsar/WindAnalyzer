projectionSeriesUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h4("Projected Wind Power Time Series"),
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(ns("projectionChart")),
      type = 4,
      color = "#286090"
    )
  )
}

projectionSeriesServer <- function(id, dados_projecao_long) {
  moduleServer(id, function(input, output, session) {
    
    output$projectionChart <- plotly::renderPlotly({
      df <- dados_projecao_long()
      req(df)
      df$type <- factor(df$type, levels = c("Scenario", "Mean"))
      df <- df[order(df$type), ] 
      p <- ggplot2::ggplot(df, ggplot2::aes(
        x = data, 
        y = value, 
        group = variable, 
        color = type,
        alpha = type,
        linewidth = type
      )) +
        ggplot2::geom_line() +
        ggplot2::scale_color_manual(values = c("Scenario" = "gray70", "Mean" = "black")) +
        ggplot2::scale_alpha_manual(values = c("Scenario" = 0.3, "Mean" = 1)) +
        ggplot2::scale_linewidth_manual(values = c("Scenario" = 0.5, "Mean" = 1.2)) +
        ggplot2::labs(
          x = "Time",
          y = "Power (kW)",
          title = "Simulated Scenarios vs. Mean Estimate"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none")
      
      gg <- plotly::ggplotly(p, tooltip = c("x", "y", "type"))
      
      gg$x$data <- lapply(gg$x$data, function(trace) {
      if (isTRUE(grepl("Scenario", trace$name)) || isTRUE(grepl("Scenario", trace$legendgroup))) {
        trace$hoverinfo <- "skip" 
        } else {
          trace$hoverinfo <- "text" 
        }
        return(trace)
      })
      
      gg <- plotly::config(gg, 
                           displaylogo = FALSE,
                           modeBarButtons = list(list("toImage")))
      return(gg)
    })
  })
}