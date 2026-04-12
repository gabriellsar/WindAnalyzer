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

projectionSeriesServer <- function(id, dados_projecao) {
  moduleServer(id, function(input, output, session) {
    
    output$projectionChart <- plotly::renderPlotly({
      df <- dados_projecao() # Recebe o data.frame consolidado do passo anterior
      req(df)
      
      p <- plotly::plot_ly(data = df, x = ~data) %>%
        # Adiciona a "nuvem" dos cenários estocásticos
        plotly::add_ribbons(
          ymin = ~Lower, ymax = ~Upper,
          name = 'Intervalo 90% (Cenários)',
          fillcolor = 'rgba(169, 169, 169, 0.3)',
          line = list(color = 'transparent'),
          hoverinfo = "skip"
        ) %>%
        # Adiciona a linha da Média
        plotly::add_lines(
          y = ~Mean,
          name = 'Média',
          line = list(color = 'black', width = 2),
          hoverinfo = "text",
          text = ~paste("Data:", data, "<br>Média:", round(Mean, 2), "kW")
        ) %>%
        plotly::layout(
          xaxis = list(title = "Time"),
          yaxis = list(title = "Power (kW)"),
          title = "Simulated Scenarios vs. Mean Estimate",
          showlegend = TRUE,
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)
        ) %>%
        plotly::config(displaylogo = FALSE, modeBarButtons = list(list("toImage")))
      
      return(p)
    })
  })
}