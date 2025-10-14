scatterplotUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "chart-container",
      tags$h4("Gráfico de Dispersão dos Clusters"),
      plotlyOutput(ns("cluster_scatterplot"))
    )
  )
}

scatterplotServer <- function(id, dados_para_plotar) {
  moduleServer(id, function(input, output, session) {
    
    output$cluster_scatterplot <- renderPlotly({
      
      plot_data <- dados_para_plotar()
      req(plot_data)
      
      validate(
        need(nrow(plot_data) > 0, "Não há dados para exibir no gráfico de dispersão. Execute a análise primeiro.")
      )
      
      p <- plot_ly(
        data = plot_data,
        x = ~speed,
        y = ~power,
        color = ~cluster, # Colore os pontos pelo cluster
        type = 'scatter',
        mode = 'markers',
        marker = list(size = 5, opacity = 0.7)
      ) %>%
        layout(
          title = "Distribuição dos Clusters (Velocidade vs. Potência)",
          xaxis = list(title = "Velocidade do Vento (m/s)"),
          yaxis = list(title = "Potência (kW)"),
          legend = list(title = list(text = '<b>Cluster</b>'))
        )
      
      return(p)
    })
    
  })
}