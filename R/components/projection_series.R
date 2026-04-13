projectionSeriesUI <- function(id) {
  ns <- NS(id)
  tags$div(class = "chart-card",
           tags$div(class = "chart-header",
                    tags$h4("Projected Wind Power Time Series (Simulated Scenarios vs. Mean Estimate)", class = "chart-title"),
                    downloadButton(ns("download_projection"), "Export", class = "btn-download")
           ),
           
           tags$div(class = "chart-body",
                    shinycssloaders::withSpinner(
                      plotOutput(ns("projectionChart"), height = "400px"),
                      type = 4, 
                      color = "#16a34a"
                    )
           )
  )
}

projectionSeriesServer <- function(id, dados_projecao) {
  moduleServer(id, function(input, output, session) {
    plot_obj <- reactive({
      df <- dados_projecao()
      req(df)
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = data)) +
        ggplot2::geom_ribbon(
          ggplot2::aes(ymin = Lower, ymax = Upper, fill = "Scenarios"),
          alpha = 0.5
        ) +
        ggplot2::geom_line(
          ggplot2::aes(y = Mean, color = "Mean"), 
          linewidth = 0.8
        ) +
        ggplot2::scale_fill_manual(values = c("Scenarios" = "darkgray")) +
        ggplot2::scale_color_manual(values = c("Mean" = "black")) +
        ggplot2::labs(
          x = "Time",
          y = "Power (kW)",
          fill = NULL,
          color = NULL
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")
      
      return(p)
    })
    
    output$projectionChart <- renderPlot({ 
      plot_obj() 
    }, res = 96)
    output$download_projection <- downloadHandler(
      filename = function() { 
        paste0("projecao_potencia_", Sys.Date(), ".png") 
      },
      content = function(file) { 
        ggplot2::ggsave(file, plot = plot_obj(), width = 10, height = 6, dpi = 300) 
      }
    )
    
  })
}