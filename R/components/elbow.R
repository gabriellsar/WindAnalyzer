elbowPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        tags$h4("Elbow Method Chart"),
        downloadButton(ns("download_elbow"), "Exportar Gráfico", class = "btn-sm")
    ),
    shinycssloaders::withSpinner(
      plotOutput(ns("elbowChart"), height = "400px"),
      type = 4,
      color = "#286090"
    )
  )
}

elbowPlotServer <- function(id, dados_cluster_definicoes, metodologia_selecionada, mes_selecionado, hora_selecionada) {
  moduleServer(id, function(input, output, session) {
    
    dados_para_plotar <- reactive({
      req(dados_cluster_definicoes())
      
      df_definicoes <- dados_cluster_definicoes()
      metodo <- metodologia_selecionada()
      
      tabela_meses <- data.frame(
        nome_completo = month.name,
        nome_abreviado = c('jan', 'fev', 'mar', 'abr', 'mai', 'jun', 'jul', 'ago', 'set', 'out', 'nov', 'dez')
      )
      
      linha_contexto <- switch(metodo,
       "Single Period" = { df_definicoes },
       "Monthly" = {
         req(mes_selecionado())
         mes_filtrar <- tabela_meses$nome_abreviado[tabela_meses$nome_completo == mes_selecionado()]
         df_definicoes %>% dplyr::filter(Month == mes_filtrar)
       },
       "Hourly" = {
         req(hora_selecionada())
         df_definicoes %>% dplyr::filter(Hour == hora_selecionada())
       },
       "Monthly and Hourly" = {
         req(mes_selecionado(), hora_selecionada())
         mes_filtrar <- tabela_meses$nome_abreviado[tabela_meses$nome_completo == mes_selecionado()]
         df_definicoes %>% dplyr::filter(Month == mes_filtrar, Hour == hora_selecionada())
       }
      )
      
      validate(
        need(nrow(linha_contexto) > 0, "Não foi possível encontrar dados do método do cotovelo para a seleção atual.")
      )
      
      dados_elbow <- linha_contexto$elbow_data[[1]]
      n_cluster_otimo <- nrow(linha_contexto)
      
      list(dados_elbow = dados_elbow, n_cluster = n_cluster_otimo)
    })
    
    plot_obj <- reactive({
      info_plot <- dados_para_plotar()
      req(info_plot)
      
      base <- info_plot$dados_elbow
      num_cl <- info_plot$n_cluster
      
      ggplot2::ggplot(data = base, ggplot2::aes(k.values, wss_values)) +
        ggplot2::geom_line(color = "black") +
        ggplot2::geom_point(size = 2, color = "black") +
        ggplot2::geom_point(data = base[num_cl, ], ggplot2::aes(k.values, wss_values), color = "#059669", size = 4) +
        ggplot2::geom_vline(xintercept = num_cl, linetype = "dashed", color = "#059669") +
        ggplot2::labs(
          x = "Number of Cluster (k)",
          y = "Within-Cluster Sum of Squares (WSS)"
        ) +
        ggplot2::theme_minimal()
    })
    
    output$elbowChart <- renderPlot({ plot_obj() }, res = 96)
    
    output$download_elbow <- downloadHandler(
      filename = function() { paste0("metodo_cotovelo_", Sys.Date(), ".png") },
      content = function(file) { ggplot2::ggsave(file, plot = plot_obj(), width = 8, height = 6, dpi = 300) }
    )
  })
}