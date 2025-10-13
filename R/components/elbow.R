elbowPlotUI <- function(id) {
  ns <- NS(id)
  
  shinycssloaders::withSpinner(
    plotly::plotlyOutput(ns("elbowChart")),
    type = 4,
    color = "#286090"
  )
}

elbowPlotServer <- function(id, dados_cluster_definicoes, metodologia_selecionada, mes_selecionado, hora_selecionada) {
  
  moduleServer(id, function(input, output, session) {
    
    # Reativo para preparar os dados específicos para o gráfico
    dados_para_plotar <- reactive({
      
      # Garante que os dados da clusterização estejam disponíveis
      req(dados_cluster_definicoes())
      
      df_definicoes <- dados_cluster_definicoes()
      metodo <- metodologia_selecionada()
      
      # Filtra os dados de definição do cluster com base no contexto (metodologia, mês, hora)
      if (metodo == "Single Period") {
        # Para single period, pegamos o primeiro (e único) resultado
        dados_elbow <- df_definicoes$elbow_data[[1]]
        n_cluster_otimo <- df_definicoes$cluster[nrow(df_definicoes)]
        
      } else if (metodo == "Monthly") {
        req(mes_selecionado())
        mes_num <- match(mes_selecionado(), month.name)
        
        # Filtra pela metodologia e mês
        linha_contexto <- df_definicoes %>%
          dplyr::filter(metodologia == metodo, mes == mes_num)
        
        dados_elbow <- linha_contexto$elbow_data[[1]]
        n_cluster_otimo <- linha_contexto$cluster[nrow(linha_contexto)]
        
      } else if (metodo == "Hourly") {
        # Lógica similar para o modo horário...
      } # ... etc. para outras metodologias
      
      # Retorna uma lista com os dados do gráfico e o k ótimo
      list(dados_elbow = dados_elbow, n_cluster = n_cluster_otimo)
    })
    
    # Renderiza o gráfico Plotly
    output$elbowChart <- plotly::renderPlotly({
      
      info_plot <- dados_para_plotar()
      req(info_plot)
      
      base <- info_plot$dados_elbow
      num_cl <- info_plot$n_cluster
      
      # Constrói o título dinamicamente
      titulo <- paste0("Application of the Elbow Method (", metodologia_selecionada(), ")")
      
      p <- ggplot(data = base, aes(k.values, wss_values)) +
        geom_line(color = "black") +
        geom_point(size = 2, color = "black") +
        
        # Destaca o ponto do cluster ótimo escolhido pelo algoritmo
        geom_point(data = base[num_cl, ], aes(k.values, wss_values), color = "#B53737", size = 4) +
        scale_x_discrete(limits = as.factor(base$k.values)) +
        labs(
          title = titulo,
          x = "Number of clusters K",
          y = "With-in-Sum-of-Squares (WSS)"
        ) +
        theme_minimal()
      
      plotly::ggplotly(p)
    })
  })
}