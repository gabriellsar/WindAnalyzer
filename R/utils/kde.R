#' @title Criar Modelos de Densidade (KDE) de Forma Genérica
#' @description Cria modelos KDE para a potência eólica, agrupando os dados
#'              de acordo com a metodologia especificada e o cluster.
#'
#' @param dados Um dataframe contendo as colunas 'power', 'cluster' e as colunas de
#'              agrupamento necessárias ('Month', 'Hour').
#' @param metodologia A string que define como agrupar: "Single Period", "Monthly",
#'                    "Hourly", ou "Monthly and Hourly".
#'
#' @return Um tibble (dataframe) que funciona como uma tabela de consulta,
#'         contendo as colunas de agrupamento e uma list-column com os modelos KDE.

criar_modelos_kde <- function(dados, metodologia) {
  
  variaveis_agrupamento <- switch(metodologia,
                                  "Single Period" = character(0), # Nenhum agrupamento extra
                                  "Monthly" = "Month",
                                  "Hourly" = "Hour",
                                  "Monthly and Hourly" = c("Month", "Hour")
  )
  
  variaveis_agrupamento <- c(variaveis_agrupamento, "cluster")
  
  modelos_kde <- dados %>%
    dplyr::group_by(!!!rlang::syms(variaveis_agrupamento)) %>%

    dplyr::filter(n() > 1) %>%
    tidyr::nest() %>%
    
    dplyr::mutate(
      ModeloKDE = purrr::map(data, ~density(.x$power, na.rm = TRUE))
    ) %>%
    dplyr::select(-data) # Remove a coluna de dados aninhados
  
  return(modelos_kde)
}

#' @title Encontrar Cluster Usando uma Tabela de Consulta Tidy
#' @description Determina o cluster de uma velocidade filtrando uma tabela de
#'              definições de cluster e, se necessário, buscando o vizinho mais próximo.
#'
#' @param velocidade O valor numérico da velocidade do vento.
#' @param mes O mês (numérico, 1-12) da observação.
#' @param hora A hora (numérica, 0-23) da observação.
#' @param metodologia A string que define o método ("Single Period", "Monthly", etc.).
#' @param tabela_definicoes A tabela tidy gerada pela função `clusterizar_dados`.
#'
#' @return Um número inteiro representando o cluster encontrado.

encontrar_cluster_para_velocidade <- function(velocidade, mes, hora, metodologia, tabela_definicoes) {

  ranges_contexto <- tabela_definicoes %>%
    dplyr::filter(metodologia == !!metodologia)
  
  if (metodologia %in% c("Monthly", "Monthly and Hourly")) {
    ranges_contexto <- ranges_contexto %>% dplyr::filter(mes == !!mes)
  }
  if (metodologia %in% c("Hourly", "Monthly and Hourly")) {
    ranges_contexto <- ranges_contexto %>% dplyr::filter(hora == !!hora)
  }
  
  min_speed_vetor <- ranges_contexto$min_speed
  max_speed_vetor <- ranges_contexto$max_speed
  cluster_ids     <- ranges_contexto$cluster
  
  indice_encontrado <- which(velocidade >= min_speed_vetor & velocidade <= max_speed_vetor)
  
  # Se falhar, inicia a busca pelo vizinho mais próximo
  if (length(indice_encontrado) == 0) {
    indice_acima <- NULL
    indice_abaixo <- NULL
    velocidade_acima <- velocidade
    velocidade_abaixo <- velocidade
    
    while (length(indice_acima) == 0 && length(indice_abaixo) == 0) {
      velocidade_acima <- velocidade_acima + 0.1
      indice_acima <- which(velocidade_acima >= min_speed_vetor & velocidade_acima <= max_speed_vetor)
      
      velocidade_abaixo <- velocidade_abaixo - 0.1
      indice_abaixo <- which(velocidade_abaixo >= min_speed_vetor & velocidade_abaixo <= max_speed_vetor)
    }
    
    indice_encontrado <- if (length(indice_acima) > 0) indice_acima else indice_abaixo
  }
  
  return(cluster_ids[indice_encontrado[1]])
}


#' @title Simular Potência Eólica (Versão Corrigida)
#' @param dados_para_simular Dataframe com os dados de entrada.
#' @param tabela_modelos_kde A tabela de modelos gerada por `criar_modelos_kde`.
#' @param tabela_definicoes_clusters A tabela tidy de definições de cluster gerada por `clusterizar_dados`.
#' @param metodologia A string da metodologia ("Single Period", etc.).
#' @param total_cenarios O número de cenários a serem gerados.

simular_potencia_kde <- function(dados_para_simular, tabela_modelos_kde, tabela_definicoes_clusters, metodologia, total_cenarios) {
  
  cenarios <- matrix(NA, ncol = total_cenarios, nrow = nrow(dados_para_simular))
  colunas_agrupamento <- setdiff(names(tabela_modelos_kde), "ModeloKDE")
  
  for (obs in 1:nrow(dados_para_simular)) {
    observacao_atual <- dados_para_simular[obs, ]
    
    cluster_atual <- encontrar_cluster_para_velocidade(
      velocidade = observacao_atual$speed,
      mes = observacao_atual$Month,
      hora = observacao_atual$Hour,
      metodologia = metodologia,
      tabela_definicoes = tabela_definicoes_clusters 
    )
    
    if (is.na(cluster_atual)) next # Pula para a próxima iteração se nenhum cluster for encontrado
    
    criterios <- tibble::tibble(cluster = cluster_atual)
    if ("Month" %in% colunas_agrupamento) criterios$Month <- observacao_atual$Month
    if ("Hour" %in% colunas_agrupamento) criterios$Hour <- observacao_atual$Hour
    
    modelo_encontrado <- dplyr::inner_join(tabela_modelos_kde, criterios, by = colunas_agrupamento)
    
    if (nrow(modelo_encontrado) == 1) {
      modelo_kde <- modelo_encontrado$ModeloKDE[[1]]
      
      cenarios[obs, ] <- sample(
        x = modelo_kde$x,
        size = total_cenarios,
        prob = modelo_kde$y,
        replace = TRUE
      )
    }
  }
  
  return(cenarios)
}