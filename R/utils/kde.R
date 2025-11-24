criar_modelos_kde <- function(dados, metodologia) {
  cols_group <- switch(metodologia,
                       "Single Period" = character(0),
                       "Monthly" = "Month",
                       "Hourly" = "Hour",
                       "Monthly and Hourly" = c("Month", "Hour")
  )
  cols_group <- c(cols_group, "cluster")
  dados_validos <- dados %>%
    dplyr::filter(!is.na(power), !is.na(cluster))
  
  modelos_kde <- dados_validos %>%
    dplyr::group_by(!!!rlang::syms(cols_group)) %>%
    dplyr::filter(dplyr::n() > 1) %>% 
    tidyr::nest() %>%
    dplyr::mutate(
      ModeloKDE = purrr::map(data, ~ tryCatch(stats::density(.x$power, na.rm = TRUE), error = function(e) NULL))
    ) %>%
    dplyr::select(-data)
  
  return(modelos_kde)
}

simular_potencia_kde <- function(dados_para_simular, tabela_modelos_kde, tabela_definicoes_clusters, metodologia, total_cenarios) {
  cols_join <- switch(metodologia,
                      "Single Period" = "cluster",
                      "Monthly" = c("Month", "cluster"),
                      "Hourly" = c("Hour", "cluster"),
                      "Monthly and Hourly" = c("Month", "Hour", "cluster")
  )
  dados_prep <- dados_para_simular
  if(!"cluster" %in% names(dados_prep)) {
    stop("Erro Crítico: A simulação precisa da coluna 'cluster'.")
  }
  
  if("cluster" %in% names(tabela_modelos_kde)) {
    dados_prep$cluster <- as.numeric(dados_prep$cluster)
    tabela_modelos_kde$cluster <- as.numeric(tabela_modelos_kde$cluster)
  }
  
  dados_prep$..row_id.. <- 1:nrow(dados_prep)
  
  dados_com_modelo <- dplyr::left_join(dados_prep, tabela_modelos_kde, by = cols_join) %>%
    dplyr::arrange(..row_id..)
  
  amostrar_cenarios <- function(modelo, n, row_info) {
  if (is.null(modelo) || !is.list(modelo)) {
      return(rep(0, n)) 
    }
    tryCatch({
      sample(modelo$x, size = n, replace = TRUE, prob = modelo$y)
    }, error = function(e) rep(0, n))
  }
  
  lista_cenarios <- purrr::map(dados_com_modelo$ModeloKDE, ~ amostrar_cenarios(.x, total_cenarios))
  matriz_final <- do.call(rbind, lista_cenarios)
  
  matriz_final[is.na(matriz_final)] <- 0
  
  return(matriz_final)
}

encontrar_cluster_para_velocidade <- function(velocidade, mes, hora, metodologia, tabela_definicoes) {
  defs <- tabela_definicoes
  if(grepl("Monthly", metodologia)) defs <- defs %>% dplyr::filter(Month == mes)
  if(grepl("Hourly", metodologia)) defs <- defs %>% dplyr::filter(Hour == hora)
  
  if(nrow(defs) == 0) return(NA)
  
  match <- defs %>% dplyr::filter(velocidade >= min_speed & velocidade <= max_speed)
  if(nrow(match) > 0) return(match$cluster[1])
  
  distancias <- pmax(0, defs$min_speed - velocidade) + pmax(0, velocidade - defs$max_speed)
  
  idx_min <- which.min(distancias)
  
  return(defs$cluster[idx_min])
}