wss = function(k,aux){
  a = kmeans(aux, k, nstart = 10 )
  return(a)
}

clusterizacao = function(temp){
  
  Vet_cluster = rep(NA, nrow(temp))
  
  wss_values  = rep(NA, 30)
  wss_size  = rep(NA, 30)
  for (k.values in 1:30) {
    clt = wss(k.values,temp$speed)
    wss_values[k.values] = clt$tot.withinss
    wss_size[k.values] = min(clt$size)
  }
  
  k.values = 1:30
  df = data.frame(k.values = k.values, wss_values = wss_values)
  df$perc = df$wss_values/df$wss_values[1]
  df$diference = NA
  for(i in k.values){
    if(i == length(k.values)){df$diference[i] = 0}else{
      df$diference[i] = round(df$perc[i] - df$perc[i+1],3)
    }
  }
  
  n_cluster = which(df$diference < 0.001)[1]
  
  while(wss_size[n_cluster] < 4) {
    n_cluster=n_cluster-1
  }
  
  cl = kmeans(temp$speed, n_cluster)
  while (min(cl$size) < 4) {
    cl = kmeans(temp$speed, n_cluster)
  }
  Vet_cluster = cl$cluster
  Vet_speed = cl$centers
  
  labelled_points = tibble(
    cluster = cl$cluster,
    x1 = temp$speed,
    x2 = temp$power)
  
  dados_EM_divisao = list()
  min_speed = max_speed = NULL
  for(i in 1:n_cluster){
    dados_EM_divisao = c(dados_EM_divisao,
                         list(temp$power[which(cl$cluster == i)]))
    min_speed[i] = min(temp$speed[which(cl$cluster == i)])
    max_speed[i] = max(temp$speed[which(cl$cluster == i)])
  }
  
  Saida = list(CL=Vet_cluster, Min_S=min_speed, Max_S=max_speed, value_S=Vet_speed, ind_cotovelo = df)
  
  return(Saida)
}

#' @title Executa a Clusterização de Dados de Vento
#' @description Agrupa os dados de vento com base na metodologia e executa a
#'              clusterização k-means para cada grupo.
#' @param dados_EM O dataframe principal com colunas `speed`, `power`, `Month`, `Hour`.
#' @param metodologia A string que define como agrupar os dados.
#' @return Uma lista contendo duas saídas:
#'         1. `atribuicoes`: Vetor com o ID do cluster para cada linha de `dados_EM`.
#'         2. `definicoes_clusters`: Uma tabela tidy com os detalhes de cada cluster.

clusterizar_dados <- function(dados_EM, metodologia) {
  dados_EM$id_original <- 1:nrow(dados_EM)
  
  variaveis_agrupamento <- switch(metodologia,
                                  "Single Period" = NULL,
                                  "Monthly" = "Month",
                                  "Hourly" = "Hour",
                                  "Monthly and Hourly" = c("Month", "Hour")
  )
  
  resultados_agrupados <- dados_EM %>%
    { if (!is.null(variaveis_agrupamento)) dplyr::group_by(., !!!rlang::syms(variaveis_agrupamento)) else . } %>%
    tidyr::nest() %>%
    dplyr::mutate(
      resultado_cluster = purrr::map(data, clusterizacao)
    )

  definicoes_clusters <- resultados_agrupados %>%
    tidyr::unnest_wider(resultado_cluster) %>%
    dplyr::select(-data, -CL) %>%
    tidyr::unnest(cols = c(Min_S, Max_S, value_S)) %>%
    dplyr::group_by(!!!rlang::syms(variaveis_agrupamento)) %>%
    dplyr::mutate(cluster = 1:n()) %>%
    dplyr::ungroup() %>%
    dplyr::rename(min_speed = Min_S, max_speed = Max_S, centroid_speed = value_S, elbow_data = ind_cotovelo)
  
  atribuicoes <- resultados_agrupados %>%
    dplyr::select(data, resultado_cluster) %>%
    tidyr::unnest(data) %>%
    dplyr::mutate(
      cluster = purrr::map2_dbl(resultado_cluster, id_original, ~ .x$CL[match(id_original, .y$id_original)])
    ) %>%
    dplyr::arrange(id_original) %>%
    dplyr::pull(cluster)
  
  return(list(
    atribuicoes = atribuicoes,
    definicoes_clusters = definicoes_clusters
  ))
}