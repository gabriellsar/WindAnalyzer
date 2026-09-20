wss = function(k,aux){
  a = kmeans(aux, k, nstart = 10, iter.max = 500)
  return(a)
}

clusterizacao = function(temp){
  
  Vet_cluster = rep(NA, nrow(temp))

  # O kmeans exige k menor que o numero de observacoes e que o de valores distintos.
  # Sem esse teto, subgrupos pequenos (ex.: fevereiro numa estratificacao mes-hora com
  # um ano de dados) abortam a analise inteira.
  k_max = max(1, min(30, nrow(temp) - 1, length(unique(temp$speed)) - 1))

  wss_values  = rep(NA, k_max)
  wss_size  = rep(NA, k_max)
  for (k.values in 1:k_max) {
    clt = wss(k.values,temp$speed)
    wss_values[k.values] = clt$tot.withinss
    wss_size[k.values] = min(clt$size)
  }

  k.values = 1:k_max
  df = data.frame(k.values = k.values, wss_values = wss_values)
  df$perc = df$wss_values/df$wss_values[1]
  df$diference = NA
  for(i in k.values){
    if(i == length(k.values)){df$diference[i] = 0}else{
      df$diference[i] = round(df$perc[i] - df$perc[i+1],3)
    }
  }
  
  n_cluster = which(df$diference < 0.001)[1]
  if (is.na(n_cluster)) { n_cluster = k_max }

  # A guarda em n_cluster > 1 evita o acesso a wss_size[0], que erra com
  # "argument is of length zero".
  while(n_cluster > 1 && wss_size[n_cluster] < 4) {
    n_cluster=n_cluster-1
  }

  cl = kmeans(temp$speed, n_cluster,iter.max = 500)
  # Reinicios limitados: sem o teto, um subgrupo que nunca produz clusters com 4
  # observacoes trava o processo R inteiro, e com ele todas as sessoes servidas por ele.
  tentativas = 1
  while (min(cl$size) < 4 && tentativas < 10) {
    cl = kmeans(temp$speed, n_cluster,iter.max = 500)
    tentativas = tentativas + 1
  }
  Vet_cluster = cl$cluster
  Vet_speed = cl$centers
  
  labelled_points = tibble(
    cluster = cl$cluster,
    x1 = temp$speed,
    x2 = temp$power)
  
  dados_EM_divisao <- vector("list", n_cluster)
  min_speed <- numeric(n_cluster)
  max_speed <- numeric(n_cluster)
  
  for(i in 1:n_cluster) {
    idx_cluster <- which(cl$cluster == i)
    
    dados_EM_divisao[[i]] <- temp$power[idx_cluster]
    min_speed[i] <- min(temp$speed[idx_cluster])
    max_speed[i] <- max(temp$speed[idx_cluster])
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

  # Semente fixa: sem ela, os centroides iniciais do k-means mudam a cada execucao e a
  # mesma base devolve numeros de clusters diferentes, impedindo auditoria dos resultados.
  set.seed(42L)

  dados_EM$id_original <- 1:nrow(dados_EM)
  
  variaveis_agrupamento <- switch(metodologia,
                                  "Single Period" = NULL,
                                  "Monthly" = "Month",
                                  "Hourly" = "Hour",
                                  "Monthly and Hourly" = c("Month", "Hour")
  )
  
  # Agrupa os dados e aplica a função de clusterização a cada grupo
  resultados_agrupados <- dados_EM %>%
    { if (!is.null(variaveis_agrupamento)) dplyr::group_by(., !!!rlang::syms(variaveis_agrupamento)) else . } %>%
    tidyr::nest() %>%
    dplyr::mutate(
      resultado_cluster = purrr::map(data, clusterizacao)
    )
  
  # Preparar a tabela de definições dos clusters
  definicoes_clusters <- resultados_agrupados %>%
    tidyr::unnest_wider(resultado_cluster) %>%
    dplyr::select(-data, -CL) %>%
    tidyr::unnest(cols = c(Min_S, Max_S, value_S)) %>%
    { if (!is.null(variaveis_agrupamento)) dplyr::group_by(., !!!rlang::syms(variaveis_agrupamento)) else . } %>%
    dplyr::mutate(cluster = 1:n()) %>%
    dplyr::ungroup() %>%
    dplyr::rename(min_speed = Min_S, max_speed = Max_S, centroid_speed = value_S, elbow_data = ind_cotovelo) %>%
    
    # Adiciona a coluna de metodologia para uso futuro
    dplyr::mutate(metodologia = !!metodologia, .before = 1)
  
  atribuicoes <- resultados_agrupados %>%
    dplyr::select(data, resultado_cluster) %>%
    dplyr::mutate(
      atribuicoes_grupo = purrr::map2(data, resultado_cluster, ~ tibble::tibble(
        id_original = .x$id_original,
        cluster = .y$CL
      ))
    ) %>%
    dplyr::select(atribuicoes_grupo) %>%
    tidyr::unnest(cols = c(atribuicoes_grupo)) %>%
    dplyr::arrange(id_original) %>%
    dplyr::pull(cluster)
  
  # Retorna a lista com as duas saídas padronizadas
  return(list(
    atribuicoes = atribuicoes,
    definicoes_clusters = definicoes_clusters
  ))
}