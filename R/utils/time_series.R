#' @title Combinar e Preparar Dados de Potência e Velocidade
#' @description Esta função recebe dataframes de potência e velocidade do vento,
#'              combina-os com base no tempo e prepara a estrutura de dados para
#'              as análises subsequentes. Assume-se que os dados de entrada
#'              já foram limpos pelo usuário.
#'
#' @param dados_potencia_brutos Um dataframe contendo os dados históricos de potência.
#'                              Deve ter três colunas: Data (YYYY-MM-DD), Hora e Potencia.
#'                              
#' @param dados_velocidade_brutos Um dataframe contendo os dados históricos de velocidade.
#'                                Deve ter três colunas: Data (YYYY-MM-DD), Hora e Velocidade.
#'
#' @return Um dataframe combinado e estruturado, pronto para a modelagem.

combinar_dados_potencia_velocidade <- function(dados_potencia_brutos, dados_velocidade_brutos, capacidade_instalada) {
  
  colnames(dados_potencia_brutos) <- c("Data", "Hora", "Potencia")
  dados_potencia_brutos$Timestamp <- as.POSIXct(paste(dados_potencia_brutos$Data, dados_potencia_brutos$Hora, sep = " "),
                                                format = "%Y-%m-%d %H", tz = "UTC")
  
  colnames(dados_velocidade_brutos) <- c("Data", "Hora", "Velocidade")
  dados_velocidade_brutos$Timestamp <- as.POSIXct(paste(dados_velocidade_brutos$Data, dados_velocidade_brutos$Hora, sep = " "),
                                                  format = "%Y-%m-%d %H", tz = "UTC")
  
  dados_combinados <- dplyr::inner_join(
    dplyr::select(dados_potencia_brutos, Timestamp, Potencia),
    dplyr::select(dados_velocidade_brutos, Timestamp, Velocidade),
    by = "Timestamp"
  )
  
  ordem_meses <- c('jan', 'fev', 'mar', 'abr', 'mai', 'jun', 'jul', 'ago', 'set', 'out', 'nov', 'dez')
  dados_combinados$MonthAbbr <- factor(format(dados_combinados$Timestamp, "%b"), levels = ordem_meses)

  dados_combinados$Estimado <- NA

  dados_finais <- dados_combinados %>%
    dplyr::rename(
      data = Timestamp,
      power = Potencia,
      speed = Velocidade,
      Month = MonthAbbr 
    ) %>%
    dplyr::mutate(
      Year  = lubridate::year(data),
      Day   = lubridate::day(data),
      Hour  = lubridate::hour(data)
    ) %>%
    dplyr::select(
      Year, Month, Day, Hour, power, data, speed, Estimado
    )

  return(dados_finais)
}