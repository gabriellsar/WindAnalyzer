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

combinar_dados_potencia_velocidade <- function(dados_potencia_brutos, dados_velocidade_brutos) {
  
  # Padroniza nomes
  colnames(dados_potencia_brutos) <- c("Data", "Hora", "Potencia")
  colnames(dados_velocidade_brutos) <- c("Data", "Hora", "Velocidade")
  
  # --- CORREÇÃO: Forçar conversão para numérico ---
  # Função auxiliar para limpar e converter
  to_numeric_safe <- function(x) {
    if(is.numeric(x)) return(x)
    # Substitui vírgula por ponto e converte
    as.numeric(gsub(",", ".", as.character(x)))
  }
  
  dados_potencia_brutos$Potencia <- to_numeric_safe(dados_potencia_brutos$Potencia)
  dados_velocidade_brutos$Velocidade <- to_numeric_safe(dados_velocidade_brutos$Velocidade)
  # -----------------------------------------------
  
  # Cria Timestamp (Assume formato YYYY-MM-DD ou DD/MM/YYYY - tenta converter)
  # Se Data já for Date/POSIXct, mantém. Se for char, converte.
  if(!lubridate::is.POSIXct(dados_potencia_brutos$Data) && !lubridate::is.Date(dados_potencia_brutos$Data)){
    dados_potencia_brutos$Data <- as.Date(parse_date_time(dados_potencia_brutos$Data, orders = c("ymd", "dmy", "mdy")))
  }
  if(!lubridate::is.POSIXct(dados_velocidade_brutos$Data) && !lubridate::is.Date(dados_velocidade_brutos$Data)){
    dados_velocidade_brutos$Data <- as.Date(parse_date_time(dados_velocidade_brutos$Data, orders = c("ymd", "dmy", "mdy")))
  }
  
  dados_potencia_brutos$Timestamp <- as.POSIXct(paste(dados_potencia_brutos$Data, dados_potencia_brutos$Hora, sep = " "),
                                                format = "%Y-%m-%d %H", tz = "UTC")
  
  dados_velocidade_brutos$Timestamp <- as.POSIXct(paste(dados_velocidade_brutos$Data, dados_velocidade_brutos$Hora, sep = " "),
                                                  format = "%Y-%m-%d %H", tz = "UTC")
  
  # Remove NAs gerados por conversão antes do join
  dados_potencia_brutos <- na.omit(dados_potencia_brutos)
  dados_velocidade_brutos <- na.omit(dados_velocidade_brutos)
  
  dados_combinados <- dplyr::inner_join(
    dplyr::select(dados_potencia_brutos, Timestamp, Potencia),
    dplyr::select(dados_velocidade_brutos, Timestamp, Velocidade),
    by = "Timestamp"
  )
  
  ordem_meses <- c('jan', 'fev', 'mar', 'abr', 'mai', 'jun', 'jul', 'ago', 'set', 'out', 'nov', 'dez')
  dados_combinados$Month <- factor(format(dados_combinados$Timestamp, "%b"), levels = ordem_meses)
  
  dados_combinados$Estimado <- NA
  
  dados_finais <- dados_combinados %>%
    dplyr::rename(
      data = Timestamp,
      power = Potencia,
      speed = Velocidade,
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