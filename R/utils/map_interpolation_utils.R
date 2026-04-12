# Encontra o ponto geográfico mais próximo de um alvo.
find_nearest_neighbor <- function(target_lon, target_lat, candidate_points_df) {
  rad <- pi / 180
  rad_target_lat <- rad * target_lat
  rad_target_lon <- rad * target_lon
  
  # Prevenção: Forçar nomes das colunas (garantir acesso vetorial)
  cand_lon_rad <- candidate_points_df[[1]] * rad
  cand_lat_rad <- candidate_points_df[[2]] * rad
  
  # Vetorialização: O R aplica a todos os pontos de uma só vez
  distances <- 6378.388 * acos(
    sin(rad_target_lat) * sin(cand_lat_rad) + 
      cos(rad_target_lat) * cos(cand_lat_rad) * cos(cand_lon_rad - rad_target_lon)
  )
  
  # Encontrar o índice da menor distância
  idx_min <- which.min(distances)
  
  return(list(
    lon = candidate_points_df[[1]][idx_min], 
    lat = candidate_points_df[[2]][idx_min], 
    distance = distances[idx_min]
  ))
}