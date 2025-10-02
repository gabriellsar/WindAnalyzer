# Encontra o ponto geográfico mais próximo de um alvo.
find_nearest_neighbor <- function(target_lon, target_lat, candidate_points_df) {
  best_lat <- NA
  best_lon <- NA
  min_distance <- Inf
  
  rad <- pi / 180
  rad_target_lat <- rad * target_lat
  rad_target_lon <- rad * target_lon
  
  names(candidate_points_df) <- c("long", "lat")
  
  for (i in 1:nrow(candidate_points_df)) {
    candidate_lat <- candidate_points_df$lat[i]
    candidate_lon <- candidate_points_df$long[i]
    
    current_distance <- 6378.388 * acos(
      sin(rad_target_lat) * sin(rad * candidate_lat) + 
        cos(rad_target_lat) * cos(rad * candidate_lat) * cos(rad * candidate_lon - rad_target_lon)
    )
    
    if (is.na(current_distance)) next
    
    if (current_distance < min_distance) {
      best_lat <- candidate_lat
      best_lon <- candidate_lon
      min_distance <- current_distance
    }
  }
  
  return(list(lon = best_lon, lat = best_lat, distance = min_distance))
}