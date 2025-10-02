# Agrega uma série temporal para uma escala de tempo maior.
aggregate_time_series_scale <- function(time_scale, input_series_df) {
  time_series_tbl <- tibble::tibble(
    time = input_series_df$MerraDate,
    speed = input_series_df$velocEXT
  )
  time_series_tbl <- tibbletime::as_tbl_time(time_series_tbl, index = time)
  
  collapse_period <- switch(time_scale,
                            "Day" = "daily",
                            "Month" = "monthly",
                            "Year" = "yearly",
                            "Hour" = "hourly")
  
  if (collapse_period == "hourly") {
    return(time_series_tbl)
  }
  
  aggregated_series <- time_series_tbl %>%
    tibbletime::collapse_by(period = collapse_period) %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(speed = mean(speed, na.rm = TRUE))
  
  return(aggregated_series)
}


# Calcula o fator de correção de viés entre as séries INMET e MERRA-2.
calculate_inmet_correction_factor <- function(comparison_df) {

  comparison_df[comparison_df[, 3] <= 0 | comparison_df[, 3] > 25, 3] <- NA
  comparison_df[comparison_df[, 2] <= 0 | comparison_df[, 2] > 25, 2] <- NA
  comparison_df <- na.omit(comparison_df)
  
  if (nrow(comparison_df) < 1) return(1)
  
  correction_factor <- mean(comparison_df[, 3], na.rm = TRUE) / mean(comparison_df[, 2], na.rm = TRUE)
  
  if (is.na(correction_factor) || !is.finite(correction_factor)) {
    return(1)
  }
  return(correction_factor)
}

# Aplica a correção de viés a uma série de velocidade do vento.
perform_bias_correction <- function(correction_type, inmet_station_info, inmet_timeseries_data, merra2_grid_points, user_series_df, tokens) {
  
  merra2_coords_for_inmet <- find_nearest_neighbor(
    inmet_station_info$lon, inmet_station_info$lat, merra2_grid_points
  )
  
  merra2_filename <- paste0(merra2_coords_for_inmet$lon, "_", merra2_coords_for_inmet$lat, ".feather")
  merra2_raw_data_for_inmet <- read_feather_dropbox_httr(
    paste0("/Wind Analyzer/", merra2_filename), 
    tokens$refresh, tokens$client_id, tokens$client_secret
  )
  
  merra2_time_index <- arrow::read_feather("MerraDate.feather")
  merra2_full_data_for_inmet <- cbind(merra2_time_index, merra2_raw_data_for_inmet)
  
  merra2_full_data_for_inmet$speed_50m <- sqrt(merra2_full_data_for_inmet$U50M^2 + merra2_full_data_for_inmet$V50M^2)
  merra2_full_data_for_inmet$speed_10m <- sqrt(merra2_full_data_for_inmet$U10M^2 + merra2_full_data_for_inmet$V10M^2)
  
  friction_coefficient <- (log(merra2_full_data_for_inmet$speed_50m) - log(merra2_full_data_for_inmet$speed_10m)) / (log(50) - log(10 + merra2_full_data_for_inmet$DISPH))
  merra2_full_data_for_inmet$merra2_speed_at_inmet_height <- merra2_full_data_for_inmet$speed_50m * ((10 / 50)^friction_coefficient)
  
  start_date_inmet <- as.POSIXct("2008-01-01 00:00:00", tz = "UTC")
  end_date_inmet <- as.POSIXct("2023-11-30 23:00:00", tz = "UTC")
  
  comparison_period_df <- merra2_full_data_for_inmet[
    merra2_full_data_for_inmet$MerraDate >= start_date_inmet & merra2_full_data_for_inmet$MerraDate <= end_date_inmet, 
    c("MerraDate", "merra2_speed_at_inmet_height")
  ]
  comparison_period_df$INMET <- inmet_timeseries_data[1:nrow(comparison_period_df), (inmet_station_info$indice + 2)]
  comparison_period_df$Hour <- lubridate::hour(comparison_period_df$MerraDate)
  comparison_period_df$Month <- lubridate::month(comparison_period_df$MerraDate)
  
  user_series_df$factor <- NA
  user_series_df$Hour <- lubridate::hour(user_series_df$MerraDate)
  user_series_df$Month <- lubridate::month(user_series_df$MerraDate)
  
  if (correction_type == "Single Period") {
    factor_value <- calculate_inmet_correction_factor(comparison_period_df)
    user_series_df$factor <- factor_value
  } else if (correction_type == "Monthly") {
    for (mes in 1:12) {
      subset <- dplyr::filter(comparison_period_df, Month == mes)
      factor_value <- calculate_inmet_correction_factor(subset)
      user_series_df$factor[user_series_df$Month == mes] <- factor_value
    }
  } else if (correction_type == "Hourly") {
    for (i in 0:23) {
      subset <- dplyr::filter(comparison_period_df, Hour == i)
      factor_value <- calculate_inmet_correction_factor(subset)
      user_series_df$factor[user_series_df$Hour == i] <- factor_value
    }
  } else if (correction_type == "Monthly and Hourly") {
    for (mes in 1:12) {
      for (i in 0:23) {
        subset <- dplyr::filter(comparison_period_df, Month == mes & Hour == i)
        factor_value <- calculate_inmet_correction_factor(subset)
        user_series_df$factor[user_series_df$Month == mes & user_series_df$Hour == i] <- factor_value
      }
    }
  }
  
  corrected_speed_series <- user_series_df$velocEXT * user_series_df$factor
  return(corrected_speed_series)
}


# Gera uma série temporal de velocidade do vento para um local e período específicos.
generate_wind_speed_series <- function(target_merra_lat, target_merra_lon, time_scale, rotor_height, start_date, end_date, use_inmet_correction, correction_type, inmet_station_info, inmet_timeseries_data, merra2_grid_points, tokens) {
  
  merra2_filename <- paste0(target_merra_lon, "_", target_merra_lat, ".feather")
  merra2_raw_data <- read_feather_dropbox_httr(
    paste0("/Wind Analyzer/", merra2_filename), 
    tokens$refresh, tokens$client_id, tokens$client_secret
  )
  
  merra2_time_index <- arrow::read_feather("MerraDate.feather")
  merra2_full_data <- cbind(merra2_time_index, merra2_raw_data)
  
  merra2_full_data$speed_50m <- sqrt(merra2_full_data$U50M^2 + merra2_full_data$V50M^2)
  merra2_full_data$speed_10m <- sqrt(merra2_full_data$U10M^2 + merra2_full_data$V10M^2)
  
  friction_coefficient <- (log(merra2_full_data$speed_50m) - log(merra2_full_data$speed_10m)) / (log(50) - log(10 + merra2_full_data$DISPH))
  merra2_full_data$velocEXT <- merra2_full_data$speed_50m * ((rotor_height / 50)^friction_coefficient)
  
  start_datetime <- as.POSIXct(paste0(start_date, " 00:00:00"), tz = "UTC")
  end_datetime <- as.POSIXct(paste0(end_date, " 23:00:00"), tz = "UTC")
  
  filtered_series_df <- merra2_full_data[merra2_full_data$MerraDate >= start_datetime & merra2_full_data$MerraDate <= end_datetime, ]
  
  processed_merra2_dataset <- data.frame(
    Data = filtered_series_df$MerraDate, U10M = filtered_series_df$U10M, U50M = filtered_series_df$U50M,
    V10M = filtered_series_df$V10M, V50M = filtered_series_df$V50M, DISPH = filtered_series_df$DISPH,
    Speed10M = filtered_series_df$speed_10m, Speed50M = filtered_series_df$speed_50m,
    SpeedEXT = filtered_series_df$velocEXT, SpeedINMET = NA
  )
  
  if (use_inmet_correction == "Yes") {
    corrected_series <- perform_bias_correction(
      correction_type, inmet_station_info, inmet_timeseries_data, 
      merra2_grid_points, filtered_series_df, tokens
    )
    processed_merra2_dataset$SpeedINMET <- corrected_series
    filtered_series_df$velocEXT <- corrected_series
  }
  
  final_aggregated_series <- aggregate_time_series_scale(time_scale, filtered_series_df)
  
  output_list <- list(
    Dados = processed_merra2_dataset, 
    Name_X = time_scale, 
    Serie = final_aggregated_series
  )
  
  return(output_list)
}