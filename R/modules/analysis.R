analysisUI <- function(id) {
  ns <- NS(id)
  
  
  tagList(
    useShinyjs(),
    tags$div(
      class = "container-principal", 
      
      fluidRow(
        column(
          width = 12,
          tags$div(
            class = "header-caixa",
            tags$h3(class = "header-title", "WindAnalyzer"),
            
            tags$h3(class = "header-subtitle", "- An Interative Tool for Wind Energy Reanalysis in Brazil")
          )
        )
      ),
      
      tags$div(
        class = "conteudo-caixa",
        uiOutput(ns("main_content")),
        
        tags$div(
          id = "windanalyzer_map_container",
          class = "map-transition-container", 
          
          tags$div(
            class = "map-content-wrapper",
            mapInputUI(ns("map_input_module"))
          )
        ),
      ),
      
       
    )
  )
}

analysisServer <- function(id, lonlat_data, estacoes_data, dados_estacoes_data, tokens) {
    moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    file_input_ids <- c("wind_speed_file", "wind_power_file")
   
    rv_files <- reactiveValues()
    
    for (file_id in file_input_ids) {
      rv_files[[file_id]] <- list(
        data = NULL, 
        name = NULL,

        config = list(
          header = TRUE,
          delim = ";",
          dec = "."
        )
      )
    }
    
    generation_trigger <- mapInputServer(
      "map_input_module",
      lonlat_data = lonlat_data, estacoes_data = estacoes_data,
      dados_estacoes_data = dados_estacoes_data, tokens = tokens
    )
    
    observeEvent(generation_trigger(), {
      params <- generation_trigger() 
      
      notification_id <- showNotification("Generating time series, please wait...", duration = NULL, type = "message")
      on.exit(removeNotification(notification_id), add = TRUE) 
      
      generated_series <- generate_wind_speed_series(
        target_merra_lat = params$merra_point$lat, target_merra_lon = params$merra_point$lon,
        time_scale = params$time_scale, rotor_height = params$rotor_height,
        start_date = params$start_date, end_date = params$end_date,
        use_inmet_correction = params$use_correction, correction_type = params$correction_type,
        inmet_station_info = params$inmet_station, inmet_timeseries_data = dados_estacoes_data,
        merra2_grid_points = lonlat_data, tokens = tokens
      )
      
      rv_files[["wind_speed_file"]] <- list(
        data = generated_series,
        name = paste0("Generated series: Lat ", round(params$lat, 2), ", Lon ", round(params$lon, 2))
      )
      
      showNotification("Time series generated successfully!", type = "message", duration = 5)
      runjs("hideMapContainer();")
    })
    
    active_config_id <- reactiveVal(NULL)
    
    lapply(file_input_ids, function(id) {
      observeEvent(input[[paste0(id, "_config")]], {
        
        active_config_id(id)
        
        current_config <- rv_files[[id]]$config
        
        showModal(modalDialog(
          title = "CSV Import Settings",
          
          checkboxInput(ns("csv_header"), "File has a header row", value = current_config$header),
          
          radioButtons(ns("csv_delim"), "Column Separator (Delimiter)",
                       choices = c("Comma (,)" = ",", "Semicolon (;)" = ";", "Tab" = "\t"),
                       selected = current_config$delim, inline = TRUE),
          
          radioButtons(ns("csv_dec"), "Decimal Separator",
                       choices = c("Dot (.)" = ".", "Comma (,)" = ","),
                       selected = current_config$dec, inline = TRUE),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save_csv_config"), "Apply and Save", class = "btn-success")
          ),
          easyClose = TRUE
        ))
      })
    })
    
    observeEvent(input$save_csv_config, {
      req(active_config_id()) 
      id <- active_config_id()
      
      rv_files[[id]]$config$header <- input$csv_header
      rv_files[[id]]$config$delim <- input$csv_delim
      rv_files[[id]]$config$dec <- input$csv_dec
      
      removeModal() 
      showNotification(paste("Settings for", id, "updated."), type = "message")
    })
    
    lapply(file_input_ids, function(id) {
      observeEvent(input[[id]], {
        file_input_value <- input[[id]]
        req(file_input_value)
        
        config <- rv_files[[id]]$config
        
        tryCatch({
          dados_do_arquivo <- readr::read_delim(
            file = file_input_value$datapath,
            delim = config$delim,
            col_names = config$header,
            locale = readr::locale(decimal_mark = config$dec)
          )
          
          rv_files[[id]]$data <- dados_do_arquivo
          rv_files[[id]]$name <- file_input_value$name
          
          showNotification(paste("File '", file_input_value$name, "' uploaded successfully!"), type = "message")
          
        }, error = function(e) {
          showNotification(paste("Error reading file:", e$message), type = "error")
        })
      })
    })
    
    output$main_content <- renderUI({
      labels <- list(
        wind_speed_file = "Historical Wind Speed Data at Turbine Rotor Height",
        wind_power_file = "Historical Wind Power Data"
      )
      
      ui_components <- lapply(file_input_ids, function(id) {
        
        if (id == "wind_speed_file") {
          tags$div(
            class = "upload-wrapper",
            customFileInputUI(
              inputId = ns(id),
              label = labels[[id]],
              loaded_filename = rv_files[[id]]$name
            ),
            tags$div(
              style = "padding: 0 5px; font-size: 0.75rem; color: #555;",
              tags$span("* Don't have a file? "),
              tags$a(
                "Generate a historical data series here.", href = "#",
                onclick = "showMapContainer(); return false;",
                style = "color: #16a34a; font-weight: 500; text-decoration: underline;"
              )
            )
          )
        } else {
          tags$div(
            class = "upload-wrapper",
            customFileInputUI(
              inputId = ns(id),
              label = labels[[id]],
              loaded_filename = rv_files[[id]]$name
            )
          )
        }
      })
      
      tags$div(
        class = "upload-container",
        ui_components
      )
    })
    
    })
}