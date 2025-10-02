mapInputUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "map-container",
      
      tags$div(
        class = "input-col",
        
        tags$div(
          id = "close-map",
          shiny::icon("circle-xmark"),
          onclick = "hideMapContainer()",
          style = "
            color: #059669;
            font-size: x-large;
            position: relative;
            top: -10px;
            left: -5px;
            cursor: pointer;
          " 
        ),
        
        tags$div(
          class = "input-fields",
          
          
          tags$h4("Configuração do Parque Eólico"),
          
          tags$div(class = "form-group latlon-group",
                   tags$div(class = "latlon-item",
                            numericInput(ns("lat"), label = "Latitude:",
                                         value = -15.25,
                                         min = -34.25,
                                         max = 7.65,
                                         step = 0.01)
                   ),
                   tags$div(class = "latlon-item",
                            numericInput(ns("lon"), label = "Longitude:",
                                         value = -55.25,
                                         min = -75.95,
                                         max = -33.40,
                                         step = 0.01)
                   )
          ),
          
          dateRangeInput(ns("TamanhoSerie"), "Period:", start = "2021-01-01", end = "2025-12-30", min = "2000-01-01", max = "2030-12-30"),
          selectInput(ns("tempo"), "Temporal Scale:", c("Hour", "Day", "Month", "Year")),
          
          tags$div(class = "form-group", numericInput(ns("rotor_height"), label = "Wind Turbine Rotor Height:",
                                                      value = 100,
                                                      min = 30,
                                                      max = 200,
                                                      step = 1)
          ),
          
          tags$div(class = "form-group",
                   radioButtons(ns("UsarCorrecao"),
                                label = "Do you want to perform bias correction with INMET data?", 
                                choices = list("Yes" = "yes", "No" = "no"),
                                selected = "no",
                                inline = TRUE)
          ),
          
          uiOutput(ns("select_correction_type"))
        ),
        
        tags$div(class = "action-button-wrapper",
                 actionButton(ns("generate_series"), "Generate Wind Speed TimeSeries", class = "btn-success", icon = icon("chart-line"))
        )
      ),
      
      tags$div(
        class = "map-col",
        leafletOutput(ns("map"))
      ),
      
      tags$div(
        class = "info-overlay",
        uiOutput(ns("info_boxes"))
      )
    )
  )
}

mapInputServer <- function(id, lonlat_data, estacoes_data, dados_estacoes_data, tokens) {
  moduleServer(id, function(input, output, session) {
    
    rv <- reactiveValues(
      merra_point = NULL,
      inmet_station = NULL,
    )
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(minZoom = 3, maxZoom = 5)) %>%
        addAwesomeMarkers(lng = -55.25, lat = -15.25, label = "Wind Farm/Turbine", icon = awesomeIcons(icon = 'location-crosshairs', library = 'fa', markerColor = 'darkred')) %>%
        addAwesomeMarkers(lng = rv$merra_point$lon, lat = rv$merra_point$lat, label = "MERRA-2 Point", icon = awesomeIcons(icon = 'cloud', library = 'fa', markerColor = 'blue')) %>%
        addAwesomeMarkers(lng = rv$inmet_station$lon, lat = rv$inmet_station$lat, label = "INMET station", icon = awesomeIcons(icon = 'tower-broadcast', library = 'fa', markerColor = 'green')) %>%
        setView(lng = -55, lat = -15, zoom = 5)
    })
    
    observe({
      lat <- input$lat
      lon <- input$lon
      
      req(lat, lon)
      
      # Validação para garantir que as coordenadas estão dentro do limite do Brasil definido no numericInput
      valid_lat <- lat >= -34.25 && lat <= 7.65
      valid_lon <- lon >= -75.95 && lon <= -33.40
      
      if (valid_lat && valid_lon) {
        
        rv$merra_point <- find_nearest_neighbor(lon, lat, lonlat_data)
        rv$inmet_station <- find_nearest_neighbor(lon, lat, data.frame(long = estacoes_data$VL_LONGITUDE, lat = estacoes_data$VL_LATITUDE))
        
        leafletProxy("map", session) %>%
          clearMarkers() %>%
          addAwesomeMarkers(lng = lon, lat = lat, label = "Wind Farm/Turbine", icon = awesomeIcons(icon = 'location-crosshairs', library = 'fa', markerColor = 'darkred')) %>%
          addAwesomeMarkers(lng = rv$merra_point$lon, lat = rv$merra_point$lat, label = "MERRA-2 Point", icon = awesomeIcons(icon = 'cloud', library = 'fa', markerColor = 'blue')) %>%
          addAwesomeMarkers(lng = rv$inmet_station$lon, lat = rv$inmet_station$lat, label = "INMET station", icon = awesomeIcons(icon = 'tower-broadcast', library = 'fa', markerColor = 'green')) %>%
          setView(lng = lon, lat = lat, zoom = 5)
      }
      
    })
    
    output$info_boxes <- renderUI({
      req(rv$merra_point, rv$inmet_station)
      
      indice_inmet <- which(estacoes_data$VL_LONGITUDE == rv$inmet_station$lon & estacoes_data$VL_LATITUDE == rv$inmet_station$lat)[1]
      nome_estacao <- estacoes_data$DC_NOME[indice_inmet]
      
      dist_inmet_check <- if (rv$inmet_station$distance <= 40) {
        p("Suitable for bias correction",
        style = "
          margin-top: 8px;
          color: white;
          background: #059669;
          border-radius: inherit;
          font-weight: 500;
          font-size: 0.8rem;
          display: flex;
          justify-content: center;
          padding: 5px;
        "
        )
        
      }
      
      tags$div(
        class = "info-box",
        
        tags$div(class = "info-section-title", icon("cloud"), "MERRA-2"),
        p(strong("Coordinates:"), paste0(round(rv$merra_point$lon, 2), "°, ", round(rv$merra_point$lat, 2), "°")),
        p(strong("Distance:"), paste0(round(rv$merra_point$distance, 2), " km")),
        tags$hr(style = "margin: 12px 0; border: none; border-top: 1px solid #e2e8f0;"),
        
        tags$div(class = "info-section-title", icon("tower-broadcast"), "INMET"),
        p(strong("Nearest Station:"), nome_estacao),
        p(strong("Coordinates:"), paste0(round(rv$inmet_station$lon, 2), "°, ", round(rv$inmet_station$lat, 2), "°")),
        p(strong("Distance:"), paste0(round(rv$inmet_station$distance, 2), " km")),
        dist_inmet_check
      )
    })
    
    output$select_correction_type <- renderUI({
      if (input$UsarCorrecao == "yes") {
        selectInput(session$ns("TipoCorrecao"), "Type of Average Correction:",
                     c("Single Period", "Monthly", "Hourly", "Monthly and Hourly"), selected = "Hourly")
      }
    })
    
    generation_event <- eventReactive(input$generate_series, {
      req(rv$merra_point, rv$inmet_station)
      
      list(
        lat = input$lat,
        lon = input$lon,
        start_date = input$TamanhoSerie[1],
        end_date = input$TamanhoSerie[2],
        time_scale = input$tempo,
        rotor_height = input$rotor_height,
        use_correction = input$UsarCorrecao,
        correction_type = input$TipoCorrecao,
        merra_point = rv$merra_point,
        inmet_station = rv$inmet_station
      )
    })
    
    return(generation_event)
  })
}