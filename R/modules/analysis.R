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
      div(class = "analysis-container",
          div(class = "analysis-card",
              div(class = "method-btn-group",
                  actionButton(ns("btn_single"), label = "Single Period", icon = icon("circle"), class = "method-btn active"),
                  actionButton(ns("btn_monthly"), label = "Monthly", icon = icon("calendar-alt"), class = "method-btn"),
                  actionButton(ns("btn_hourly"), label = "Hourly", icon = icon("clock"), class = "method-btn"),
                  actionButton(ns("btn_monthly_hourly"), label = "Monthly & Hourly", icon = icon("calendar-check"), class = "method-btn")
              ),
              
              div(style = "margin-top: 20px;",
                  actionButton(ns("run_analysis"), "Apply Methodology", class = "btn-success", icon = icon("play"))
              )
          ),
          
          div(class = "plot-grid",
              div(class = "analysis-card",
                  div(class = "plot-filters",
                      div(id = ns("month_filter_panel"), style = "display: none;",
                          selectInput(ns("mes_selecionado"), "Selecione o Mês:",
                                      choices = month.name, selected = month.name[1])
                      ),
                      div(id = ns("hour_filter_panel"), style = "display: none;",
                          sliderInput(ns("hora_selecionada"), "Selecione a Hora:",
                                      min = 0, max = 23, value = 0, step = 1, width = "100%")
                      )
                  ),
                  hr(), # Linha separadora
                  fluidRow(
                    column(width = 6, elbowPlotUI(ns("elbow_module"))),
                    column(width = 6, scatterplotUI(ns("scatterplot_module")))
                  )
              ),
              
              div(class = "analysis-card",
                  densityPlotUI(ns("density_module"))
              )
          )
      )
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
      
      selected_methodology <- reactiveVal("Single Period")
      btn_ids <- c("btn_single", "btn_monthly", "btn_hourly", "btn_monthly_hourly")
      
      lapply(btn_ids, function(btn_id) {
        observeEvent(input[[btn_id]], {
          lapply(btn_ids, function(id) shinyjs::removeClass(id, "active"))
          shinyjs::addClass(btn_id, "active")
          
          method_value <- switch(btn_id,
                                 "btn_single" = "Single Period",
                                 "btn_monthly" = "Monthly",
                                 "btn_hourly" = "Hourly",
                                 "btn_monthly_hourly" = "Monthly and Hourly")
          selected_methodology(method_value)
        })
      })
      
      observe({
        method <- selected_methodology()
        is_monthly <- method %in% c("Monthly", "Monthly and Hourly")
        is_hourly <- method %in% c("Hourly", "Monthly and Hourly")
        
        shinyjs::toggleElement(id = "month_filter_panel", condition = is_monthly)
        shinyjs::toggleElement(id = "hour_filter_panel", condition = is_hourly)
        shinyjs::toggleElement(id = "filters_hr", condition = is_monthly || is_hourly)
      })
      
      analysis_results <- eventReactive(input$run_analysis, {
        req(rv_files$wind_speed_file$data, rv_files$wind_power_file$data, cancelOutput = TRUE)
        
        showNotification("Iniciando a análise de clusterização...", type = "message")
        
        dados_combinados <- combinar_dados_potencia_velocidade(
          dados_potencia_brutos = rv_files$wind_power_file$data,
          dados_velocidade_brutos = rv_files$wind_speed_file$data
        )
        
        if (is.null(dados_combinados) || nrow(dados_combinados) == 0) {
          showNotification("Erro: Não foi encontrada correspondência entre os dados.", type = "error", duration = 10)
          return(NULL)
        }
        
        resultados <- clusterizar_dados(dados_combinados, selected_methodology())
        showNotification("Análise concluída com sucesso!", type = "message")
        
        return(list(
          dados_originais = dados_combinados,
          resultados_cluster = resultados
        ))
      })
      
      observeEvent(generation_trigger(), {
        params <- generation_trigger() 
        
        notification_id <- showNotification("Generating time series, please wait...", duration = NULL, type = "message")
        on.exit(removeNotification(notification_id), add = TRUE) 
        
        generated_series_list <- generate_wind_speed_series(
          target_merra_lat = params$merra_point$lat, target_merra_lon = params$merra_point$lon,
          time_scale = params$time_scale, rotor_height = params$rotor_height,
          start_date = params$start_date, end_date = params$end_date,
          use_inmet_correction = params$use_correction, correction_type = params$correction_type,
          inmet_station_info = params$inmet_station, inmet_timeseries_data = dados_estacoes_data,
          merra2_grid_points = lonlat_data, tokens = tokens
        )
        
        dados_serie <- generated_series_list$Serie
        dados_gerados_padronizados <- dados_serie %>%
          dplyr::transmute(
            Data = as.Date(time),
            Hora = lubridate::hour(time),
            Velocidade = speed
          )
        
        rv_files[["wind_speed_file"]] <- list(
          data = dados_gerados_padronizados,
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
      
      definicoes_reativo_elbow <- reactive({
        resultados_completos <- analysis_results()
        req(resultados_completos)
        
        resultados_completos$resultados_cluster$definicoes_clusters
      })
      
      dados_filtrados_para_plot <- reactive({
        resultados_completos <- analysis_results()
        metodologia <- selected_methodology()
        req(resultados_completos)
        
        tabela_meses <- data.frame(
          nome_completo = month.name,
          nome_abreviado = c('jan', 'fev', 'mar', 'abr', 'mai', 'jun', 'jul', 'ago', 'set', 'out', 'nov', 'dez')
        )
        
        dados_orig <- resultados_completos$dados_originais
        atribuicoes <- resultados_completos$resultados_cluster$atribuicoes
        
        dados_orig$..original_row_index.. <- 1:nrow(dados_orig)
        
        dados_filtrados <- switch(metodologia,
          "Single Period" = { dados_orig },
          "Monthly" = {
            req(input$mes_selecionado)
            mes_filtrar <- tabela_meses$nome_abreviado[tabela_meses$nome_completo == input$mes_selecionado]
            dados_orig %>% dplyr::filter(Month == mes_filtrar)
          },
          "Hourly" = {
            req(input$hora_selecionada)
            dados_orig %>% dplyr::filter(Hour == input$hora_selecionada)
          },
          "Monthly and Hourly" = {
            req(input$mes_selecionado, input$hora_selecionada)
            mes_filtrar <- tabela_meses$nome_abreviado[tabela_meses$nome_completo == input$mes_selecionado]
            dados_orig %>% dplyr::filter(Month == mes_filtrar, Hour == input$hora_selecionada)
          }
        )
        
        validate(
          need(nrow(dados_filtrados) > 0, "Não existem dados para o grupo selecionado.")
        )
        
        indices_filtrados <- dados_filtrados$..original_row_index..
        atribuicoes_filtradas <- atribuicoes[indices_filtrados]
        
        dados_filtrados$cluster <- as.factor(atribuicoes_filtradas)
        
        dados_filtrados$..original_row_index.. <- NULL
        
        return(dados_filtrados)
      })
      
      scatterplotServer(
        "scatterplot_module",
        dados_para_plotar = dados_filtrados_para_plot 
        )
      
      densityPlotServer(
        "density_module",
        dados_para_plotar = dados_filtrados_para_plot
        )
      
      elbowPlotServer(
        id = "elbow_module",
        dados_cluster_definicoes = definicoes_reativo_elbow,
        metodologia_selecionada = selected_methodology,
        mes_selecionado = reactive(input$mes_selecionado),
        hora_selecionada = reactive(input$hora_selecionada)
      )
    })
}