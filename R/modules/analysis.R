analysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    tags$div(
      class = "container-principal", 
      
      fluidRow(column(12, tags$div(class = "header-caixa",
              tags$h3(class = "header-title", "WindAnalyzer"),
              tags$h3(class = "header-subtitle", "- An Interative Tool for Wind Energy Reanalysis in Brazil")
      ))),
      
      tags$div(class = "conteudo-caixa",
        uiOutput(ns("main_content")),
        tags$div(id = "windanalyzer_map_container", class = "map-transition-container", 
                tags$div(class = "map-content-wrapper", mapInputUI(ns("map_input_module")))
        ),
      ),
      div(class = "analysis-container",
          div(class = "analysis-card",
              tags$h4("1. Select Methodology", style = "font-size: 1rem; color: #64748b; margin-bottom: 15px; font-weight: 600;"),
              
              div(class = "segmented-control",
                  actionButton(ns("btn_single"), label = "Single Period", icon = icon("circle"), class = "segment-btn active"),
                  actionButton(ns("btn_monthly"), label = "Monthly", icon = icon("calendar-alt"), class = "segment-btn"),
                  actionButton(ns("btn_hourly"), label = "Hourly", icon = icon("clock"), class = "segment-btn"),
                  actionButton(ns("btn_monthly_hourly"), label = "Monthly & Hourly", icon = icon("calendar-check"), class = "segment-btn")
              ),
              
              div(style = "margin-top: 25px;",
                  actionButton(ns("run_analysis"), "Apply Methodology", class = "btn-run-analysis", icon = icon("play"))
              )
          ),
        
          div(class = "plot-grid",
            div(class = "analysis-card",
                div(id = ns("filter_panel"), class = "filter-toolbar", style = "display: none;",
                    div(id = ns("month_filter_panel"), class = "filter-item", style = "display: none;",
                        tags$span("Month:", class = "control-label"),
                        selectInput(ns("mes_selecionado"), label = NULL,
                                    choices = month.name, selected = month.name[1], width = "160px")
                    ),
                    div(id = ns("hour_filter_panel"), class = "filter-item", style = "display: none;",
                        tags$span("Hour:", class = "control-label"),
                        sliderInput(ns("hora_selecionada"), label = NULL,
                                    min = 0, max = 23, value = 0, step = 1, width = "250px")
                    )
                ),
              
                hr(),
                fluidRow(
                  column(width = 6, elbowPlotUI(ns("elbow_module"))),
                  column(width = 6, scatterplotUI(ns("scatterplot_module")))
                )),
          div(class = "analysis-card",
              densityPlotUI(ns("density_module"))
          )
        ),
        
        div(class = "analysis-card",
          tags$h4("Model Validation (In-Sample)", 
                  style = "font-size: 0.9rem; color: var(--text-color-light); margin-bottom: 15px; font-weight: 600; text-transform: uppercase; letter-spacing: 0.5px;"),
          fluidRow(
            column(6, validationScatterUI(ns("val_scatter_module"))),
            column(6, validationDensityUI(ns("val_density_module")))
          )
        ),
        div(class = "analysis-card",
            tags$h4("Future Projection", 
                    style = "font-size: 0.9rem; color: var(--text-color-light); margin-bottom: 15px; font-weight: 600; text-transform: uppercase; letter-spacing: 0.5px;"),
            
            # A classe flex-row-stretch garante que ambas as colunas tenham a mesma altura
            fluidRow(class = "flex-row-stretch",
                     column(4,
                            div(class = "side-panel-controls",
                                # Módulo de Upload já customizado
                                uiOutput(ns("projection_file_ui")),
                                
                                tags$hr(style = "margin: 15px 0 20px 0; border-color: var(--card-border-color);"),
                                
                                # Input Numérico Moderno
                                div(class = "modern-input-group",
                                    tags$span("NUMBER OF SCENARIOS:", class = "control-label"),
                                    numericInput(ns("num_cenarios"), label = NULL, value = 100, min = 10, max = 1000, width = "100%")
                                ),
                                
                                # Reutilizamos o botão gigante de CTA para dar destaque
                                actionButton(ns("run_projection"), "Generate Projection", class = "btn-run-analysis", style = "margin-top: auto;", icon = icon("bolt"))
                            )
                     ),
                     
                     column(8, 
                            projectionSeriesUI(ns("projection_module"))
                     )
            )
        )
    )
  )
)}

analysisServer <- function(id, lonlat_data, estacoes_data, dados_estacoes_data, tokens) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Gestão de Arquivos de Upload
    rv_files <- reactiveValues(
      wind_speed_file = list(data = NULL, name = NULL, config = list(header = TRUE, delim = ";", dec = ".")),
      wind_power_file = list(data = NULL, name = NULL, config = list(header = TRUE, delim = ";", dec = ".")),
      projection_speed_file = list(data = NULL, name = NULL, config = list(header = TRUE, delim = ";", dec = "."))
    )
    
    setup_file_observers(input, ns, rv_files, "wind_speed_file", "Wind Speed Settings")
    setup_file_observers(input, ns, rv_files, "wind_power_file", "Power Data Settings")
    setup_file_observers(input, ns, rv_files, "projection_speed_file", "Projection Settings")
    
    output$main_content <- renderUI({
      labels <- list(
        wind_speed_file = "Historical Wind Speed Data at Turbine Rotor Height",
        wind_power_file = "Historical Wind Power Data"
      )
      div(class = "upload-container",
          div(class = "upload-wrapper", 
              customFileInputUI(ns("wind_speed_file"), label = labels$wind_speed_file, loaded_filename = rv_files$wind_speed_file$name),
              div(style = "padding: 0 5px; font-size: 0.75rem; color: #555;",
                  span("* Don't have a file? "),
                  tags$a("Generate a historical data series here.", href = "#", onclick = "showMapContainer(); return false;", style = "color: #16a34a; font-weight: 500; text-decoration: underline;")
              )
          ),
          div(class = "upload-wrapper", 
              customFileInputUI(ns("wind_power_file"), label = labels$wind_power_file, loaded_filename = rv_files$wind_power_file$name)
          )
      )
    })
    
    output$projection_file_ui <- renderUI({
      customFileInputUI(ns("projection_speed_file"), 
        label = "Upload Future Wind Speed (CSV)", 
        loaded_filename = rv_files$projection_speed_file$name)
    })
    
    # 2. Controle da Metodologia
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
      show_toolbar <- is_monthly || is_hourly
      
      shinyjs::toggleElement(id = "filter_panel", condition = show_toolbar)
      shinyjs::toggleElement(id = "month_filter_panel", condition = is_monthly)
      shinyjs::toggleElement(id = "hour_filter_panel", condition = is_hourly)
    })
    
    # 3. Processamento da Metodologia
    analysis_results <- eventReactive(input$run_analysis, {
      req(rv_files$wind_speed_file$data, rv_files$wind_power_file$data, cancelOutput = TRUE)
      showNotification("Starting analysis...", type = "message")
      
      dados_combinados <- combinar_dados_potencia_velocidade(
        dados_potencia_brutos = rv_files$wind_power_file$data,
        dados_velocidade_brutos = rv_files$wind_speed_file$data
      )
      if (is.null(dados_combinados) || nrow(dados_combinados) == 0) {
        showNotification("Error: No data match found.", type = "error", duration = 10)
        return(NULL)
      }
      
      metodo <- selected_methodology()
      resultados <- clusterizar_dados(dados_combinados, metodo)
      
      if (is.null(resultados$atribuicoes) || length(resultados$atribuicoes) != nrow(dados_combinados)) {
        showNotification("Clustering failed: Could not assign clusters correctly.", type = "error")
        return(NULL)
      }
      
      dados_combinados$cluster <- as.numeric(resultados$atribuicoes)
      if(any(is.na(dados_combinados$cluster))) {
        showNotification("Warning: Some points could not be clustered.", type = "warning")
        dados_combinados <- dados_combinados[!is.na(dados_combinados$cluster), ]
      }
      
      modelos_kde <- criar_modelos_kde(dados_combinados, metodo)
      
      matriz_simulacao <- simular_potencia_kde(
        dados_para_simular = dados_combinados,
        tabela_modelos_kde = modelos_kde,
        tabela_definicoes_clusters = resultados$definicoes_clusters,
        metodologia = metodo,
        total_cenarios = 100
      )
      dados_combinados$estimado <- rowMeans(matriz_simulacao, na.rm = TRUE)
      showNotification("Analysis completed!", type = "message")
      
      return(list(
        dados_originais = dados_combinados,
        resultados_cluster = resultados,
        modelos_kde = modelos_kde,
        metodo_atual = metodo
      ))
    })
    
    # 4. Geração de Série
    generation_trigger <- mapInputServer(
      "map_input_module",
      lonlat_data = lonlat_data, estacoes_data = estacoes_data,
      dados_estacoes_data = dados_estacoes_data, tokens = tokens
    )
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
    
    # 5. Output de Gráficos
    definicoes_reativo_elbow <- reactive({
      rev <- analysis_results()
      req(rev)
      rev$resultados_cluster$definicoes_clusters
    })
    dados_filtrados_para_plot <- reactive({
      rev <- analysis_results()
      req(rev)
      
      dados_orig <- rev$dados_originais
      atribuicoes <- rev$resultados_cluster$atribuicoes
      metodologia <- rev$metodo_atual
      
      tabela_meses <- data.frame(
        nome_completo = month.name,
        nome_abreviado = c('jan', 'fev', 'mar', 'abr', 'mai', 'jun', 'jul', 'ago', 'set', 'out', 'nov', 'dez')
      )
      
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
        need(nrow(dados_filtrados) > 0, "There is no data for the selected group.")
      )
      
      indices_filtrados <- dados_filtrados$..original_row_index..
      atribuicoes_filtradas <- atribuicoes[indices_filtrados]
      
      dados_filtrados$cluster <- as.factor(atribuicoes_filtradas)
      dados_filtrados$..original_row_index.. <- NULL
      
      return(dados_filtrados)
    })
    dados_validacao <- reactive({
      res <- analysis_results(); req(res)
      df <- res$dados_originais
      
      hist <- df %>% dplyr::select(speed, power) %>% dplyr::mutate(origin = "Historical")
      est <- df %>% dplyr::select(speed) %>% dplyr::mutate(power = df$estimado, origin = "Estimated") 
      dplyr::bind_rows(hist, est)
    })
    
    scatterplotServer("scatterplot_module",dados_para_plotar = dados_filtrados_para_plot)
    densityPlotServer("density_module",dados_para_plotar = dados_filtrados_para_plot)
    elbowPlotServer("elbow_module", definicoes_reativo_elbow,reactive(analysis_results()$metodo_atual),reactive(input$mes_selecionado),reactive(input$hora_selecionada))
    validationScatterServer("val_scatter_module", dados_validacao)
    validationDensityServer("val_density_module", dados_validacao)
    
    # Projeção Futura
    projection_raw <- eventReactive(input$run_projection, {
      req(analysis_results(), rv_files$projection_speed_file$data)
      
      treino <- analysis_results()
      input_futuro <- rv_files$projection_speed_file$data
      
      showNotification("Processing projection file...", type = "message")
      tem_tres_colunas <- ncol(input_futuro) >= 3
      
      if (tem_tres_colunas) {
        nomes_cols <- names(input_futuro)
        names(input_futuro)[1] <- "Data_Base"
        names(input_futuro)[2] <- "Hora_Num"
        names(input_futuro)[3] <- "Velocidade_Raw"
        
        datas_base <- tryCatch(
          as.POSIXct(input_futuro$Data_Base, tz="UTC"),
          error = function(e) as.POSIXct(lubridate::parse_date_time(input_futuro$Data_Base, c("ymd", "dmy", "mdy", "Ymd", "dmY")), tz="UTC")
        )
        
        datas_convertidas <- datas_base + lubridate::hours(as.numeric(input_futuro$Hora_Num))
        velocidade_limpa <- as.numeric(input_futuro$Velocidade_Raw)
        
      } else {
        if(!"Data" %in% names(input_futuro)) names(input_futuro)[1] <- "Data"
        if(!"Velocidade" %in% names(input_futuro)) names(input_futuro)[2] <- "Velocidade"
        
        datas_convertidas <- tryCatch(
          as.POSIXct(input_futuro$Data, tz="UTC"),
          error = function(e) as.POSIXct(lubridate::parse_date_time(input_futuro$Data, c("ymd", "dmy", "mdy", "Ymd", "dmY")), tz="UTC")
        )
        velocidade_limpa <- as.numeric(input_futuro$Velocidade)
      }
      
      # --- 2. Limpeza e Montagem do DataFrame ---
      velocidade_limpa[velocidade_limpa < 0 | velocidade_limpa > 25] <- NA
      
      mean_v <- mean(velocidade_limpa, na.rm = TRUE)
      if(is.na(mean_v)) mean_v <- 0
      velocidade_limpa[is.na(velocidade_limpa)] <- mean_v
      
      df_proj <- data.frame(data = datas_convertidas, speed = velocidade_limpa) %>% 
        dplyr::mutate(Hour = lubridate::hour(data)) %>%
        dplyr::arrange(data)
      
      # --- 3. Correção dos Blocos (Mapeamento de Meses) ---
      meses_treino_exemplo <- unique(treino$resultados_cluster$definicoes_clusters$Month)
      mes_num_proj <- lubridate::month(df_proj$data)
      
      if(is.numeric(meses_treino_exemplo[1])) {
        df_proj$Month <- mes_num_proj
      } else { 
        meses_pt_sistema <- c('jan', 'fev', 'mar', 'abr', 'mai', 'jun', 'jul', 'ago', 'set', 'out', 'nov', 'dez')
        df_proj$Month <- meses_pt_sistema[mes_num_proj]
      }
      
      # --- 4. Classificação e Simulação ---
      clusters_validos_kde <- unique(treino$modelos_kde$cluster)
      defs <- treino$resultados_cluster$definicoes_clusters %>%
        dplyr::filter(cluster %in% clusters_validos_kde)
      
      metod <- treino$metodo_atual
      
      df_proj$cluster <- purrr::pmap_dbl(
        list(df_proj$speed, df_proj$Month, df_proj$Hour),
        function(s, m, h) {
          encontrar_cluster_para_velocidade(s, m, h, metod, defs)
        }
      )
      
      matriz_cenarios <- simular_potencia_kde(
        dados_para_simular = df_proj,
        tabela_modelos_kde = treino$modelos_kde,
        tabela_definicoes_clusters = defs,
        metodologia = treino$metodo_atual,
        total_cenarios = input$num_cenarios
      )
      
      # --- 5. Corte de Limites (Clamping) ---
      max_potencia_treino <- max(treino$dados_originais$power, na.rm = TRUE)
      if(is.infinite(max_potencia_treino)) max_potencia_treino <- 100000 
      
      matriz_cenarios[matriz_cenarios < 0] <- 0
      matriz_cenarios[matriz_cenarios > max_potencia_treino] <- max_potencia_treino
      
      list(df_resultado = df_proj, matriz_cenarios = matriz_cenarios)
    })
    projection_plot <- reactive({
      req(projection_raw())
      raw <- projection_raw()
      
      # Cálculos vetorizados diretos na matriz (muito mais rápidos e econômicos em RAM)
      media_cenarios <- rowMeans(raw$matriz_cenarios, na.rm = TRUE)
      p05 <- apply(raw$matriz_cenarios, 1, stats::quantile, probs = 0.05, na.rm = TRUE)
      p95 <- apply(raw$matriz_cenarios, 1, stats::quantile, probs = 0.95, na.rm = TRUE)
      
      # Retorna apenas um data.frame leve com a faixa de confiança e a média
      df_resumo <- data.frame(
        data = raw$df_resultado$data,
        Mean = media_cenarios,
        Lower = p05,
        Upper = p95
      )
      
      return(df_resumo)
    })
    projectionSeriesServer("projection_module", projection_plot)
  })
}