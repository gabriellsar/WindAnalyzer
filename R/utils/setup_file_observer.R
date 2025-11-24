#' Configura os observadores para inputs de arquivo com configurações customizáveis
#' @param input O objeto input do módulo shiny
#' @param ns A função de namespace do módulo atual
#' @param rv O objeto reactiveValues onde os dados e configs estão armazenados
#' @param id_base O ID base do input de arquivo (ex: "wind_speed_file")
#' @param label_modal O título para exibir no modal de configurações
setup_file_observers <- function(input, ns, rv, id_base, label_modal = "File Settings") {
  
  # 1. Observador do Botão de Configuração (Abre o Modal)
  # O ID do botão deve ser id_base + "_config" (definido no customFileInputUI)
  shiny::observeEvent(input[[paste0(id_base, "_config")]], {
    
    # Recupera a configuração atual salva no reactiveValues
    config_atual <- rv[[id_base]]$config
    
    shiny::showModal(shiny::modalDialog(
      title = paste(label_modal, "-", "CSV Options"),
      
      # Checkbox para Cabeçalho
      shiny::checkboxInput(ns(paste0(id_base, "_header")), "File has Header", value = config_atual$header),
      
      # Radio Buttons para Separador
      shiny::radioButtons(ns(paste0(id_base, "_delim")), "Column Separator:",
                          choices = c("Semicolon (;)" = ";", "Comma (,)" = ",", "Tab" = "\t"),
                          selected = config_atual$delim, inline = TRUE),
      
      # Radio Buttons para Decimal
      shiny::radioButtons(ns(paste0(id_base, "_dec")), "Decimal Separator:",
                          choices = c("Dot (.)" = ".", "Comma (,)" = ","),
                          selected = config_atual$dec, inline = TRUE),
      
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(ns(paste0(id_base, "_save_config")), "Save Settings", class = "btn-success")
      ),
      easyClose = TRUE
    ))
  })
  
  # 2. Observador do Botão Salvar (Dentro do Modal)
  shiny::observeEvent(input[[paste0(id_base, "_save_config")]], {
    
    # Atualiza o reactiveValues com as novas escolhas
    rv[[id_base]]$config$header <- input[[paste0(id_base, "_header")]]
    rv[[id_base]]$config$delim  <- input[[paste0(id_base, "_delim")]]
    rv[[id_base]]$config$dec    <- input[[paste0(id_base, "_dec")]]
    
    shiny::removeModal()
    shiny::showNotification(paste("Settings for", id_base, "updated."), type = "message")
  })
  
  # 3. Observador do Upload do Arquivo
  shiny::observeEvent(input[[id_base]], {
    file_info <- input[[id_base]]
    shiny::req(file_info)
    
    # Pega a configuração ATUAL (que pode ter sido alterada acima)
    cfg <- rv[[id_base]]$config
    
    tryCatch({
      df <- readr::read_delim(
        file = file_info$datapath,
        delim = cfg$delim,
        col_names = cfg$header,
        locale = readr::locale(decimal_mark = cfg$dec),
        show_col_types = FALSE
      )
      
      # Validação básica: se leu apenas 1 coluna, provavelmente o separador está errado
      if (ncol(df) <= 1) {
        shiny::showNotification("Warning: File read with only 1 column. Check separator settings.", type = "warning", duration = 10)
      }
      
      # Salva no objeto reativo principal
      rv[[id_base]]$data <- df
      rv[[id_base]]$name <- file_info$name
      
      shiny::showNotification(paste("File '", file_info$name, "' loaded successfully!"), type = "message")
      
    }, error = function(e) {
      shiny::showNotification(paste("Error reading file:", e$message), type = "error")
      rv[[id_base]]$data <- NULL # Limpa dados antigos em caso de erro
    })
  })
}