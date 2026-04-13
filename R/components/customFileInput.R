customFileInputUI <- function(inputId, label, placeholder_text = "Click to select or drag and drop here", loaded_filename = NULL, ...) {
  
  ns <- NS(NULL) # Garante compatibilidade se usado fora de módulos
  
  tags$div(
    class = "upload-wrapper",
    
    # Cabeçalho com Título e Ícones de Config/Info
    tags$div(
      style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 5px;",
      tags$h5(label, class = "upload-title"),
      
      tags$div(
        class = "title-icons",
        style = "display: flex; align-items: center; gap: 8px;",
        
        actionButton(
          inputId = paste0(inputId, "_config"),
          label = NULL,
          icon = icon("gear"),
          style = "background: transparent; border: none; padding: 0; font-size: 0.9rem;"
        ),
        
        tags$span(
          id = paste0(inputId, "_info"),
          style = "cursor: help; color: var(--text-color-light);",
          icon("circle-info")
        )
      )
    ),
    
    if (!is.null(loaded_filename)) {
      tags$div(
        class = "file-input-wrapper loaded",
        icon("check"),
        tags$span(class = "file-name-text", loaded_filename)
      )
    } 
    else {
      tags$label(
        class = "file-input-label",
        `for` = inputId, 
        
        icon("cloud-arrow-up"),
        tags$span(placeholder_text),
        
        # Input real escondido
        tags$div(
          style = "display: none;",
          fileInput(
            inputId = inputId, 
            label = NULL, 
            buttonLabel = NULL,
            placeholder = NULL,
            width = "100%",
            ...
          )
        )
      )
    }
  )
}