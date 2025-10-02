customFileInputUI <- function(inputId, label, placeholder_text = "Click to select or drag and drop here", loaded_filename = NULL, ...) {
  
  tags$div(
    class = "upload-wrapper",
    
    tags$div(
      style = "display: flex; flex-direction: row; align-items: center; justify-content: space-between; cursor: pointer",
      tags$h5(label, class = "upload-title"),
      
      tags$div(
        class = "title-icons",
        style = "color: #555; font-size: 1em; padding: 0 10px; gap: 10px; text-decoration: none; display: flex;align-items: center;", 
        
        actionButton(
          inputId = paste0(inputId, "_config"),
          label = NULL,
          icon = shiny::icon("gear"),
          style = "color: #555; background-color: transparent; border: none; box-shadow: none; padding: 0;"
        ),
        
        tags$span(
          id = paste0(inputId, "_info"),
          shiny::icon("circle-info")
        )
      )
    ),
    
    if (!is.null(loaded_filename)) {
      tags$div(
        class = "file-input-wrapper loaded",
        style = "background-color: #e8f5e9; border: 1px solid #16a34a;",
        
        shiny::icon("check-circle", style = "color: #16a34a;"),
        tags$span(loaded_filename, style = "color: #16a34a; font-weight: 500;")
      )
    } else {
      tags$label(
        class = "file-input-wrapper",
        `for` = inputId, 
        
        shiny::icon("arrow-up-from-bracket"),
        tags$span(placeholder_text),
        
        tags$div(style="display:none;", 
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