heroSectionUI <- function(id) {
  ns <- NS(id)
  
  tags$section(class = "hero",
               tags$div(class = "container",
                        tags$h1("WindAnalyzer"),
                        tags$p("An Interative Tool for Wind Energy Reanalysis in Brazil"),
                        actionButton(
                          inputId = ns("cta_button"),
                          class = "cta-button",
                          label = tagList(
                            tags$svg(
                              class = "cta-icon", viewBox = "0 0 24 24", fill = "none",
                              stroke = "currentColor", `stroke-width` = "2",
                              tags$path(d = "M13 2L3 14h9l-1 8 10-12h-9l1-8z")
                            ),
                            "Start Analysis"
                          )
                        )
               )
  )
}

heroSectionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(
      reactive(input$cta_button)
    )
  })
}