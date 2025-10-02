cardsSectionUI <- function(id) {
  ns <- NS(id)
  
  tags$section(class = "cards-section",
               tags$div(class = "container",
                        tags$div(class = "cards-grid",
                                 # Card 1
                                 tags$div(class = "card",
                                          tags$div(class = "card-icon", icon("wind")),
                                          tags$h3("Advanced Temporal Analysis"),
                                          tags$p("Process and visualize wind time series using MERRA-2 data with interpolation, extrapolation, and bias correction techniques to obtain precise insights into historical wind patterns.")
                                 ),
                                 # Card 2
                                 tags$div(class = "card",
                                          tags$div(class = "card-icon", icon("bolt")),
                                          tags$h3("Stochastic Modeling"),
                                          tags$p("Develop robust non-parametric models that capture the complex relationship between wind speed and power generation, allowing for accurate simulations of future scenarios.")
                                 ),
                                 # Card 3
                                 tags$div(class = "card",
                                          tags$div(class = "card-icon", icon("book-open")),
                                          tags$h3("Validated Methodology"),
                                          tags$p("Built on peer-reviewed research and scientifically validated methodologies, offering reliable results for decision-making in wind energy projects within the Brazilian context.")
                                 )
                        )
               )
  )
}

cardsSectionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}