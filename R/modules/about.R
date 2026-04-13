aboutUI <- function(id) {
  ns <- NS(id)
  tags$div(class = "container-principal",
           tags$div(class = "analysis-card", style = "max-width: 1000px; margin: 0 auto; padding: 40px;",
                    # Cabeçalho da Página
                    tags$div(style = "text-align: center; margin-bottom: 40px;",
                             tags$h2("About WindAnalyzer", style = "color: #2c3e50; font-weight: bold;"),
                             tags$hr(style = "width: 100px; border-top: 3px solid #16a34a; margin: 20px auto;")
                    ),  
                    
                    # Author Highlight Box (Adicionado antes do Abstract)
                      div(
                        style = "background-color: #f8fafc; border-left: 5px solid #16a34a; padding: 25px; margin-bottom: 35px; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",
                        tags$h4("Authors & Affiliation", style = "margin-top: 0; color: #0f172a; font-weight: 700; text-transform: uppercase; font-size: 0.9rem; letter-spacing: 1px;"),
                        tags$p(
                          tags$strong("Saulo Custodio, Fernando Cyrino, Paula Maçaira, Gabriel Rosas and Gustavo Melo"),
                          tags$br(),
                          "Pontifícia Universidade Católica do Rio de Janeiro (PUC-Rio)",
                          style = "margin-bottom: 0; color: #1e293b;"
                        )
                      ),
          
                    
                    # 1: Apresentação do Projeto Científico
                    tags$div(class = "about-section",
                             tags$h3("Abstract", style = "color: #2c3e50; border-bottom: 2px solid #e2e8f0; padding-bottom: 10px;"),
                             p("Brazil has always had its electrical matrix based mainly on renewable sources, specifically hydro. Over the years, this has diversified and demonstrated a greater participation of wind sources. To better explore it, research aimed at modeling its behavior is essential. However, it is only sometimes that data on wind speed and wind generation is available in quantity and the locations of interest. This data is necessary for identifying potential locations for installing wind farms, improving the performance of existing ones, and stimulating research into forecasting and simulating wind generation, which are inputs to help improve the planning and operation of the Brazilian electricity sector."),
                             p("In the absence of wind speed data, an alternative is to use data from a reanalysis database. They provide long histories of data on climatic and atmospheric variables for different parts of the world, free of charge. Therefore, the first contribution of this work focused on verifying the representativeness of wind speed data made available by MERRA-2 in Brazilian territory. Following literature recommendations, interpolation, extrapolation, and bias correction techniques were used to improve the adequacy of the speeds provided by the reanalysis based on those that occur at the height of the wind farm turbine rotors. In the WIND SPEED tab, the time series of wind speed is available at any point in Brazilian territory after processing the data coming from MERRA-2 using the techniques (interpolation, extrapolation, and bias correction) suggested in the first contribution of this work."),
                             p("The second contribution proposes modeling the relationship between wind speed and wind generation in a stochastic and nonparametric way based on historical data for both variables. For this purpose, clustering techniques using K-Means, estimation of density curves using KDE, and Monte Carlo simulation were used. In the WIND POWER tab, it is possible to develop the relationship between speed and power for any location in Brazil by simply providing a history of both variables or just wind generation; in the latter case, data from MERRA-2 is used to build the history of wind speed. It is possible to generate future generation scenarios by projecting wind speeds.")
                    ),
                  
                    tags$hr(style = "margin: 30px 0; border-color: #e2e8f0;"),
                    
                    # 2. Methodology Overview (Baseado na explicação de Cluster e KDE)
                    tags$h3("2. Methodology Overview", style = "color: #0f172a; margin-bottom: 15px; font-weight: 600;"),
                    tags$p("The application allows users to group historical generation data based on combinations of variables, such as month and hour. The generation profiles for each clustered group form probability density functions, which are defined using Kernel Density Estimation (KDE). This non-parametric approach smooths the data, creating continuous probability curves that accurately represent the power generation characteristics of each specific cluster."),
                    
                    tags$hr(style = "margin: 30px 0; border-color: #e2e8f0;"),
                    
                    # 3. Generation Projection (Baseado na última imagem enviada)
                    tags$h3("3. Generation Projection", style = "color: #0f172a; margin-bottom: 15px; font-weight: 600;"),
                    tags$p("Based on the KDE distributions formed for each cluster, the tool enables the generation of future scenarios. By providing a projected wind speed time series and specifying the desired number of scenarios, WindAnalyzer extracts stochastic percentiles from these distributions."),
                    tags$p("This process yields multiple possible power generation trajectories, effectively capturing the intrinsic uncertainty of the wind. The final output is presented as a time series that highlights the mean estimate along with a 90% confidence interval across the generated scenarios, allowing for a robust risk assessment."),
                    
                    # 4. References & Papers (Nova seção adicionada ao fim)
                    tags$h3("4. References & Related Papers", style = "color: #0f172a; margin-bottom: 15px; font-weight: 600;"),
                    tags$p("For more detailed information regarding the mathematical foundations and the datasets used in this project, please refer to the following publications:"),
                    tags$ul(
                      style = "list-style-type: none; padding-left: 0;",
                      tags$li(
                        style = "margin-bottom: 12px; display: flex; align-items: flex-start; gap: 10px;",
                        shiny::icon("file-pdf", style = "color: #dc2626; margin-top: 5px;"),
                        tags$div(
                          "Ferreira, S. C. A; Cyrino Oliveira, F. L.; Maçaira, P. M. Validation of the representativeness of wind speed time series obtained from reanalysis data for Brazilian territory. v. 258. p. 124746. Energy. 2022. DOI:",
                          tags$a(href="https://doi.org/10.1016/j.energy.2022.124746", "https://doi.org/10.1016/j.energy.2022.124746", target = "_blank", style = "color: #16a34a; font-weight: 500; text-decoration: underline;")
                        )
                      ),
                      tags$li(
                        style = "margin-bottom: 12px; display: flex; align-items: flex-start; gap: 10px;",
                        shiny::icon("file-pdf", style = "color: #dc2626; margin-top: 5px;"),
                        tags$div(
                          "Ferreira, S. C. A; Cyrino Oliveira, F. L.; Maçaira, P. M. Joint Modeling of Wind Speed and Power via a Nonparametric Approach. Energies 2024, 17, 3573.",
                          tags$a(href = "https://doi.org/10.3390/en17143573", "https://doi.org/10.3390/en17143573", target = "_blank", style = "color: #16a34a; font-weight: 500; text-decoration: underline;")
                        )
                      )
                    )
           )
  )
}

aboutServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}