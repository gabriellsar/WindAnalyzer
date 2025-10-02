aboutUI <- function(id) {
  ns <- NS(id)
  
  tags$div(
    class = "container",
    style = "margin-top: 100px; color: #000000;",
    
    tags$h2("Sobre o WindAnalyzer"),
    tags$p("Este projeto foi desenvolvido como uma ferramenta interativa para análise de dados de reanálise de vento no Brasil, utilizando a base de dados MERRA-2."),
    tags$p("A metodologia empregada é baseada em pesquisas científicas validadas, oferecendo uma plataforma robusta para tomada de decisão no setor de energia eólica."),
    br(),
    tags$p("Desenvolvido por: [Seu Nome/Sua Organização]")
  )
}

aboutServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}