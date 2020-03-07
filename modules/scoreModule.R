# Score Module
# Uses: {base, shiny}

scoreUI <- function(id){
  ns <- shiny::NS(id)
  
  tags$div(
    style = "width:100%; text-align: center;",
    "Scores",
    tags$br(),
    "Player : Computer",
    tags$br(),
    shiny::verbatimTextOutput(outputId = "score_ui")
  )
  
}
