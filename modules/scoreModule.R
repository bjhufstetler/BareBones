# Score Module
# Uses: {shiny}

scoreUI <- function(id){
  ns <- NS(id)
  
  tags$div(
    style = "width:100%; text-align: center;",
    "Scores",
    tags$br(),
    "Player : Computer",
    tags$br(),
    verbatimTextOutput(outputId = "score_ui")
  )
}
