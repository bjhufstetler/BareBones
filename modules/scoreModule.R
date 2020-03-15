# Score Module
# Uses: {shiny}

scoreUI <- function(id){
  ns <- NS(id)
  
  tags$div(
    style = "width:100%; text-align: center;",
    tags$h4("Scores"),
    "White : Black",
    tags$br(),
    verbatimTextOutput(outputId = "score_ui")
  )
}
