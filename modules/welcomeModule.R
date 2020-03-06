# Welcome Module
# Uses: {base, shiny}

welcomeUI <- function(id){
  ns <- shiny::NS(id)
  shiny::modalDialog(
    title = tags$h1(
      style = "text-align: center;",
      "Welcome to BareBones!"
    ),
    tags$div(
      style = "text-align: center;",
      tags$p("Challenge the AI by"),
      tags$p("clicking the button below")
    ),
    footer = shiny::actionButton(
      inputId = ns("play"),
      label = "Play !",
      icon = shiny::icon("play"),
      style = "width: 100%"
    )
  )
}

welcome <- function(input, output, session){
  id <- base::gsub("-$", "", session$ns(""))
  shiny::showModal(ui = welcomeUI(id))
  
  shiny::observeEvent(input$play, {
    shiny::removeModal()
  })
  
  return(shiny::reactive(input$play))
}