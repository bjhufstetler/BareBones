# Welcome Module
# Uses: {base, shiny}

welcomeUI <- function(id){
  ns <- NS(id)
  modalDialog(
    title = tags$h1(
      style = "text-align: center;",
      "Welcome to BareBones!"
    ),
    tags$div(
      style = "text-align: center;",
      tags$p("Challenge the AI by"),
      tags$p("clicking the button below")
    ),
    footer = actionButton(
      inputId = ns("play"),
      label = "Play !",
      icon = icon("play"),
      style = "width: 100%"
    )
  )
}

welcome <- function(input, output, session){
  id <- gsub("-$", "", session$ns(""))
  showModal(ui = welcomeUI(id))
  
  observeEvent(input$play, {
    removeModal()
  })
  
  return(reactive(input$play))
}