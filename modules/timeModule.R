# Time Module

timeUI <- function(id){
  ns <- shiny::NS(id)
  tags$div(
    style = "width:100%; text-align: center;",
    "Time elapsed:",
    shiny::uiOutput(outputId = ns("timer_ui"),
                    style = "font-size: 200%; font-weight: bold;",
                    inline = TRUE)
  )
}

time <- function(input,
                 output,
                 session,
                 start = shiny::reactive(0)){
  timeR <- shiny::reactiveVal(value = 0)
  started <- shiny::reactiveVal(value = FALSE)
  
  shiny::observeEvent(start(), {
    timeR(0)
    started(TRUE)
  }, ignoreInit = TRUE)
  
  shiny::observe({
    if(started()){
      invalidateLater(1000, session)
      isolate({
        newTime <- TimeR() + 1
        timeR(newTime)
      })
    }
  })
  
  output$timer_ui <- shiny::renderUI({
    base::as.character(timeR())
  })
  
  return(timeR)
}