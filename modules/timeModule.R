# Time Module
# Uses: {base, shiny}

timeUI <- function(id){
  ns <- NS(id)
  tags$div(
    style = "width:100%; text-align: center;",
    "Time elapsed:",
    uiOutput(outputId = ns("timer_ui"),
                    style = "font-size: 200%; font-weight: bold;",
                    inline = TRUE)
  )
}

time <- function(input,
                 output,
                 session,
                 start = reactive(0)){
  timeR <- reactiveVal(value = 0)
  started <- reactiveVal(value = FALSE)
  
  observeEvent(start(), {
    timeR(0)
    started(TRUE)
  }, ignoreInit = TRUE)
  
  observe({
    if(started()){
      invalidateLater(1000, session)
      isolate({
        newTime <- timeR() + 1
        timeR(newTime)
      })
    }
  })
  
  output$timer_ui <- renderUI({
    as.character(timeR())
  })
  
  return(timeR)
}