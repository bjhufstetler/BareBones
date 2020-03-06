# Hex Image Module

hexUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::imageOutput(
      outputId = ns("hex"),
      click = clickOpts(id = ns("hex_click"), clip = FALSE),
      width = 139,
      height = 120,
      inline = TRUE
    )
  )
}

hex <- function(input, 
                output, 
                session, 
                hexID, 
                reset = shiny::reactiveValues(x = NULL),
                block = shiny::reactiveValues(x = NULL)){
  clickStatus <- shiny::reactiveValues(show = FALSE,
                                       hex = hexID,
                                       ts = base::Sys.time(),
                                       found = FALSE)
  shiny::observeEvent(input$hex_click,{
    if(!clickStatus$found){
      clickStatus$show <- !clickStatus$show
      clickStatus$ts <- base::Sys.time()
    }
  })
  
  shiny::observeEvent(block$x, {
    if(hexID %in% block$x) clickStatus$found <- TRUE
  })
  
  shiny::observeEvent(reset$x, {
    if(hexID %in% reset$x & !clickStatus$found) clickStatus$show <- FALSE
  })
  
  output$hex <- shiny::renderImage({
    if(!clickStatus$show){
      base::list(
        src = "www/barebones.png",
        width = 139,
        height = 120,
        contentType = "image/png"
      )
    } else {
      base::list(
        src = base::paste0("www/hex/", hexID),
        width = 139,
        height = 120,
        contentType = "image/png"
      )
    }
  }, deleteFile = FALSE)
  
  return(clickStatus)
}