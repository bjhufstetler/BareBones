# Hex Image Module
# Uses: {base, shiny}

hexUI <- function(id, id2){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::absolutePanel(
      bottom = relationships[id2, 6], left = relationships[id2, 5], width = 139, draggable = F,
      shiny::imageOutput(
        outputId = ns("hex"),
        click = clickOpts(id = ns("hexClick"), clip = FALSE),
        width = 139,
        height = 120,
        inline = TRUE
      )
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
  shiny::observeEvent(input$hexClick,{
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
        src = "www/spaces/beige2.png",
        width = 139,
        height = 120,
        contentType = "image/png"
      )
    } else {
      base::list(
        src = base::paste0("www/spaces/red.png"),
        width = 139,
        height = 120,
        contentType = "image/png"
      )
    }
  }, deleteFile = FALSE)
  
  return(clickStatus)
}