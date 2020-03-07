# Hex Image Module
# Uses: {base, shiny}

hexUI <- function(id, id2){
  ns <- shiny::NS(id)
  shiny::tagList(
    # Selected Card Outline
    shiny::absolutePanel(
      bottom = relationships[id2, 6] - 2, left = relationships[id2, 5] - 3, width = 139, draggable = F,
      shiny::imageOutput(
        outputId = ns("hexSelected"),
        width = 145, height = 124, inline = TRUE
      )
    ),
    # Card Image
    shiny::absolutePanel(
      bottom = relationships[id2, 6], left = relationships[id2, 5], width = 139, draggable = F,
      shiny::imageOutput(
        outputId = ns("hex"),
        click = clickOpts(id = ns("hexClick"), clip = FALSE),
        width = 139, height = 120, inline = TRUE
      )
    ),
    # Top Number
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 90), left = (relationships[id2, 5] + 60),
      shiny::imageOutput(outputId = ns("e1"), inline = TRUE)
    ),
    # Top Right Number
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 70), left = (relationships[id2, 5] + 95), 
      shiny::imageOutput(outputId = ns("e2"), inline = TRUE)
    ),
    # Bottom Right Number
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 30), left = (relationships[id2, 5] + 95),
      shiny::imageOutput(outputId = ns("e3"), inline = TRUE)
    ),
    # Bottom Number
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 10), left = (relationships[id2, 5] + 60),
      shiny::imageOutput(outputId = ns("e4"), inline = TRUE)
    ),
    # Bottom Left Number
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 30), left = (relationships[id2, 5] + 25),
      shiny::imageOutput(outputId = ns("e5"), inline = TRUE)
    ),
    # Top Left Number
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 70), left = (relationships[id2, 5] + 25),
      shiny::imageOutput(outputId = ns("e6"), inline = TRUE)
    ),
    # Left Chit
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 50), left = (relationships[id2, 5] + 45),
      shiny::imageOutput(outputId = ns("c1"), inline = TRUE)
    ),
    # Right Chit
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 50), left = (relationships[id2, 5] + 70),
      shiny::imageOutput(outputId = ns("c2"), inline = TRUE)
    )
  )
}

hex <- function(input, 
                output, 
                session,
                id2, bc){
  clickStatus <- shiny::reactiveValues(show = FALSE)
  
  # Click Status
  shiny::observeEvent(input$hexClick, {
    if(id2 > 19 & sum(isolate(bc$se[20:21])) < 2){
      clickStatus$show <- !clickStatus$show
    }
  })
  
  # Outline of Selected Card
  output$hexSelected <- shiny::renderImage({
    if(clickStatus$show){
      base::list(
        src = "www/spaces/red.png",
        width = 144,
        height = 124,
        contentType = "image/png"
      )
    } else {
      base::list(
        src = "www/spaces/red.png",
        width = 0,
        height = 0,
        contentType = "image/png")
    }
  }, deleteFile = FALSE)
  
  # Card Face
  output$hex <- shiny::renderImage({
      base::list(
        src = isolate(bc$im[id2]),
        width = 139,
        height = 120,
        contentType = "image/png")
    }, deleteFile = FALSE)
  
  # Top number
  output$e1 <- shiny::renderImage({
    if(isolate(bc$e1[id2]) != 0){
      base::list(
        src = base::paste0("www/nums/nums_0", isolate(bc$e1[id2]), ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
    )} else {
      base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
    }
  }, deleteFile = FALSE)
  
  # Top Right Number
  output$e2 <- shiny::renderImage({
    if(isolate(bc$e2[id2]) != 0){
      base::list(
        src = base::paste0("www/nums/nums_0", isolate(bc$e2[id2]), ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Bottom Right Number
  output$e3 <- shiny::renderImage({
    if(isolate(bc$e3[id2]) != 0){
      base::list(
        src = base::paste0("www/nums/nums_0", isolate(bc$e3[id2]), ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Bottom Number
  output$e4 <- shiny::renderImage({
    if(isolate(bc$e4[id2]) != 0){
      base::list(
        src = base::paste0("www/nums/nums_0", isolate(bc$e4[id2]), ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Bottom Left Number
  output$e5 <- shiny::renderImage({
    if(isolate(bc$e5[id2]) != 0){
      base::list(
        src = base::paste0("www/nums/nums_0", isolate(bc$e5[id2]), ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Top Left Number
  output$e6 <- shiny::renderImage({
    if(isolate(bc$e6[id2]) != 0){
      base::list(
        src = base::paste0("www/nums/nums_0", isolate(bc$e6[id2]), ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Left Chit
  output$c1 <- shiny::renderImage({
    if(isolate(bc$c1[id2]) != 0){
      base::list(
        src = base::paste0("www/chits/chits_01.png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Right Chit
  output$c2 <- shiny::renderImage({
    if(isolate(bc$c2[id2]) != 0){
      base::list(
        src = base::paste0("www/chits/chits_02.png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  return(clickStatus)
}