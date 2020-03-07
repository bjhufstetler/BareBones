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
        width = 139, height = 120, inline = TRUE
      )
    ),
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 90), left = (relationships[id2, 5] + 60),
      shiny::imageOutput(outputId = ns("s1"), inline = TRUE)
    ),
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 70), left = (relationships[id2, 5] + 95), 
      shiny::imageOutput(outputId = ns("s2"), inline = TRUE)
    ),
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 30), left = (relationships[id2, 5] + 95),
      shiny::imageOutput(outputId = ns("s3"), inline = TRUE)
    ),
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 10), left = (relationships[id2, 5] + 60),
      shiny::imageOutput(outputId = ns("s4"), inline = TRUE)
    ),
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 30), left = (relationships[id2, 5] + 25),
      shiny::imageOutput(outputId = ns("s5"), inline = TRUE)
    ),
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 70), left = (relationships[id2, 5] + 25),
      shiny::imageOutput(outputId = ns("s6"), inline = TRUE)
    ),
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 50), left = (relationships[id2, 5] + 45),
      shiny::imageOutput(outputId = ns("c1"), inline = TRUE)
    ),
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 50), left = (relationships[id2, 5] + 70),
      shiny::imageOutput(outputId = ns("c2"), inline = TRUE)
    )
  )
}

hex <- function(input, 
                output, 
                session, 
                reset = shiny::reactiveValues(x = NULL),
                block = shiny::reactiveValues(x = NULL),
                faceIMG, s1, s2, s3, s4, s5, s6, c1, c2, id2){
  clickStatus <- shiny::reactiveValues(show = FALSE,
                                       ts = base::Sys.time(),
                                       found = FALSE)
  shiny::observeEvent(input$hexClick,{
    if(id2 > 19){
      clickStatus$show <- !clickStatus$show
      #clickStatus$ts <- base::Sys.time()
    }
  })
  
  output$hex <- shiny::renderImage({
    if(clickStatus$show){
      base::list(
        src = "www/spaces/beige1.png",
        width = 139,
        height = 120,
        contentType = "image/png"
      )
    } else {
      base::list(
        src = faceIMG,
        width = 139,
        height = 120,
        contentType = "image/png")
    }
    }, deleteFile = FALSE)
  
  output$s1 <- shiny::renderImage({
    if(s1 != 0){
      base::list(
        src = base::paste0("www/nums/nums_0", s1, ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
    )} else {
      base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
    }
  }, deleteFile = FALSE)
  
  output$s2 <- shiny::renderImage({
    if(s2 != 0){
      base::list(
        src = base::paste0("www/nums/nums_0", s2, ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  output$s3 <- shiny::renderImage({
    if(s3 != 0){
      base::list(
        src = base::paste0("www/nums/nums_0", s3, ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  output$s4 <- shiny::renderImage({
    if(s4 != 0){
      base::list(
        src = base::paste0("www/nums/nums_0", s4, ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  output$s5 <- shiny::renderImage({
    if(s5 != 0){
      base::list(
        src = base::paste0("www/nums/nums_0", s5, ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  output$s6 <- shiny::renderImage({
    if(s6 != 0){
      base::list(
        src = base::paste0("www/nums/nums_0", s6, ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  output$c1 <- shiny::renderImage({
    if(c1 != 0){
      base::list(
        src = base::paste0("www/chits/chits_01.png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  output$c2 <- shiny::renderImage({
    if(c2 != 0){
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