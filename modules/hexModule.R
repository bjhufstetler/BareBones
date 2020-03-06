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
    ),
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 90), left = (relationships[id2, 5] + 60),
      shiny::imageOutput(
        outputId = ns("s1"),
        click = clickOpts(id = ns("hexClick"), clip = FALSE),
        inline = TRUE
      )
    ),
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 70), left = (relationships[id2, 5] + 95), 
      shiny::imageOutput(
        outputId = ns("s2"),
        click = clickOpts(id = ns("hexClick"), clip = FALSE),
        inline = TRUE
      )
    ),
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 30), left = (relationships[id2, 5] + 95),
      shiny::imageOutput(
        outputId = ns("s3"),
        click = clickOpts(id = ns("hexClick"), clip = FALSE),
        inline = TRUE
      )
    ),
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 10), left = (relationships[id2, 5] + 60),
      shiny::imageOutput(
        outputId = ns("s4"),
        click = clickOpts(id = ns("hexClick"), clip = FALSE),
        inline = TRUE
      )
    ),
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 30), left = (relationships[id2, 5] + 25),
      shiny::imageOutput(
        outputId = ns("s5"),
        click = clickOpts(id = ns("hexClick"), clip = FALSE),
        inline = TRUE
      )
    ),
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 70), left = (relationships[id2, 5] + 25),
      shiny::imageOutput(
        outputId = ns("s6"),
        click = clickOpts(id = ns("hexClick"), clip = FALSE),
        inline = TRUE
      )
    ),
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 50), left = (relationships[id2, 5] + 45),
      shiny::imageOutput(
        outputId = ns("c1"),
        click = clickOpts(id = ns("hexClick"), clip = FALSE),
        inline = TRUE
      )
    ),
    shiny::absolutePanel(
      bottom = (relationships[id2, 6] + 50), left = (relationships[id2, 5] + 70),
      shiny::imageOutput(
        outputId = ns("c2"),
        click = clickOpts(id = ns("hexClick"), clip = FALSE),
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
        src = base::paste0("www/hex/", hexID),
        width = 139,
        height = 120,
        contentType = "image/png"
      )
    }
  }, deleteFile = FALSE)
  
  output$s1 <- shiny::renderImage({
    if(!clickStatus$show){
      base::list(
        src = base::paste0("www/nums/nums_02.png"),
        width = 0,
        height = 0,
        contentType = "image/png"
      )
    } else {
      base::list(
        src = base::paste0("www/nums/nums_01.png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )
    }
  }, deleteFile = FALSE)
  
  output$s2 <- shiny::renderImage({
    if(!clickStatus$show){
      base::list(
        src = base::paste0("www/nums/nums_02.png"),
        width = 0,
        height = 0,
        contentType = "image/png"
      )
    } else {
      base::list(
        src = base::paste0("www/nums/nums_02.png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )
    }
  }, deleteFile = FALSE)
  
  output$s3 <- shiny::renderImage({
    if(!clickStatus$show){
      base::list(
        src = base::paste0("www/nums/nums_03.png"),
        width = 0,
        height = 0,
        contentType = "image/png"
      )
    } else {
      base::list(
        src = base::paste0("www/nums/nums_03.png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )
    }
  }, deleteFile = FALSE)
  
  output$s4 <- shiny::renderImage({
    if(!clickStatus$show){
      base::list(
        src = base::paste0("www/nums/nums_02.png"),
        width = 0,
        height = 0,
        contentType = "image/png"
      )
    } else {
      base::list(
        src = base::paste0("www/nums/nums_04.png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )
    }
  }, deleteFile = FALSE)
  
  output$s5 <- shiny::renderImage({
    if(!clickStatus$show){
      base::list(
        src = base::paste0("www/nums/nums_02.png"),
        width = 0,
        height = 0,
        contentType = "image/png"
      )
    } else {
      base::list(
        src = base::paste0("www/nums/nums_05.png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )
    }
  }, deleteFile = FALSE)
  
  output$s6 <- shiny::renderImage({
    if(!clickStatus$show){
      base::list(
        src = base::paste0("www/nums/nums_02.png"),
        width = 0,
        height = 0,
        contentType = "image/png"
      )
    } else {
      base::list(
        src = base::paste0("www/nums/nums_06.png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )
    }
  }, deleteFile = FALSE)
  
  output$c1 <- shiny::renderImage({
    if(!clickStatus$show){
      base::list(
        src = base::paste0("www/nums/nums_02.png"),
        width = 0,
        height = 0,
        contentType = "image/png"
      )
    } else {
      base::list(
        src = base::paste0("www/chits/chits_01.png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )
    }
  }, deleteFile = FALSE)
  
  output$c2 <- shiny::renderImage({
    if(!clickStatus$show){
      base::list(
        src = base::paste0("www/nums/nums_02.png"),
        width = 0,
        height = 0,
        contentType = "image/png"
      )
    } else {
      base::list(
        src = base::paste0("www/chits/chits_02.png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )
    }
  }, deleteFile = FALSE)
  
  return(clickStatus)
}