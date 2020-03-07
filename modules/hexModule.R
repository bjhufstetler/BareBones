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
                id2, bc){
  clickStatus <- shiny::reactiveValues(show = FALSE,
                                       ts = base::Sys.time(),
                                       found = FALSE)
  # bc <- base::lapply(bc, function(x) bc[[x]])
  # base::unlist(bc, use.names = FALSE)
  print(isolate(bc$e1[id2]))
  
  shiny::observeEvent(input$hexClick,{
    if(id2 > 19){
      clickStatus$show <- !clickStatus$show
    }
  })
  
  output$hex <- shiny::renderImage({
    if(clickStatus$show){
      base::list(
        src = "www/spaces/beige3.png",
        width = 139,
        height = 120,
        contentType = "image/png"
      )
    } else {
      base::list(
        src = isolate(bc$im[id2]),
        width = 139,
        height = 120,
        contentType = "image/png")
    }
    }, deleteFile = FALSE)
  
  output$s1 <- shiny::renderImage({
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
  
  output$s2 <- shiny::renderImage({
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
  
  output$s3 <- shiny::renderImage({
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
  
  output$s4 <- shiny::renderImage({
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
  
  output$s5 <- shiny::renderImage({
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
  
  output$s6 <- shiny::renderImage({
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