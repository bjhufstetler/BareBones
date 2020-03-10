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
                id2, boardCards
                ){
  #clickStatus <- shiny::reactiveValues(show = FALSE)
  # Click Status
  shiny::observeEvent(input$hexClick, {
    # select card from hand
    if(id2 > 19){
    boardCards$se[20:21] <- FALSE
    boardCards$se[id2] = TRUE
    }
    # place selected card on board
    print(c(id2, boardCards$av[id2], sum(boardCards$se[20:21])))
    if(id2 < 20 & boardCards$av[id2] == 1 & sum(boardCards$se[20:21]) == 1){
      if(boardCards$se[20] == 1){ 
        se <- 20
      } else {
        se <- 21
      }
      boardCards$e1[id2] <- boardCards$e1[se]
      boardCards$e2[id2] <- boardCards$e2[se]
      boardCards$e3[id2] <- boardCards$e3[se]
      boardCards$e4[id2] <- boardCards$e4[se]
      boardCards$e5[id2] <- boardCards$e5[se]
      boardCards$e6[id2] <- boardCards$e6[se]
      boardCards$im[id2] <- boardCards$im[se]
      boardCards$c1[id2] <- 1 # Add chit
      boardCards$se[se] <- 0 # Deselect card from hand
      boardCards$av[id2] <- 0 # Make placement area unavailable
      boardCards$oc[id2] <- 1 # Mark placement area as occupied
      neighbors <- relationships[relationships[,1] == id2,3]
      print(neighbors)
      for(edge in neighbors){
        if(edge < 20 & boardCards$oc[edge] == 0){
          boardCards$av[edge] <- 1 #Make all neighbors available
          boardCards$im[edge] <- "www/spaces/red.png"
        }
      }
    }
  })
  # Outline of Selected Card
  output$hexSelected <- shiny::renderImage({
    if(boardCards$se[id2]){
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
      src = boardCards$im[id2],
      width = 139,
      height = 120,
      contentType = "image/png")
  }, deleteFile = FALSE)
  
  # Top number
  output$e1 <- shiny::renderImage({
    if(boardCards$e1[id2] != 0){
      base::list(
        src = base::paste0("www/nums/nums_0", boardCards$e1[id2], ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Top Right Number
  output$e2 <- shiny::renderImage({
    if(boardCards$e2[id2] != 0){
      base::list(
        src = base::paste0("www/nums/nums_0", boardCards$e2[id2], ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Bottom Right Number
  output$e3 <- shiny::renderImage({
    if(boardCards$e3[id2] != 0){
      base::list(
        src = base::paste0("www/nums/nums_0", boardCards$e3[id2], ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Bottom Number
  output$e4 <- shiny::renderImage({
    if(boardCards$e4[id2] != 0){
      base::list(
        src = base::paste0("www/nums/nums_0", boardCards$e4[id2], ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Bottom Left Number
  output$e5 <- shiny::renderImage({
    if(boardCards$e5[id2] != 0){
      base::list(
        src = base::paste0("www/nums/nums_0", boardCards$e5[id2], ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Top Left Number
  output$e6 <- shiny::renderImage({
    if(boardCards$e6[id2] != 0){
      base::list(
        src = base::paste0("www/nums/nums_0", boardCards$e6[id2], ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Left Chit
  output$c1 <- shiny::renderImage({
    if(boardCards$c1[id2] != 0){
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
    if(boardCards$c2[id2] != 0){
      base::list(
        src = base::paste0("www/chits/chits_02.png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        base::list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  return(boardCards)
}
