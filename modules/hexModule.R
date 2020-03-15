# Hex Image Module
# Uses: {base, shiny}

hexUI <- function(id, id2){
  ns <- NS(id)
  tagList(
    # Selected Card Outline
    absolutePanel(
      top = relationships[id2, 6] - 2, left = relationships[id2, 5] - 3, 
      width = 139, draggable = F, fixed = T,
      imageOutput(
        outputId = ns("hexSelected"),
        width = 145, height = 124, inline = TRUE
      )
    ),
    # Card Image
    absolutePanel(
      top = relationships[id2, 6], left = relationships[id2, 5], width = 139, 
      draggable = F, fixed = T,
      imageOutput(
        outputId = ns("hex"),
        click = clickOpts(id = ns("hexClick"), clip = FALSE),
        width = 139, height = 120, inline = TRUE
      )
    ),
    # Top Number
    absolutePanel(
      top = (relationships[id2, 6] + 90), left = (relationships[id2, 5] + 60),
      fixed = T,
      imageOutput(outputId = ns("e1"), inline = TRUE)
    ),
    # Top Right Number
    absolutePanel(
      top = (relationships[id2, 6] + 70), left = (relationships[id2, 5] + 95), 
      fixed = T,
      imageOutput(outputId = ns("e2"), inline = TRUE)
    ),
    # Bottom Right Number
    absolutePanel(
      top = (relationships[id2, 6] + 30), left = (relationships[id2, 5] + 95),
      fixed = T,
      imageOutput(outputId = ns("e3"), inline = TRUE)
    ),
    # Bottom Number
    absolutePanel(
      top = (relationships[id2, 6] + 10), left = (relationships[id2, 5] + 60),
      fixed = T,
      imageOutput(outputId = ns("e4"), inline = TRUE)
    ),
    # Bottom Left Number
    absolutePanel(
      top = (relationships[id2, 6] + 30), left = (relationships[id2, 5] + 25),
      fixed = T,
      imageOutput(outputId = ns("e5"), inline = TRUE)
    ),
    # Top Left Number
    absolutePanel(
      top = (relationships[id2, 6] + 70), left = (relationships[id2, 5] + 25),
      fixed = T,
      imageOutput(outputId = ns("e6"), inline = TRUE)
    ),
    # Left Chit
    absolutePanel(
      top = (relationships[id2, 6] + 50), left = (relationships[id2, 5] + 45),
      fixed = T,
      imageOutput(outputId = ns("c1"), inline = TRUE)
    ),
    # Right Chit
    absolutePanel(
      top = (relationships[id2, 6] + 50), left = (relationships[id2, 5] + 70),
      fixed = T,
      imageOutput(outputId = ns("c2"), inline = TRUE)
    )
  )
}

hex <- function(input, 
                output, 
                session,
                id2, 
                boardCards, 
                playerCards
                ){
  
  ####################################
  #------- Card Click Logic ---------#
  ####################################
  
  observeEvent(input$hexClick, {
    
    # CARD CLICK FUNCTIONS
    # SELECT CARD IN HAND
    if(id2 > 19){
    boardCards$se[20:21] <- FALSE
    boardCards$se[id2] = TRUE
    }
    
    se <- 19
    # PLACE SELECTED CARD ON BOARD
    if(id2 < 20 & boardCards$av[id2] == 1 & 
       sum(boardCards$se[20:21]) == 1){
      if(boardCards$se[20] == 1){ 
        se <- 20
      } else {
        se <- 21
      }
      if(boardCards$tu == 1 & 
         se > 19 &  
         boardCards$im[se] != "www/barebones.png"){
        boardCards <- PlayerTurn(boardCards, playerCards, se, id2)
      }
      
    }
  })
  
  ####################################
  #-------- Visualize Cards ---------#
  ####################################
  
  # Outline of Selected Card
  output$hexSelected <- renderImage({
    if(boardCards$se[id2]){
      list(
        src = "www/spaces/red.png",
        width = 144,
        height = 124,
        contentType = "image/png"
      )
    } else {
      list(
        src = "www/spaces/red.png",
        width = 0,
        height = 0,
        contentType = "image/png")
    }
  }, deleteFile = FALSE)
  
  # Card Face
  output$hex <- renderImage({
    list(
      src = boardCards$im[id2],
      width = 139,
      height = 120,
      contentType = "image/png")
  }, deleteFile = FALSE)
  
  # Top number
  output$e1 <- renderImage({
    if(boardCards$e1[id2] != 0){
      list(
        src = paste0("www/nums/nums_0", boardCards$e1[id2], ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Top Right Number
  output$e2 <- renderImage({
    if(boardCards$e2[id2] != 0){
      list(
        src = paste0("www/nums/nums_0", boardCards$e2[id2], ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Bottom Right Number
  output$e3 <- renderImage({
    if(boardCards$e3[id2] != 0){
      list(
        src = paste0("www/nums/nums_0", boardCards$e3[id2], ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Bottom Number
  output$e4 <- renderImage({
    if(boardCards$e4[id2] != 0){
      list(
        src = paste0("www/nums/nums_0", boardCards$e4[id2], ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Bottom Left Number
  output$e5 <- renderImage({
    if(boardCards$e5[id2] != 0){
      list(
        src = paste0("www/nums/nums_0", boardCards$e5[id2], ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Top Left Number
  output$e6 <- renderImage({
    if(boardCards$e6[id2] != 0){
      list(
        src = paste0("www/nums/nums_0", boardCards$e6[id2], ".png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Left Chit
  output$c1 <- renderImage({
    if(boardCards$c1[id2] != 0){
      list(
        src = paste0("www/chits/chits_06.png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Right Chit
  output$c2 <- renderImage({
    if(boardCards$c2[id2] != 0){
      list(
        src = paste0("www/chits/chits_05.png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  return(boardCards)
}
