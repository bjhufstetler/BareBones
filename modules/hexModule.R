# Hex Image Module
# Uses: {base, shiny}

hexUI <- function(id, id2){
  ns <- NS(id)
  tagList(
    # Selected Card Outline
    absolutePanel(
      bottom = relationships[id2, 6] - 2, left = relationships[id2, 5] - 3, width = 139, draggable = F,
      imageOutput(
        outputId = ns("hexSelected"),
        width = 145, height = 124, inline = TRUE
      )
    ),
    # Card Image
    absolutePanel(
      bottom = relationships[id2, 6], left = relationships[id2, 5], width = 139, draggable = F,
      imageOutput(
        outputId = ns("hex"),
        click = clickOpts(id = ns("hexClick"), clip = FALSE),
        width = 139, height = 120, inline = TRUE
      )
    ),
    # Top Number
    absolutePanel(
      bottom = (relationships[id2, 6] + 90), left = (relationships[id2, 5] + 60),
      imageOutput(outputId = ns("e1"), inline = TRUE)
    ),
    # Top Right Number
    absolutePanel(
      bottom = (relationships[id2, 6] + 70), left = (relationships[id2, 5] + 95), 
      imageOutput(outputId = ns("e2"), inline = TRUE)
    ),
    # Bottom Right Number
    absolutePanel(
      bottom = (relationships[id2, 6] + 30), left = (relationships[id2, 5] + 95),
      imageOutput(outputId = ns("e3"), inline = TRUE)
    ),
    # Bottom Number
    absolutePanel(
      bottom = (relationships[id2, 6] + 10), left = (relationships[id2, 5] + 60),
      imageOutput(outputId = ns("e4"), inline = TRUE)
    ),
    # Bottom Left Number
    absolutePanel(
      bottom = (relationships[id2, 6] + 30), left = (relationships[id2, 5] + 25),
      imageOutput(outputId = ns("e5"), inline = TRUE)
    ),
    # Top Left Number
    absolutePanel(
      bottom = (relationships[id2, 6] + 70), left = (relationships[id2, 5] + 25),
      imageOutput(outputId = ns("e6"), inline = TRUE)
    ),
    # Left Chit
    absolutePanel(
      bottom = (relationships[id2, 6] + 50), left = (relationships[id2, 5] + 45),
      imageOutput(outputId = ns("c1"), inline = TRUE)
    ),
    # Right Chit
    absolutePanel(
      bottom = (relationships[id2, 6] + 50), left = (relationships[id2, 5] + 70),
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
        boardCards <- PlaceCard(player = boardCards$tu,
                                round = boardCards$ro,
                                cardEdgeValues = c(boardCards$e1[se],
                                                   boardCards$e2[se],
                                                   boardCards$e3[se],
                                                   boardCards$e4[se],
                                                   boardCards$e5[se],
                                                   boardCards$e6[se]),
                                im = boardCards$im[se],
                                location = id2,
                                evaluationType = "real",
                                boardCards = boardCards,
                                se = se)
        # Deselect card from hand
        boardCards$se[se] <- 0 
        
        # Update card in player's hand
        boardCards$e1[se] <- playerCards$e1[-c(boardCards$p1, boardCards$p2)][1]
        boardCards$e2[se] <- playerCards$e2[-c(boardCards$p1, boardCards$p2)][1]
        boardCards$e3[se] <- playerCards$e3[-c(boardCards$p1, boardCards$p2)][1]
        boardCards$e4[se] <- playerCards$e4[-c(boardCards$p1, boardCards$p2)][1]
        boardCards$e5[se] <- playerCards$e5[-c(boardCards$p1, boardCards$p2)][1]
        boardCards$e6[se] <- playerCards$e6[-c(boardCards$p1, boardCards$p2)][1]
        boardCards$im[se] <- playerCards$im[-c(boardCards$p1, boardCards$p2)][1]
        boardCards$p1 <- c(boardCards$p1, max(boardCards$p1, boardCards$p2) + 1)
        
        # If out of cards, show logo
        if(boardCards$ro > 8){
          boardCards$e1[se] <- 0
          boardCards$e2[se] <- 0
          boardCards$e3[se] <- 0
          boardCards$e4[se] <- 0
          boardCards$e5[se] <- 0
          boardCards$e6[se] <- 0
          boardCards$im[se] <- "www/barebones.png"
        }
        
        # Change player's turn
        boardCards$tu <- 2
      }
      
      
      ####################################
      #------- AI Opponent Logic --------#
      ####################################
      
      if(boardCards$tu == 2){
        p2Hand <- matrix(c(boardCards$e1[22:23],
                           boardCards$e2[22:23],
                           boardCards$e3[22:23],
                           boardCards$e4[22:23],
                           boardCards$e5[22:23],
                           boardCards$e6[22:23]),
                         byrow = F, ncol = 6)
        # AI chooses card, rotation, and location
        choice <- EvaluateOptions(player = 2, 
                                  round = boardCards$ro, 
                                  hand = p2Hand, 
                                  intelligence = intelligence,
                                  boardCards = boardCards)
        # AI chosen card rotated
        rotatedCard <- rep(p2Hand[choice$index, ], 2)[choice$rotation:(choice$rotation + 5)]
        
        # Place AI chosen rotated card
        boardCards <- PlaceCard(player = 2,
                                round = boardCards$ro,
                                cardEdgeValues = rotatedCard,
                                im = boardCards$im[21 + choice$index],
                                location = choice$location,
                                evaluationType = "real",
                                boardCards = boardCards,
                                se = 21 + choice$index)
        
        # Update card in AI's hand
        boardCards$e1[21 + choice$index] <- playerCards$e1[-c(boardCards$p1, boardCards$p2)][1]
        boardCards$e2[21 + choice$index] <- playerCards$e2[-c(boardCards$p1, boardCards$p2)][1]
        boardCards$e3[21 + choice$index] <- playerCards$e3[-c(boardCards$p1, boardCards$p2)][1]
        boardCards$e4[21 + choice$index] <- playerCards$e4[-c(boardCards$p1, boardCards$p2)][1]
        boardCards$e5[21 + choice$index] <- playerCards$e5[-c(boardCards$p1, boardCards$p2)][1]
        boardCards$e6[21 + choice$index] <- playerCards$e6[-c(boardCards$p1, boardCards$p2)][1]
        boardCards$im[21 + choice$index] <- playerCards$im[-c(boardCards$p1, boardCards$p2)][1]
        boardCards$p2 <- c(boardCards$p2, max(boardCards$p1, boardCards$p2) + 1)
        
        # Change player's turn
        boardCards$tu <- 1
        
        # Update AIs choice
        boardCards$la <- choice$location
      }
      
      # Update scores
      boardCards$sc[1] <- sum(boardCards$c1, boardCards$ch[1]) + 2
      boardCards$sc[2] <- sum(boardCards$c2, boardCards$ch[2])
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
        src = paste0("www/chits/chits_05.png"),
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
        src = paste0("www/chits/chits_06.png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  return(boardCards)
}
