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
    # FIND MATCHED CARDS
    GetMatches <- function(cardEdgeValues, location){ # pass the 6 edge values and the location
      edgePlacedCard <- 0
      edgeNeighbors <- c(4,5,6,1,2,3)
      edgeMatches <- vector()
      # neighbor is the location of each adjacent card starting at the top and going clockwise
      for(neighbor in relationships[relationships[,colnames(relationships) == "placecard"] == location,
                                    colnames(relationships) == "neighborcard"]){
        edgePlacedCard <- edgePlacedCard + 1 # The edge being considered
        edgeNeighborCard <- edgeNeighbors[edgePlacedCard] # The edge of the neighbor being considered
        # Check if neighbor is on the board and occupied
        if(neighbor > 0 && neighbor <= 19 && boardCards$oc[neighbor] == 1){
          edgeNeighborValue <- switch(edgeNeighborCard,
                                      boardCards$e1[neighbor],
                                      boardCards$e2[neighbor],
                                      boardCards$e3[neighbor],
                                      boardCards$e4[neighbor],
                                      boardCards$e5[neighbor],
                                      boardCards$e6[neighbor])
          if(cardEdgeValues[edgePlacedCard] == as.integer(edgeNeighborValue)){
            edgeMatches <- c(edgeMatches, neighbor)
          }
        }
      }
      return(edgeMatches) # edgeMatches is the location of a matched card
    }

    # FIND OVERPOWERED CARDS
    GetOverpowers <- function(cardEdgeValues, location){
      edgePlacedCard <- 0
      edgeNeighbors <- c(4,5,6,1,2,3)
      edgeOverpowers <- vector()
      for(neighbor in relationships[relationships[,colnames(relationships) == "placecard"] == location,
                                    colnames(relationships) == "neighborcard"]){
        edgePlacedCard <- edgePlacedCard + 1
        edgeNeighborCard <- edgeNeighbors[edgePlacedCard]
        if(neighbor > 0 && neighbor <= 19 && boardCards$oc[neighbor] == 1){
          edgeNeighborValue <- switch(edgeNeighborCard,
                                      boardCards$e1[neighbor],
                                      boardCards$e2[neighbor],
                                      boardCards$e3[neighbor],
                                      boardCards$e4[neighbor],
                                      boardCards$e5[neighbor],
                                      boardCards$e6[neighbor])
          if(cardEdgeValues[edgePlacedCard] > as.integer(edgeNeighborValue)){
            edgeOverpowers <- c(edgeOverpowers, neighbor)
          }
        }
      }
      return(edgeOverpowers) # edgeOverpowers is the location of an overpowered card
    }

    # PLACE CHITS
    AddChits <- function(player, addChits, evaluationType){
      # addChits is a vector of locations to add a chit
      # add chit to card placed
      placedCardResults <- 1

      if(evaluationType == "test"){
        chitsTest <- matrix(c(unlist(boardCards$c1),
                              unlist(boardCards$c2)),
                            ncol = 2)
        for(chitLocation in addChits){
          # add chit to existing card, with 0 player chits
          if(chitsTest[chitLocation, player] == 0){
            chitsTest[chitLocation, player] <- 1
            placedCardResults <- placedCardResults + 1
          }else{
            # add chit to existing card, with 1 player chit
            placedCardResults <- placedCardResults + sum(chitsTest[chitLocation,-player])
            chitsTest[chitLocation, -player] <- 0
          }
        }
      } else if(evaluationType == "real"){
        chits <- matrix(c(unlist(boardCards$c1),
                          unlist(boardCards$c2)),
                        ncol = 2)
        capturedChits <- unlist(boardCards$ch)
        for(chitLocation in addChits){
          # add chit to existing card, with 0 player chits
          if(chits[chitLocation, player] == 0){
            chits[chitLocation, player] <- 1
            placedCardResults <- placedCardResults + 1
          }else{
            # add chit to existing card, with 1 player chit
            capturedChits[player] <- capturedChits[player] + sum(chits[chitLocation,-player])
            chits[chitLocation, -player] <- 0
            placedCardResults <- placedCardResults + sum(chits[chitLocation,-player])
          }
        }
        boardCards$c1 <- chits[,1]
        boardCards$c2 <- chits[,2]
        boardCards$ch <- capturedChits
      } else {
        print("Unexpected evaluation type.")
      }
      boardCards$te <- placedCardResults # Used to evaluate options
      return(boardCards)
    }

    # Place Card
    PlaceCard <- function(player, round, cardEdgeValues, im, location, evaluationType = c("test", "real")){
      # for card being placed: update board.cards edge values, chit, occupied, and available
      print(im)
      if(evaluationType == "real"){
        # set edge values, image, occupied = 1, available = 0
        boardCards$e1[location] <- boardCards$e1[se]
        boardCards$e2[location] <- boardCards$e2[se]
        boardCards$e3[location] <- boardCards$e3[se]
        boardCards$e4[location] <- boardCards$e4[se]
        boardCards$e5[location] <- boardCards$e5[se]
        boardCards$e6[location] <- boardCards$e6[se]
        boardCards$im[location] <- im
        boardCards$av[location] <- 0 # Make placement area unavailable
        boardCards$oc[location] <- 1 # Mark placement area as occupied
        
        # New neighbors
        neighbors <- relationships[relationships[,1] == location,3]
        for(edge in neighbors){
          if(edge < 20 & boardCards$oc[edge] == 0){
            boardCards$av[edge] <- 1 #Make all neighbors available
            boardCards$im[edge] <- "www/spaces/red.png"
          }
        }
      }
      addChits <- location

      if (round > peaceRounds){
        # get matches
        matches <- GetMatches(cardEdgeValues, location)
        # get matched overpowers
        if(length(matches) >= 2){
          for(match in matches){
            matchEdgeValues <- c(boardCards$e1[match],
                                 boardCards$e2[match],
                                 boardCards$e3[match],
                                 boardCards$e4[match],
                                 boardCards$e5[match],
                                 boardCards$e6[match])
            matchOverpowers <- GetOverpowers(matchEdgeValues, match)
            # add a chit to all matched cards, and cards overpowered by matched cards
            addChits <- c(addChits, match, matchOverpowers)
          }
        }
        overpoweredNeighbors <- GetOverpowers(cardEdgeValues, location)
        if(length(overpoweredNeighbors) > 0){
          addChits <- c(addChits, overpoweredNeighbors)
        }
      }
      # add chits to all identified cards
      print(addChits)
      placedCardResults <- AddChits(player = player, 
                                    addChits = addChits, 
                                    evaluationType = evaluationType)
      return(boardCards)
    }
    
    # Evaluate options
    EvaluateOptions <- function(player, round, hand, intelligence){
      index = 0
      bestScore <- 0
      bestSoln <- list()
      for(card in 1:dim(hand)[1]){ # check each card
        if(!is.na(hand[card,1])){
          index = index + 1
          for(location in seq_len(boardSize)[boardCards$av == 1]){ # check each viable location
            for(rotation in 1:6){ # check each orientation
              cardRotated <- rep(hand[card,], 2)[rotation:(rotation+5)]
              tempScore <- PlaceCard(player,
                                     round = round,
                                     cardRotated,
                                     im = "null",
                                     location,
                                     "test")$te
              if((tempScore > bestScore & runif(1) < intelligence) | bestScore == 0){
                bestScore = tempScore
                bestSoln <- list(index = index,
                                 rotation = rotation,
                                 location = location)
              } else if(tempScore == bestScore){
                if(runif(1) > 0.9){
                  bestSoln <- list(index = index,
                                   rotation = rotation,
                                   location = location)
                }
              }
            }
          }
        }
      }
      return(bestSoln)
    }
    
    # SELECT CARD IN HAND
    if(id2 > 19){
    boardCards$se[20:21] <- FALSE
    boardCards$se[id2] = TRUE
    }
    
    # PLACE SELECTED CARD ON BOARD
    if(id2 < 20 & boardCards$av[id2] == 1 & sum(boardCards$se[20:21]) == 1){
      if(boardCards$se[20] == 1){ 
        se <- 20
      } else {
        se <- 21
      }
      if(boardCards$tu == 1){
        boardCards <- PlaceCard(player = boardCards$tu,
                                round = 3,
                                cardEdgeValues = c(boardCards$e1[se],
                                                   boardCards$e2[se],
                                                   boardCards$e3[se],
                                                   boardCards$e4[se],
                                                   boardCards$e5[se],
                                                   boardCards$e6[se]),
                                im = boardCards$im[se],
                                location = id2,
                                evaluationType = "real")
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
        
        # Change player's turn
        boardCards$tu <- 2
      }
      
      # AI's move
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
                                  round = 3, 
                                  hand = p2Hand, 
                                  intelligence = intelligence)
        # AI chosen card rotated
        rotatedCard <- rep(p2Hand[choice$index, ], 2)[choice$rotation:(choice$rotation + 5)]
        
        # Place AI chosen rotated card
        boardCards <- PlaceCard(player = 2,
                                round = 3,
                                cardEdgeValues = rotatedCard,
                                im = boardCards$im[21 + choice$index],
                                location = choice$location,
                                evaluationType = "real")
        
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
      boardCards$sc[1] <- sum(boardCards$c1, boardCards$ch[1])
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
        src = paste0("www/chits/chits_01.png"),
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
        src = paste0("www/chits/chits_02.png"),
        width = 20,
        height = 20,
        contentType = "image/png"
      )} else {
        list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  return(boardCards)
}
