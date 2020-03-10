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
    # GetMatches <- function(cardEdgeValues, location){
    #   edgePlacedCard <- 0
    #   edgeNeighbors <- c(4,5,6,1,2,3)
    #   edgeMatches <- vector()
    #   for(neighbor in relationships[relationships[,colnames(relationships) == "placecard"] == location,
    #                                 colnames(relationships) == "neighborcard"]){
    #     edgePlacedCard <- edgePlacedCard + 1
    #     edgeNeighborCard <- edgeNeighbors[edgePlacedCard]
    #     if(neighbor > 0 && neighbor <= 19 && boardCards$oc[neighbor] == 1){
    #       if(cardEdgeValues[edgePlacedCard] == as.integer(boardCards[neighbor, edgeNeighborCard])){
    #         edgeMatches <- c(edgeMatches, neighbor)
    #       }
    #     }
    #   }
    #   return(edgeMatches)
    # }
    # 
    # GetOverpowers <- function(cardEdgeValues, location){
    #   edgePlacedCard <- 0
    #   edgeNeighbors <- c(4,5,6,1,2,3)
    #   edgeOverpowers <- vector()
    #   for(neighbor in relationships[relationships[,colnames(relationships) == "placecard"] == location, 
    #                                 colnames(relationships) == "neighborcard"]){
    #     edgePlacedCard <- edgePlacedCard + 1
    #     edgeNeighborCard <- edgeNeighbors[edgePlacedCard]
    #     if(neighbor > 0 && neighbor <= 19 && boardCards$oc[neighbor] == TRUE){
    #       if(cardEdgeValues[edgePlacedCard] > as.integer(boardCards[neighbor, edgeNeighborCard])){
    #         edgeOverpowers <- c(edgeOverpowers, neighbor)
    #       } 
    #     }
    #   }
    #   return(edgeOverpowers)
    # }  
    # 
    # AddChits <- function(player, addChits, evaluationType){
    #   # add chit to card placed
    #   placedCardResults <- 1
    #   
    #   if(evaluationType == "test"){
    #     chitsTest <- chits
    #     for(chitLocation in addChits){
    #       # add chit to existing card, with 0 player chits
    #       if(chitsTest[chitLocation, player] == 0){
    #         chitsTest[chitLocation, player] == 1
    #         placedCardResults <- placedCardResults + 1
    #       }else{
    #         # add chit to existing card, with 1 player chit
    #         placedCardResults <- placedCardResults + sum(chitsTest[chitLocation,-player])
    #         chitsTest[chitLocation, -player] <- 0
    #       }
    #     }
    #   }else if(evaluationType == "real"){
    #     for(chitLocation in addChits){
    #       # add chit to existing card, with 0 player chits
    #       if(chits[chitLocation, player] == 0){
    #         chits[chitLocation, player] <<- 1
    #         placedCardResults <- placedCardResults + 1
    #       }else{
    #         # add chit to existing card, with 1 player chit
    #         placedCardResults <- placedCardResults + sum(chits[chitLocation,-player])
    #         capturedChits[player] <<- capturedChits[player] + sum(chits[chitLocation,-player])
    #         chits[chitLocation, -player] <<- 0
    #       }
    #     }
    #   }else{
    #     print("Unexpected evaluation type.")
    #   }
    #   return(placedCardResults)
    # }
    # 
    # PlaceCard <- function(player, round, cardEdgeValues, location, evaluationType = c("test", "real")){
    #   # for card being placed: update board.cards edge values, chit, occupied, and available
    #   if(evaluationType == "real"){
    #     boardCards[location, ] <<- c(cardEdgeValues, 1, 0)  
    #     for (newNeighbor in relationships[relationships[,colnames(relationships) == "placecard"] == location &
    #                                       relationships[,colnames(relationships) == "neighborcard"] > 0 &
    #                                       relationships[,colnames(relationships) == "neighborcard"] <= (boardSize),
    #                                       colnames(relationships) == "neighborcard"]){
    #       if (length(newNeighbor) > 0 &
    #           boardCards[newNeighbor,colnames(boardCards) == "occupied"] == 0) {
    #         boardCards[newNeighbor, colnames(boardCards) == "available"] <<- 1
    #       }
    #     }
    #   }
    #   addChits <- location
    #   
    #   if (round > peaceRounds){ 
    #     # get matches
    #     matches <- GetMatches(cardEdgeValues, location)
    #     # get matched overpowers
    #     if(length(matches) >= 2){
    #       for(match in matches){
    #         matchEdgeValues <- boardCards[match, 1:6]
    #         matchOverpowers <- GetOverpowers(matchEdgeValues, match)
    #         # add a chit to all matched cards, and cards overpowered by matched cards
    #         addChits <- c(addChits, match, matchOverpowers)
    #       }
    #     }
    #     overpoweredNeighbors <- GetOverpowers(cardEdgeValues, location)
    #     if(length(overpoweredNeighbors) > 0){
    #       addChits <- c(addChits, overpoweredNeighbors)
    #     }
    #   }
    #   # add chits to all identified cards
    #   placedCardResults <- AddChits(player = player, addChits = addChits, evaluationType = evaluationType)
    #   return(placedCardResults)
    # }            
    
    # SELECT CARD IN HAND
    if(id2 > 19){
    boardCards$se[20:21] <- FALSE
    boardCards$se[id2] = TRUE
    }
    
    # PLACE SELECTED CARD ON BOARD
    print(c(id2, boardCards$av[id2], sum(boardCards$se[20:21])))
    if(id2 < 20 & boardCards$av[id2] == 1 & sum(boardCards$se[20:21]) == 1){
      if(boardCards$se[20] == 1){ 
        se <- 20
      } else {
        se <- 21
      }
      # Update card on board
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
      
      # Update card in hand
      boardCards$e1[se] <- playerCards$e1[-c(boardCards$p1, boardCards$p2)][1]
      boardCards$e2[se] <- playerCards$e2[-c(boardCards$p1, boardCards$p2)][1]
      boardCards$e3[se] <- playerCards$e3[-c(boardCards$p1, boardCards$p2)][1]
      boardCards$e4[se] <- playerCards$e4[-c(boardCards$p1, boardCards$p2)][1]
      boardCards$e5[se] <- playerCards$e5[-c(boardCards$p1, boardCards$p2)][1]
      boardCards$e6[se] <- playerCards$e6[-c(boardCards$p1, boardCards$p2)][1]
      boardCards$im[se] <- playerCards$im[-c(boardCards$p1, boardCards$p2)][1]
      boardCards$p1 <- c(boardCards$p1, max(boardCards$p1, boardCards$p2) + 1)
      print(playerCards$im)
      for(edge in neighbors){
        if(edge < 20 & boardCards$oc[edge] == 0){
          boardCards$av[edge] <- 1 #Make all neighbors available
          boardCards$im[edge] <- "www/spaces/red.png"
        }
      }
      # TODO Add the rest of the card logic and the AI here
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
