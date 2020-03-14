# utils
# Uses: {base}

dealCards <- function(){
  # Deal cards to each player
  tempEdges <- matrix(NA, ncol = 6)
  for (playerCard in seq_len(25)){
    # Create cardsPerPlayer*playerCount random cards
    # Determine the card level
    cardLevelRandomVariable <- runif(1)
    cardLevel = 1
    for(cdf in distribution[,9]){
      if (cardLevelRandomVariable > cdf){
        cardLevel = cardLevel + 1
      }
    }
    cardBounds <- distribution[cardLevel, colnames(distribution) %in% c("lb1","ub1","lb2","ub2","lb3","ub3")]
    edgeNums1 <- sample(as.integer(cardBounds[names(cardBounds) == "lb1"]):
                          as.integer(cardBounds[names(cardBounds) == "ub1"]), 
                        replace = TRUE, size = 2)
    edgeNums2 <- sample(as.integer(cardBounds[names(cardBounds) == "lb2"]):
                          as.integer(cardBounds[names(cardBounds) == "ub2"]), 
                        replace = TRUE, size = 2)
    edgeNums3 <- sample(as.integer(cardBounds[names(cardBounds) == "lb3"]):
                          as.integer(cardBounds[names(cardBounds) == "ub3"]), 
                        replace = TRUE, size = 2)
    edgeNums <- sample(c(edgeNums1, edgeNums2, edgeNums3))
    tempEdges <- rbind(tempEdges, edgeNums)
  }
  tempEdges <- tempEdges[-1,]
  return(tempEdges)
}

# FIND MATCHED CARDS
GetMatches <- function(cardEdgeValues, location, boardCards){ # pass the 6 edge values and the location
  edgePlacedCard <- 0
  edgeNeighbors <- c(4,5,6,1,2,3)
  edgeMatches <- vector()
  edgeNumbers <- vector()
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
        edgeNumbers <- c(edgeNumbers, edgePlacedCard)
      }
    }
  }
  return(list(edgeMatches = edgeMatches, 
              edgeNumbers = edgeNumbers)
         ) # edgeMatches is the location of a matched card
}

# FIND OVERPOWERED CARDS
GetOverpowers <- function(cardEdgeValues, location, boardCards){
  edgePlacedCard <- 0
  edgeNeighbors <- c(4,5,6,1,2,3)
  edgeOverpowers <- vector()
  edgeNumbers <- vector()
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
        edgeNumbers <- c(edgeNumbers, edgePlacedCard)
      }
    }
  }
  return(list(edgeOverpowers = edgeOverpowers, 
              edgeNumbers = edgeNumbers)
         ) # edgeOverpowers is the location of an overpowered card
}

# PLACE CHITS
AddChits <- function(player, addChits, evaluationType, boardCards){
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
PlaceCard <- function(player, 
                      round, 
                      cardEdgeValues, 
                      im, 
                      location, 
                      evaluationType = c("test", "real"),
                      boardCards,
                      se){
  # for card being placed: update board.cards edge values, chit, occupied, and available
  
  if(evaluationType == "real"){
    # Next Round
    boardCards$ro <- boardCards$ro + 1
    
    # set edge values, image, occupied = 1, available = 0
    boardCards$e1[location] <- cardEdgeValues[1]
    boardCards$e2[location] <- cardEdgeValues[2]
    boardCards$e3[location] <- cardEdgeValues[3]
    boardCards$e4[location] <- cardEdgeValues[4]
    boardCards$e5[location] <- cardEdgeValues[5]
    boardCards$e6[location] <- cardEdgeValues[6]
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
  arrows <- list("location" = location)
  if (round > peaceRounds){
    boardCards$ar <- list()
    # get matches
    matches <- GetMatches(cardEdgeValues, 
                          location, 
                          boardCards = boardCards)
    boardCards$ar[[location]] <- rep(0,6)
    # get matched overpowers
    if(length(matches$edgeMatches) >= 2){
      boardCards$ar[[location]][matches$edgeNumbers] <- 1
      for(match in matches$edgeMatches){
        matchEdgeValues <- c(boardCards$e1[match],
                             boardCards$e2[match],
                             boardCards$e3[match],
                             boardCards$e4[match],
                             boardCards$e5[match],
                             boardCards$e6[match])
        matchOverpowers <- GetOverpowers(matchEdgeValues, 
                                         match, 
                                         boardCards = boardCards)
        
        boardCards$ar[[match]] <- rep(0,6)
        boardCards$ar[[match]][matchOverpowers$edgeNumbers] <- 1
        # add a chit to all matched cards, and cards overpowered by matched cards
        addChits <- c(addChits, match, matchOverpowers$edgeOverpowers)
        arrows[[match]] <- rep(0,6)
        arrows[[match]][matchOverpowers$edgeNumbers] <- 1
      }
    }
    overpoweredNeighbors <- GetOverpowers(cardEdgeValues, 
                                          location,
                                          boardCards = boardCards)
    boardCards$ar[[location]][overpoweredNeighbors$edgeNumbers] <- 1
    if(length(overpoweredNeighbors$edgeOverpowers) > 0){
      addChits <- c(addChits, overpoweredNeighbors$edgeOverpoweres)
    }
  }
  # add chits to all identified cards
  
  boardCards <- AddChits(player = player, 
                         addChits = addChits, 
                         evaluationType = evaluationType,
                         boardCards = boardCards)
  boardCards$cp <- TRUE
  boardCards$ac <- addChits
  
  return(boardCards)
}

# Evaluate options
EvaluateOptions <- function(player, round, hand, intelligence, boardCards){
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
                                 "test",
                                 boardCards = boardCards)$te
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

AITurn <- function(boardCards, playerCards){
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
  
  return(boardCards)
}


# Player Turn
PlayerTurn <- function(boardCards, playerCards, se, id2){
  
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
  
  return(boardCards)
}