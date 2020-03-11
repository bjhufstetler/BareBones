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
    for (cdf in distribution[colnames(distribution) == "cdf"]){
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
GetOverpowers <- function(cardEdgeValues, location, boardCards){
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
    matches <- GetMatches(cardEdgeValues, 
                          location, 
                          boardCards = boardCards)
    # get matched overpowers
    if(length(matches) >= 2){
      for(match in matches){
        matchEdgeValues <- c(boardCards$e1[match],
                             boardCards$e2[match],
                             boardCards$e3[match],
                             boardCards$e4[match],
                             boardCards$e5[match],
                             boardCards$e6[match])
        matchOverpowers <- GetOverpowers(matchEdgeValues, 
                                         match, 
                                         boardCards = boardCards)
        # add a chit to all matched cards, and cards overpowered by matched cards
        addChits <- c(addChits, match, matchOverpowers)
      }
    }
    overpoweredNeighbors <- GetOverpowers(cardEdgeValues, 
                                          location,
                                          boardCards = boardCards)
    if(length(overpoweredNeighbors) > 0){
      addChits <- c(addChits, overpoweredNeighbors)
    }
  }
  # add chits to all identified cards
  print(addChits)
  boardCards <- AddChits(player = player, 
                         addChits = addChits, 
                         evaluationType = evaluationType,
                         boardCards = boardCards)
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