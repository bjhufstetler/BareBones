# utils
# Uses: {base}

####################################
#----------- Deal Cards -----------#
####################################

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

####################################
#----------- Find Matches ---------#
####################################

GetMatches <- function(cardEdgeValues, location, boardCards){ # pass the 6 edge values and the location
  edgePlacedCard <- 0
  edgeNeighbors <- c(4,5,6,1,2,3)
  edgeMatches <- vector()
  edgeNumbers <- vector()
  # neighbor is the location of each adjacent card starting at the top and going clockwise
  for(neighbor in relationships[relationships[,1] == location, 3]){
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

####################################
#--------- Find Overpowers --------#
####################################

GetOverpowers <- function(cardEdgeValues, location, boardCards){
  edgePlacedCard <- 0
  edgeNeighbors <- c(4,5,6,1,2,3)
  edgeOverpowers <- vector()
  edgeNumbers <- vector()
  for(neighbor in relationships[relationships[,1] == location, 3]){
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

####################################
#----------- Place Chits ----------#
####################################

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

####################################
#----------- Place Card -----------#
####################################

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
      }
    }
    overpoweredNeighbors <- GetOverpowers(cardEdgeValues, 
                                          location,
                                          boardCards = boardCards)
    boardCards$ar[[location]][overpoweredNeighbors$edgeNumbers] <- 1
    if(length(overpoweredNeighbors$edgeOverpowers) > 0){
      addChits <- c(addChits, overpoweredNeighbors$edgeOverpowers)
    }
  }
  # add chits to all identified cards
  boardCards <- AddChits(player = player, 
                         addChits = addChits, 
                         evaluationType = evaluationType,
                         boardCards = boardCards)
  if(evaluationType == "real"){
    boardCards$cp <- TRUE
    boardCards$ms <- FALSE
    boardCards$ac <- addChits
  }
  
  return(boardCards)
}

####################################
#-------- Evaluate Options --------#
####################################

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

####################################
#------- AI Opponent Logic --------#
####################################

AITurn <- function(boardCards, playerCards){
  
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
    
    # Update AIs choice
    boardCards$la <- choice$location
  }
  
  # Update scores
  boardCards$sc[1] <- sum(boardCards$c1, boardCards$ch[1])
  boardCards$sc[2] <- sum(boardCards$c2, boardCards$ch[2])
  
  boardCards$tu <- 1
  return(boardCards)
}


# Player Turn
PlayerTurn <- function(boardCards, playerCards, se, id2){
  
  boardCards <- PlaceCard(player = 1,
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
  
  # Update scores
  boardCards$sc[1] <- sum(boardCards$c1, boardCards$ch[1])
  boardCards$sc[2] <- sum(boardCards$c2, boardCards$ch[2])
  
  boardCards$tu <- 2
  return(boardCards)
}


helpUI <- function(){
  showModal(modalDialog(
    title = tags$h1(
      style = "text-align: center;",
      "Bare Bones Rules"
    ),
    tags$div(
      style = "text-align: left;",
      tags$h4("Overview"),
      tags$p("
      Bare Bones is played by each player placing hexagonal cards, one at a
      time, onto a grid adjacent to previously played cards. The first card
      played is called the *anchor* and must be placed in the center of the
      grid. Each player is dealt six cards but only have access to two at a
      time. When a card is placed, its edges are evaluated against their
      neighbors to determine if any points are scored. Players can score
      points by matching edge numbers or placing higher edge numbers against
      existing cards. The player with the most points after all cards are
      played is the winner. The first player to lay down a card wins in the
      event of a tie.
      "),
      
      tags$h4("Choosing the first player"),
      tags$p("
      The first player is chosen randomly. The first
      player is given two bonus points and wins in the event of a tie. Again
      the first card must be placed in the center of the grid.
      "),
      
      tags$h4("Placing cards"),
      tags$p("
      Cards can be selected by clicking on one of the cards on the left side
      of the screen. This is your *hand*. You may rotate the cards by pressing
      the left or right arrows on your keyboard. Cards may be placed in any of
      the red hexagons on the grid, and in any orientation.
      "),
      
      tags$h4("Placing Chits"),
      tags$p("
      Chits are used to track each player’s score. You are placing white
      chits and trying to capture black chits. A chit is placed on a card when
      it is first laid down. Additional chits may be placed onto or removed
      from neighboring cards given the overpowering and matching rules
      described in the next section.
      
      Multiple players may have chits on the same card but they cannot have
      more than one chit on a single card. If you have the opportunity to
      place a second chit on a card containing an opponent’s chit, you capture
      the opponent’s chit, removing it from the board and adding it to your
      score.
      "),
      
      tags$h4("Overpowering"),
      tags$p("
      A card is overpowered when a card is placed along its edge containing
      a higher number on the shared edge. When a card is overpowered, the
      player who placed the overpowering card places a chit of their color
      onto the overpowered card.
      "),
      
      tags$h4("Matching"),
      tags$p("
      If at least two edges of a newly placed card match the numbers on
      previously played cards, then a chit may be placed on each of the
      matched cards, and the matched cards may attempt to overpower
      their neighbors as if the matched cards had just been played. All edges
      of a matched card may be evaluated, thus, if two matched cards are
      adjacent to one another, one may overpower the other.
      "),
      
      tags$h4("AI"),
      tags$p("
      The AI uses a greedy algorithm but occassionally makes sub-optimal
      decisions. Bare Bones is a game of chance and skill. It has been
      empirically shown to be evenly balanced when each player uses the same
      strategy and card distribution.
      "),
      
      tags$h4("Physical Game"),
      tags$p("
      Bare Bones will be going on sale in the Summer of 2020 as a physical
      trading card game. The same rules apply with the addition that when a
      game is played, the winner will take posession of one of their
      opponent’s cards (of the winner’s choosing). There are five large
      pieces of artwork in the initial release, each of which has been cut up
      into hexagons and randomly distributed into the packs being sold. The
      goal is to complete each work of art.
      "),
      
      tags$h4("Special Thanks and Recognition"),
      tags$p("
      The code used in this application was originally adapted from a contest
      submission last year by [dreamRs](https://github.com/dreamRs/memory-hex)
      (dreamRs 2019).
      
      All artwork created by Eldyn Park.
      ")
    ),
  easyClose = TRUE)
  )
  }
