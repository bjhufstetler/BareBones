# Title : BareBones
#    By : bjhufstetler
#  Date : 2020-03-06

# Uses: {base, shiny, glue}

# TODO: 
# 1) display captured chits, COMPLETE - maybe 
# 2) display rules, 
# 3) display arrows for each placed chit, 
# 4) pause while placing chits
# 4) let AI go first
# 5) display final message after game is over
# 6) update Readme.md

function(input, output, session){
  
  start <- callModule(module = welcome, id = "welcome")
  timer <- callModule(module = time, id = "timer", start = start)
  
  # Get enough card images to cover the board
  hex_png <- sample(list.files(path = "www/hex/", pattern = "png$"), boardSize + 5)
  
  # Deal cards for each player
  tempEdges <- dealCards()
  
  # Set the player's cards as a reactive object
  playerCards <- reactiveValues(e1 = tempEdges[,1],
                                e2 = tempEdges[,2],
                                e3 = tempEdges[,3],
                                e4 = tempEdges[,4],
                                e5 = tempEdges[,5],
                                e6 = tempEdges[,6],
                                im = paste0("www/hex/", hex_png))
  
  # Create a reactive object to hold the information on the board
  # [20:21] ==> Player 1's cards
  # [22:23] ==> Player 2's cards
  boardCards <- reactiveValues(e1 = c(rep(0, boardSize-2), tempEdges[1:4,1]), # Top edge
                               e2 = c(rep(0, boardSize-2), tempEdges[1:4,2]), # Top right edge
                               e3 = c(rep(0, boardSize-2), tempEdges[1:4,3]), # Bottom right edge
                               e4 = c(rep(0, boardSize-2), tempEdges[1:4,4]), # Bottom edge
                               e5 = c(rep(0, boardSize-2), tempEdges[1:4,5]), # Bottom left edge
                               e6 = c(rep(0, boardSize-2), tempEdges[1:4,6]), # Top left edge
                               c1 = rep(0, boardSize), # P1 chits on the board
                               c2 = rep(0, boardSize), # P2 chits on the board
                               oc = c(rep(FALSE, boardSize - 2), 1, 1), # Board position occupied
                               av = c(TRUE, rep(FALSE, boardSize - 1)), # Board position available
                               im = c("www/spaces/red.png",
                                      rep("www/spaces/beige2.png", 18),
                                      paste0("www/hex/", hex_png[1:4])), # Card image
                               se = c(rep(FALSE, boardSize)), # Hand card selected
                               sc = c(2, 0), # Scores
                               ch = c(0, 0), # Captured chits
                               p1 = c(1, 2), # Player cards taken by player 1
                               p2 = c(3, 4), # Player cards taken by player 2
                               tu = 1, # Turn determination
                               te = 0, # Test score
                               la = 1, # AIs location choice
                               ar = list(), # Arrow locations
                               ro = 0, # Round
                               ac = NULL, # Altered Cards
                               cp = 0) # Card Placed
  
  # Determine player 1
  # if(runif(1) > 0.5) boardCards$tu <- 2
  
  ####################################
  #--- Refresh Card Visualization ---#
  ####################################
  
  results_mods <- reactiveValues() # I don't know why I need this
  refreshCard <- function(x){
    results_mods[[paste0("module", x)]] <- callModule(
      module = hex,
      id = paste0("module", x),
      id2 = x,
      boardCards = boardCards,
      playerCards = playerCards
    )
  }
  
  lapply(
    X = seq_len(21),
    FUN = function(x){refreshCard(x)}
    )

  ####################################
  #--------- Display Scores ---------#
  ####################################
  
  output$score_ui <- renderText(
    paste(boardCards$sc[1], boardCards$sc[2], sep = " : ")
  )
  
  ####################################
  #---------- Reload Game -----------#
  ####################################
  
  observeEvent(input$reload, {
    session$reload()
  }, ignoreInit = TRUE)
  
  ####################################
  #---------- Rotate Cards ----------#
  ####################################
  observe({
    invalidateLater(100, session)
    if(isolate(boardCards$cp == 1)){
      isolate(boardCards$ar[[20]] <- 1)
      
      # Update card numbers and chits
      for(x in isolate(boardCards$ac)){
        refreshCard(x)
      }
      
      # Update arrows
      lapply(
        X = seq_len(19),
        FUN = function(x) {
          results_mods[[paste0("arrow", x)]] <- callModule(
            module = arrow,
            id = paste0("arrow", x),
            location = x,
            sides = isolate(boardCards$ar[[x]])
          )
        }
      )
      
      # Update P1 captured chits
      if(isolate(boardCards$ch[1] > 0)){
        for(i in 1:isolate(boardCards$ch[1])){
          results_mods[[paste0("cplayer",i)]] <- callModule(
            module = chit_player,
            id = paste0("cplayer", i)
          )
        }
      }
      
      # Update P2 captured chits
      if(isolate(boardCards$ch[2] > 0)){
        for(i in 1:isolate(boardCards$ch[2])){
          results_mods[[paste0("cai",i)]] <- callModule(
            module = chit_ai,
            id = paste0("cai", i)
          )
        }
      }
      isolate(boardCards$cp <- 0)
    }
  })
  
  observe({
    invalidateLater(1000, session)
    if(isolate(boardCards$tu == 2)){
      Sys.sleep(2)
      AITurn(isolate(boardCards), isolate(playerCards))
    }
  })
  
  observeEvent(input$pressedKey, {
    if(input$pressedKeyID == 37 | input$pressedKeyID == 39){
      for(card in c(20, 21)){
        if(input$pressedKeyID == 37){
        rCard <- c(boardCards$e2[card], boardCards$e3[card], boardCards$e4[card],
                   boardCards$e5[card], boardCards$e6[card], boardCards$e1[card])
        } else if(input$pressedKeyID == 39){
          rCard <- c(boardCards$e6[card], boardCards$e1[card], boardCards$e2[card],
                     boardCards$e3[card], boardCards$e4[card], boardCards$e5[card])
        } else {
          rCard <- c(boardCards$e1[card], boardCards$e2[card], boardCards$e3[card],
                     boardCards$e4[card], boardCards$e5[card], boardCards$e6[card])
        }
        boardCards$e1[card] <- rCard[1]
        boardCards$e2[card] <- rCard[2]
        boardCards$e3[card] <- rCard[3]
        boardCards$e4[card] <- rCard[4]
        boardCards$e5[card] <- rCard[5]
        boardCards$e6[card] <- rCard[6]
        refreshCard(card)
      }
    }
  })
}


