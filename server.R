# Title : BareBones
#    By : bjhufstetler
#  Date : 2020-03-06

# Uses: {base, shiny, glue}

function(input, output, session){
  
  start <- callModule(module = welcome, id = "welcome")
  timer <- callModule(module = time, id = "timer", start = start)
  
  # Get enough card images to cover the board
  hex_png <- sample(list.files(path = "www/hex/", pattern = "png$"), boardSize)
  
  # Deal cards for each player
  tempEdges <- dealCards()
  
  # Set the player's cards as a reactive object
  playerCards <- reactiveValues(edge1 = tempEdges[,1],
                                edge2 = tempEdges[,2],
                                edge3 = tempEdges[,3],
                                edge4 = tempEdges[,4],
                                edge5 = tempEdges[,5],
                                edge6 = tempEdges[,6],
                                faceIMG = paste0("www/hex/", hex_png[1:playerCount*cardsPerPlayer]))
  
  # Create a reactive object to hold the information displayed on the cards
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
                               sc = c(2, 3), # Scores
                               ch = c(0, 0), # Captured chits
                               p1 = c(1, 2), # Player cards taken by player 1
                               p2 = c(3, 4)) # Playre cards taken by player 2
  
  
  
  
  ####################################
  #--- Refresh Card Visualization ---#
  ####################################
  
  results_mods <- reactiveValues() # I don't know why I need this
  refreshCard <- function(x){
    results_mods[[paste0("module", x)]] <- callModule(
      module = hex,
      id = paste0("module", x),
      id2 = x, boardCards = boardCards
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
  

