# Title : BareBones
#    By : bjhufstetler
#  Date : 2020-03-06

# Uses: {base, shiny, glue}

function(input, output, session){
  
  start <- callModule(module = welcome, id = "welcome")
  timer <- callModule(module = time, id = "timer", start = start)
  
  # Get enough cards to cover the board
  hex_png <- sample(list.files(path = "www/hex/", pattern = "png$"), boardSize)
  
  # Deal enough cards for each player
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
  boardCards <- reactiveValues(e1 = c(rep(0, boardSize-2), tempEdges[1:2,1]),
                                      e2 = c(rep(0, boardSize-2), tempEdges[1:2,2]),
                                      e3 = c(rep(0, boardSize-2), tempEdges[1:2,3]),
                                      e4 = c(rep(0, boardSize-2), tempEdges[1:2,4]),
                                      e5 = c(rep(0, boardSize-2), tempEdges[1:2,5]),
                                      e6 = c(rep(0, boardSize-2), tempEdges[1:2,6]),
                                      c1 = rep(0, boardSize),
                                      c2 = rep(0, boardSize),
                                      oc = c(rep(FALSE, boardSize - 2), 1, 1),
                                      av = c(TRUE, rep(FALSE, boardSize-1)),
                                      im = c("www/spaces/red.png",
                                             rep("www/spaces/beige2.png", 18),
                                             paste0("www/hex/", hex_png[1:2])),
                                      se = c(rep(FALSE, boardSize))
                                      )
  
  # Create a reactive object to hold the player's scores and captured chits
  scores <- reactiveValues(s1 = 2,
                                  s2 = 0,
                                  c1 = 0,
                                  c2 = 0)
  
  # Apply the boardCard info to each of the cards on the board (19 on table, 2 in player's hand)
  results_mods <- reactiveValues()
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
  
  output$score_ui <- renderText(
    paste(scores$s1, scores$s2, sep = " : ")
  )
  
  observeEvent(input$reload, {
    session$reload()
  }, ignoreInit = TRUE)
  
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
  

