# Title : BareBones
#    By : bjhufstetler
#  Date : 2020-03-06

# Uses: {base, shiny, glue}

function(input, output, session){
  
  start <- shiny::callModule(module = welcome, id = "welcome")
  timer <- shiny::callModule(module = time, id = "timer", start = start)
  
  # Get enough cards to cover the board
  hex_png <- base::sample(base::list.files(path = "www/hex/", pattern = "png$"), boardSize)
  
  # Deal enough cards for each player
  tempEdges <- dealCards()
  
  # Set the player's cards as a reactive object
  playerCards <- shiny::reactiveValues(edge1 = tempEdges[,1],
                                       edge2 = tempEdges[,2],
                                       edge3 = tempEdges[,3],
                                       edge4 = tempEdges[,4],
                                       edge5 = tempEdges[,5],
                                       edge6 = tempEdges[,6],
                                       faceIMG = base::paste0("www/hex/", hex_png[1:playerCount*cardsPerPlayer]))
  
  # Create a reactive object to hold the information displayed on the cards
  boardCards <- shiny::reactiveValues(e1 = c(base::rep(0, boardSize-2), tempEdges[1:2,1]),
                                      e2 = c(base::rep(0, boardSize-2), tempEdges[1:2,2]),
                                      e3 = c(base::rep(0, boardSize-2), tempEdges[1:2,3]),
                                      e4 = c(base::rep(0, boardSize-2), tempEdges[1:2,4]),
                                      e5 = c(base::rep(0, boardSize-2), tempEdges[1:2,5]),
                                      e6 = c(base::rep(0, boardSize-2), tempEdges[1:2,6]),
                                      c1 = base::rep(0, boardSize),
                                      c2 = base::rep(0, boardSize),
                                      oc = c(base::rep(FALSE, boardSize - 2), 1, 1),
                                      av = base::rep(FALSE, boardSize),
                                      im = c("www/spaces/red.png",
                                             base::rep("www/spaces/beige2.png", 18),
                                             base::paste0("www/hex/", hex_png[1:2])),
                                      se = c(base::rep(FALSE, boardSize))
                                      )
  
  # Create a reactive object to hold the player's scores and captured chits
  scores <- shiny::reactiveValues(s1 = 0,
                                  s2 = 0,
                                  c1 = 0,
                                  c2 = 0)
  
  # Apply the boardCard info to each of the cards on the board (19 on table, 2 in player's hand)
  results_mods <- shiny::reactiveValues()
  refreshCard <- function(x){
    results_mods[[base::paste0("module", x)]] <- shiny::callModule(
      module = hex,
      id = base::paste0("module", x),
      id2 = x,
      bc = boardCards
    )
    #boardCards$se[x] <- isolate(results_mods$)
    print(isolate(unlist(lapply(results_mods, `[[`, base::paste0("module",x)), use.names = FALSE)))
  }
  
  base::lapply(
    X = base::seq_len(21),
    FUN = function(x){refreshCard(x)}
    )
  
  output$score_ui <- shiny::renderText(
    paste(scores$s1, scores$s2, sep = " : ")
  )
  
  
  shiny::observeEvent(input$reload, {
    session$reload()
  }, ignoreInit = TRUE)
  
  shiny::observeEvent(input$pressedKey, {
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
  
  #output$results = renderPrint({input$keystroke}) 
  
}

