# utils
# Uses: {base}

filterFound <- function(l){
  found <- unlist(lapply(l, `[[`, "found"), use.names = FALSE)
  l[!found]
}

allFound <- function(l){
  found <- unlist(lapply(l, `[[`, "found"), use.names = FALSE)
  all(found)
}

asNull <- function(x){
  if(is.na(x)){
    NULL
  } else {
    x    
  }
}

whichHex <- function(l, module){
  res <- lapply(module, function(x) l[[x]]$hex)
  unlist(res, use.names = FALSE)
}

whichShow <- function(l, indice = NULL){
  l <- filterFound(l)
  if(length(l) == 0) return(NULL)
  
  res <- unlist(lapply(l, `[[`, "show"))
  if(all(!res)) return(NULL)
  
  ts <- unlist(lapply(l[res], `[[`, "ts"), use.names = FALSE)
  res <- names(l)[res]
  res <- res[order(ts, decreasing = FALSE)]
  if(is.null(indice)){
    res
  } else {
    asNull(res[indice])
  }
}


dealCards <- function(){
  # Deal cards to each player
  tempEdges <- matrix(NA, ncol = 6)
  for (playerCard in 1:(playerCount*cardsPerPlayer)){
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