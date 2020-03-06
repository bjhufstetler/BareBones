# utils

filterFound <- function(l){
  found <- base::unlist(base::lapply(l, `[[`, "found"), use.names = FALSE)
  l[!found]
}

allFound <- function(l){
  found <- base::unlist(base::lapply(l, `[[`, "found"), use.names = FALSE)
  base::all(found)
}

asNull <- function(x){
  base::ifelse(base::is.na(x), NULL, x)
}

whichHex <- function(l, module){
  res <- base::lapply(module, function(x) l[[x]]$hex)
  base::unlist(res, use.names = FALSE)
}

whichShow <- function(l, indice = NULL){
  l <- filterFound(l)
  if(base::length(l) == 0) return(NULL)
  
  res <- base::unlist(base::lapply(l, `[[`, "show"))
  if(base::all(!res)) return(NULL)
  
  ts <- base::unlist(base::lapply(l[res], `[[`, "ts"), use.names = FALSE)
  res <- base::names(l)[res]
  res <- res[base::order(ts, decreasing = FALSE)]
  ifelse(base::is.null(indice), res, asNull(res[indice]))
}