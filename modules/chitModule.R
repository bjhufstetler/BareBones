chit_UI_player <- function(id) {
  ns <- NS(id)
  tagList(
    imageOutput(
      outputId = ns("chit_player"),
      width = 20, 
      height = 20, 
      inline = TRUE
    )
  )
}

chit_player <- function(input, output, session) {
  output$chit_player <- renderImage({
      list(
        src = "www/chits/chits_06.png", 
        width = 20, 
        height = 20, 
        contentType = "image/png"
      )
  }, deleteFile = FALSE)
}

##################

chit_UI_ai <- function(id) {
  ns <- NS(id)
  tagList(
    imageOutput(
      outputId = ns("chit_ai"),
      width = 20, 
      height = 20, 
      inline = TRUE
    )
  )
}

chit_ai<- function(input, output, session) {
  output$chit_ai <- renderImage({
    list(
      src = "www/chits/chits_05.png", 
      width = 20, 
      height = 20, 
      contentType = "image/png"
    )
  }, deleteFile = FALSE)
}