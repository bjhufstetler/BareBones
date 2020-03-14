# Arrow image module

arrow_UI <- function(id, location) {
  ns <- NS(id)
  tagList(
    # Top Arrow
    absolutePanel(
      bottom = (relationships[location, 6] + 100), left = (relationships[location, 5] + 50),
      imageOutput(outputId = ns("a1"), inline = TRUE)
    ),
    # Top Right Number
    absolutePanel(
      bottom = (relationships[location, 6] + 75), left = (relationships[location, 5] + 100), 
      imageOutput(outputId = ns("a2"), inline = TRUE)
    ),
    # Bottom Right Number
    absolutePanel(
      bottom = (relationships[location, 6] + 10), left = (relationships[location, 5] + 100),
      imageOutput(outputId = ns("a3"), inline = TRUE)
    ),
    # Bottom Number
    absolutePanel(
      bottom = (relationships[location, 6] - 15), left = (relationships[location, 5] + 50),
      imageOutput(outputId = ns("a4"), inline = TRUE)
    ),
    # Bottom Left Number
    absolutePanel(
      bottom = (relationships[location, 6] + 10), left = (relationships[location, 5] + 0),
      imageOutput(outputId = ns("a5"), inline = TRUE)
    ),
    # Top Left Number
    absolutePanel(
      bottom = (relationships[location, 6] + 75), left = (relationships[location, 5] + 0),
      imageOutput(outputId = ns("a6"), inline = TRUE)
    )
  )
}

arrow <- function(input, output, session, location, sides){
  # Top number
  output$a1 <- renderImage({
    if(length(sides) > 0 && sides[1] != 0){
      list(
        src = paste0("www/arrows/arrows_01.png"),
        width = 40,
        height = 40,
        contentType = "image/png"
      )} else {
        list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Top Right Number
  output$a2 <- renderImage({
    if(length(sides) > 0 && sides[2] != 0){
      list(
        src = paste0("www/arrows/arrows_02.png"),
        width = 40,
        height = 40,
        contentType = "image/png"
      )} else {
        list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Bottom Right Number
  output$a3 <- renderImage({
    if(length(sides) > 0 && sides[3] != 0){
      list(
        src = paste0("www/arrows/arrows_03.png"),
        width = 40,
        height = 40,
        contentType = "image/png"
      )} else {
        list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Bottom Number
  output$a4 <- renderImage({
    if(length(sides) > 0 && sides[4] != 0){
      list(
        src = paste0("www/arrows/arrows_04.png"),
        width = 40,
        height = 40,
        contentType = "image/png"
      )} else {
        list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Bottom Left Number
  output$a5 <- renderImage({
    if(length(sides) > 0 && sides[5] != 0){
      list(
        src = paste0("www/arrows/arrows_05.png"),
        width = 40,
        height = 40,
        contentType = "image/png"
      )} else {
        list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
  
  # Top Left Number
  output$a6 <- renderImage({
    if(length(sides) > 0 && sides[6] != 0){
      list(
        src = paste0("www/arrows/arrows_06.png"),
        width = 40,
        height = 40,
        contentType = "image/png"
      )} else {
        list(src = "ww", width = 0, height = 0, contentType = "image/png")
      }
  }, deleteFile = FALSE)
}