# Title : BareBones
#    By : bjhufstetler
#  Date : 2020-03-06

# Uses: {base, shiny}

fluidPage(
  
  tags$head(
    tags$link(href = "styles.css", rel = "stylesheet", type = "text/css"),
    tags$script(src = "https://platform.twitter.com/widgets.js")
  ),
  
  tags$div(
    class = "title-app",
    tags$h1("Bare Bones Trading Card Game"),
    tags$h4("Rotate cards with left and right arrow buttons")
  ),
  tags$br(),
  
  tags$div(
    style = "width: 200px; margin: center;",
    timeUI("timer")
    ),
  tags$br(),
  
  tags$div(
    style = "width: 200px; margin: center;",
    scoreUI("score"),
  ),
  tags$br(),
  
  # Put the cards on the board
  tags$div(
    style = "width: 650px; margin: auto;",
    lapply(
      X = seq_len(21),
      FUN = function(x) {
        hexUI(id = paste0("module", x), id2 = x)
      }
    )
  ),
  
  # TODO: add captured chits for each player. Why is this not showing up?
  tags$div(
    style = "width: 120px; margin: left;",
    tags$h4("P1's Captives"),
    tags$br(),
    lapply(
      X = seq_len(20),
      FUN = function(x) {
        chit_UI_player(id = paste0("cplayer", x))
      }
    )
  ),
  
  tags$div(
    style = "width: 120px; margin: right;",
    tags$h4("P2's Captives"),
    tags$br(),
    lapply(
      X = seq_len(20),
      FUN = function(x) {
        chit_UI_ai(id = paste0("cai", x))
      }
    )
  ),
  
  # Get keyboard input
  tags$script('
    pressedKeyCount = 0;
    $(document).on("keydown", function (e) {
       Shiny.onInputChange("pressedKey", pressedKeyCount++);
       Shiny.onInputChange("pressedKeyID", e.which);
    });'
  )
)