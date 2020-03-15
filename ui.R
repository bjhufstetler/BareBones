# Title : BareBones
#    By : bjhufstetler
#  Date : 2020-03-06

# Uses: {base, shiny}

fluidPage(
  
  tags$head(
    tags$link(href = "styles.css", rel = "stylesheet", type = "text/css"),
    tags$script(src = "https://platform.twitter.com/widgets.js")
  ),
  
  titlePanel("Bare Bones TCG"
  ),
  
  sidebarLayout(
    sidebarPanel(width = 3, style = "background: #e0d0c1;",
  actionButton(
    inputId = "reload",
    label = "New Game",
    style = "width: 160px; margin: auto; background: #a76d60"
  ),
  
  tags$div(
    style = "width: 160px; margin: center;",
    scoreUI("score")
  ),
  
  tags$br(),
  
  tags$div(
    style = "width: 160px; margin: center;",
    tags$h4("White Player's Captives"),
    tags$br(),
    lapply(
      X = seq_len(20),
      FUN = function(x) {
        chit_UI_player(id = paste0("cplayer", x))
      }
    )
  ),
  tags$br(),tags$br(),
  tags$div(
    style = "width: 160px; margin: right;",
    tags$h4("Black Player's Captives"),
    tags$br(),
    lapply(
      X = seq_len(20),
      FUN = function(x) {
        chit_UI_ai(id = paste0("cai", x))
      }
    ),
    tags$br(),tags$br(),
    tags$h4("*Rotate cards with keyboard arrows"),
    tags$h4("*No attacking on first two rounds"),
    tags$h4("*First player gets two bonus points and wins ties"),
  )),
  
  mainPanel(width = 9,
  # Put the cards on the board
  tags$div(
    style = "width: 650px; margin: auto;",
    lapply(
      X = seq_len(21),
      FUN = function(x) {
        hexUI(id = paste0("module", x), id2 = x)
      }
    ),
    lapply(
      X = seq_len(21),
      FUN = function(x) {
        arrow_UI(id = paste0("arrow", x), location = x)
      }
    )
  ))),
  

  
  # Get keyboard input
  tags$script('
    pressedKeyCount = 0;
    $(document).on("keydown", function (e) {
       Shiny.onInputChange("pressedKey", pressedKeyCount++);
       Shiny.onInputChange("pressedKeyID", e.which);
    });'
  )
)