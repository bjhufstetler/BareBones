# Title : BareBones
#    By : bjhufstetler
#  Date : 2020-03-06

# Uses: {base, shiny}

fluidPage(
  # Import the .css information
  tags$head(
    tags$link(href = "styles.css", rel = "stylesheet", type = "text/css"),
    tags$script(src = "https://platform.twitter.com/widgets.js")
  ),
  
  # Title
  absolutePanel(bottom = 600, left = 480, width = 300, 
                tags$h1("Bare Bones TCG")
  ),
  
  # New Game Button
  absolutePanel(bottom = 550, left = 50, width = 200, 
  actionButton(
    inputId = "reload",
    label = "New Game",
    style = "width: 160px; margin: auto; background: #a76d60"
  )),
  
  # Score Module
  absolutePanel(bottom = 450, left = 50, width = 200, 
  tags$div(
    style = "width: 160px; margin: center;",
    scoreUI("score")
  )),
  
  # White Player's captured chits
  absolutePanel(bottom = 350, left = 50, width = 200, 
    tags$div(
      style = "width: 160px; margin: center;",
      tags$h4("White Player's Captives"),
      lapply(
        X = seq_len(20),
        FUN = function(x) {
          chit_UI_player(id = paste0("cplayer", x))
        }
      )
    )
  ),
  
  # Black Player's captured chits
  absolutePanel(bottom = 250, left = 50, width = 200, 
    tags$div(
      style = "width: 160px; margin: right;",
      tags$h4("Black Player's Captives"),
      lapply(
        X = seq_len(20),
        FUN = function(x) {
          chit_UI_ai(id = paste0("cai", x))
        }
      )
    )),
  
  # Notes
  absolutePanel(bottom = 50, left = 50, width = 400, 
    "*Rotate cards with keyboard arrows", tags$br(),
    "*No attacking on first two rounds", tags$br(),
    "*First player gets two bonus points and wins ties"
  ),
  
  # Cards
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
  ),
  
  # Keyboard input
  tags$script('
    pressedKeyCount = 0;
    $(document).on("keydown", function (e) {
       Shiny.onInputChange("pressedKey", pressedKeyCount++);
       Shiny.onInputChange("pressedKeyID", e.which);
    });'
  )
)