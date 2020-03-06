# Title : BareBones
#    By : bjhufstetler
#  Date : 2020-03-06

# Uses: {base, shiny}

shiny::fluidPage(
  
  tags$head(
    tags$link(href = "styles.css", rel = "stylesheet", type = "text/css"),
    tags$script(src = "https://platform.twitter.com/widgets.js")
  ),
  
  tags$div(
    class = "title-app",
    tags$h1("Bare Bones Trading Card Game"),
    tags$h4("Get the most points")
  ),
  tags$br(),
  
  tags$div(
    style = "width: 650px; margin: auto;",
    timeUI("timer"),
    tags$br(),
    base::lapply(
      X = base::seq_len(19),
      FUN = function(x) {
        hexUI(id = base::paste0("module", x), id2 = x)
      }
    )
  )
)