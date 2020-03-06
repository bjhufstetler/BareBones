# Title : BareBones
#    By : bjhufstetler
#  Date : 2020-03-06

# Uses: {base, shiny, glue}

function(input, output, session){
  
  start <- shiny::callModule(module = welcome, id = "welcome")
  timer <- shiny::callModule(module = time, id = "timer", start = start)
  
  hex_png <- base::sample(base::list.files(path = "www/hex/", pattern = "png$"), n_hex)
  hex_png <- base::sample(base::rep(hex_png, 2))
  
  results_mods <- shiny::reactiveValues()
  results_mods_parse <- shiny::reactiveValues(all = NULL, 
                                              show1 = NULL, 
                                              show2 = NULL, 
                                              show3 = NULL)
  reset <- shiny::reactiveValues(x = NULL)
  block <- shiny::reactiveValues(x = NULL)
  
  base::lapply(
    X = base::seq_len(n_hex * 2),
    FUN = function(x) {
      results_mods[[base::paste0("module", x)]] <- shiny::callModule(
        module = hex,
        id = base::paste0("module", x),
        hexID = hex_png[x],
        reset = reset,
        block = block
      )
    }
  )
  
  shiny::observe({
    res_mod <- base::lapply(
      X = shiny::reactiveValuesToList(results_mods),
      FUN = shiny::reactiveValuesToList
    )
    results_mods_parse$all <- res_mod
    results_mods_parse$show1 <- whichShow(res_mod, 1)
    results_mods_parse$show2 <- whichShow(res_mod, 2)
    results_mods_parse$show3 <- whichShow(res_mod, 3)
  })
  
  shiny::observeEvent(results_mods_parse$show2, {
    hex1 <- whichHex(results_mods_parse$all, results_mods_parse$show1)
    hex2 <- whichHex(results_mods_parse$all, results_mods_parse$show2)
    if(base::identical(hex1, hex2)){
      block$x <- hex1
      shiny::showNotification(
        ui = tags$div(
          style = "font-size: 160%; font-weight: bold;",
          base::sample(
            x = c("Well done!", "Bravo!", "Great!", "Good job!",
                  "Amazing!", "Nice!", "Hooray!"),
            size = 1
          )
        ), type = "message"
      )
    }
  })
  
  shiny::observeEvent(results_mods_parse$show3, {
    reset$x <- whichHex(
      results_mods_parse$all,
      c(results_mods_parse$show1, results_mods_parse$show2)
    )
    results_mods_parse$show1 <- NULL
    results_mods_parse$show2 <- NULL
    results_mods_parse$show1 <- results_mods_parse$show3
    results_mods_parse$show3 <- NULL
  })
  
  shiny::observe({
    allfound <- allFound(results_mods_parse$all)
    if(allfound){
      shiny::showModal(shiny::modalDialog(
        tags$div(
          style = "text-align: center;",
          tags$h2(
            tags$span(shiny::icon("trophy"), style = "color: #F7E32F;"),
            "Well done !",
            tags$span(shiny::icon("trophy"), style = "color: #F7E32F;")
          ),
          tags$h4("You've found all matching hex in"),
          tags$h1(shiny::isolate(timer()), "seconds!"),
          tags$br(), tags$br(),
          tags$a(
            href = glue::glue(shareurl, time = shiny::isolate(timer())),
            shiny::icon("twitter"), "Tweet your score !",
            class = "btn btn-info btn-lg"
          ),
          tags$br(), tags$br(),
          shiny::actionButton(
            inputId = "reload",
            label = "Play again !",
            sytle = "width: 100%;"
          )
        ),
        footer = NULL,
        easyClose = FALSE
      ))
    }
  })
  
  shiny::observeEvent(input$reload, {
    session$reload()
  }, ignoreInit = TRUE)
  
  output$test_res_show <- shiny::renderPrint({
    c(results_mods_parse$show1, 
      results_mods_parse$show2, 
      results_mods_parse$show3)
  })
  
  output$test_res <- shiny::renderPrint({
    results_mods_parse$all
  })
}