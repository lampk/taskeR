#' Addin to insert date
#' @export
insert_date_addin <- function() {
  require(shiny)
  require(miniUI)

  ## ui
  ui <- miniPage(
    gadgetTitleBar("Insert Date",
                   right = miniTitleBarButton("done", "Done", primary = TRUE)),
    miniContentPanel(
      dateInput("date", "Date:", value = Sys.Date())
    )
  )

  ## server
  server <- function(input, output, session){
    ## stop app when click "Done"
    observeEvent(input$done, {
      rstudioapi::insertText(paste(as.character(input$date), "00:00"))
      stopApp()
    })
  }

  ## run
  runGadget(ui, server)
}
