# app/view/raw_outputs.R

box::use(
  DT[DTOutput,renderDT],
  shiny[h3, moduleServer, NS, tagList],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Raw outputs"),
    DTOutput(ns("raw_dat"))
  )
}

#' @export
server <- function(id,sim_dat) {

  moduleServer(id, function(input, output, session) {

    # Eventually replace the DT button with a Shiny one, easier to customise and doesn't require client-side rendering
    output$raw_dat = renderDT(sim_dat(), extensions = 'Buttons', rownames=T, server=F,
                              options = list(dom = "Blfrtip", buttons=c('colvis','csv')))

  })

}
