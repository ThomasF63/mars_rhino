# app/view/raw_outputs.R

box::use(
  dplyr[...],
  reactable,
  shiny[h3, moduleServer, NS, tagList, reactive],
)
box::use(
  app/logic/sim_run[run_sim],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Raw outputs"),
    reactable$reactableOutput(ns("table"))
  )
}

#' @export
server <- function(id,sim_dat) {

  moduleServer(id, function(input, output, session) {

    output$table = reactable$renderReactable(
      sim_dat() %>%
        reactable$reactable()
    )

  })

}
