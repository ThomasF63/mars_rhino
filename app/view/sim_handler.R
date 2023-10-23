# app/view/sim_handler.R

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
server <- function(id,params) {

  moduleServer(id, function(input, output, session) {

    # out = reactive({
    #   run_sim(params$sim_params,params$crop_params,params$farm_layout,params$sheet_lookup,params$cal_path)
    # })

    out = run_sim(params$sim_params,params$crop_params,params$farm_layout,params$sheet_lookup,params$cal_path)

    output$table = reactable$renderReactable(
      out %>%
        reactable$reactable()
    )

    # Using return lets us grab the simulation outputs to feed into other modules too
    return(out)

  })

}
