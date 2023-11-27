# app/view/import_params.R

box::use(
  dplyr[...],
  purrr[map],
  janitor[clean_names],
  shiny[moduleServer, NS, tagList, reactive, reactiveVal, fileInput, req],
  readxl[read_excel],
)
box::use(
  app/logic/file_io[get_params],
  app/logic/sim_run[run_sim],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    fileInput(ns("param_input"), "Choose parameter file for alternate scenario",
              multiple = F, accept = c(".xlsx")),
  )
}

#' @export
server <- function(id,params) {

  moduleServer(id, function(input, output, session) {

    sim2 = reactive({
      req(input$param_input)
      scenario2 = get_params(input$param_input$datapath)
      # reactive on file upload: set sim2 stuff
      run_sim(scenario2$sim_params,
              scenario2$crop_params,
              scenario2$farm_layout,
              params$crop_cals,
              scenario="Alternate")
    })

    return(sim2)

  })

}
