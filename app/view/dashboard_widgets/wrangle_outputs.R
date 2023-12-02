# app/view/dashboard_widgets/wrangle_outputs.R

box::use(
  dplyr[...],
  shiny[moduleServer, NS, tagList, reactive, reactiveVal, checkboxInput, req],
)
box::use(
  app/logic/file_io[get_params],
  app/logic/sim_run[run_sim],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    checkboxInput(ns("aggregate_to_yr"), "Aggregate to year", value=F),
  )
}

#' @export
server <- function(id,sim_dat) {

  moduleServer(id, function(input, output, session) {

    sim_wrangled = reactive({

      if(input$aggregate_to_yr) {
        data = sim_dat %>%
          group_by(scenario,crop,planting,crop_planting,year) %>%
          summarise(across(is.numeric, sum, na.rm=T)) %>%
          mutate(timestep=year)
      } else {
        data = sim_dat %>%
          mutate(timestep=t)
      }

    })

    return(sim_wrangled)

  })

}
