# debugging
# options(shiny.reactlog=TRUE)

# Import libraries
box::use(
  shiny[bootstrapPage, navbarPage, tabPanel, div, moduleServer, NS, renderUI, tags, uiOutput, reactiveValues, reactive],
  waiter
)
# Import modules
box::use(
  app/view/dashboard,
  app/view/raw_outputs,
  #app/view/import_params,
  app/view/param_browser,
  app/view/comparisons,
  app/logic/file_io[get_params,get_crop_cals],
  app/logic/sim_run[run_sim],
)

#' @export
ui = function(id) {
  ns <- NS(id)
  bootstrapPage(

    waiter$use_waiter(),

    navbarPage("mars demo", id="nav", collapsible = T, windowTitle = "placeholder title",

               tabPanel("Dashboard",
                        dashboard$ui(ns("sim_dashboard"))
               ),

               tabPanel("Config",
                        # import_params$ui(ns("sim_inputs")) # import disabled until upload sorted out
                        param_browser$ui(ns("browser"))
               ),

#               tabPanel("Raw Data",
#                        raw_outputs$ui(ns("sim_outputs"))
#               ),

               tabPanel("Compare",
                        comparisons$ui(ns("compare"))
               ),

              )

  )
}

#' @export
server = function(id) {

  moduleServer(id, function(input, output, session) {

    # Import base data
    # We hold onto these to undo changes to values (not implemented yet)
    waiter$waiter_show()
    base_params = get_params("app/data/parameters.xlsx")
    base_cals = get_crop_cals("app/data/Crop Calendars - Prod, Labor and Materials .xlsx",base_params$cal_lookup)
    waiter$waiter_hide()

    # Now make them reactive
    params = reactiveValues(sim_params = base_params$sim_params,
                            crop_params = base_params$crop_params,
                            farm_layout = base_params$farm_layout,
                            crop_cals = base_cals)

    # Now make them edit-able
    params = param_browser$server("browser",params)

    # Reactive simulation
    sim = reactive({
      # simplify to run_sim(params) and extract each one within run_sim()
      run_sim(params$sim_params,
              params$crop_params,
              params$farm_layout,
              params$crop_cals)
      })

    # Dashboard tab
    dashboard$server("sim_dashboard",sim_dat=sim)

    # Raw Data tab
    # raw_outputs$server("sim_outputs",sim_dat=sim)

    # Comparisons tab
    comparisons$server("compare",sim_dat=sim,params=params)

  })

}
