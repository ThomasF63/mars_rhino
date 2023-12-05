# debugging
# options(shiny.reactlog=TRUE)

# Import libraries
box::use(
  shiny[tagList,h4,bootstrapPage, navbarPage, tabPanel, div, moduleServer, NS, renderUI, tags, uiOutput, reactiveValues, reactive],
  bslib[page_fluid,page_navbar,nav_panel,navset_tab],
  waiter[autoWaiter,useWaiter,waiterShowOnLoad,spin_ellipsis,waiter_hide,waiterPreloader],
  shinydisconnect,
)
# Import modules
box::use(
  app/view/dashboard,
  app/view/raw_outputs,
  app/view/param_browser,
  app/view/comparisons,
  app/logic/file_io[get_params,get_crop_cals],
  app/logic/sim_run[run_sim],
)

loading_screen = tagList(
  spin_ellipsis(),
  # Other spinners: https://shiny.john-coene.com/waiter/
  h4("Importing crop calendars...")
)

#' @export
ui = function(id) {
  ns = NS(id)

  page_fluid(

    useWaiter(),
    waiterPreloader(html=loading_screen, color="#0000A5"),
    shinydisconnect$disconnectMessage(),

    page_navbar(
      title="Prototype Farm Simulator",
      nav_panel("Dashboard",
                dashboard$ui(ns("sim_dashboard"))
      ),

      nav_panel("Config",
                param_browser$ui(ns("browser"))
      ),

      nav_panel("Compare",
                comparisons$ui(ns("compare"))
      )

    )


  )
}

#' @export
server = function(id) {

  moduleServer(id, function(input, output, session) {

    # Import base data
    # We hold onto these to undo changes to values (not implemented yet)
    base_params = get_params("app/data/parameters.xlsx")
    base_cals = get_crop_cals("app/data/Crop Calendars - Prod, Labor and Materials .xlsx",base_params$cal_lookup)

    # Now make them reactive
    params = reactiveValues(sim_params = base_params$sim_params,
                            crop_params = base_params$crop_params,
                            farm_layout = base_params$farm_layout,
                            cal_lookup = base_params$cal_lookup,
                            crop_cals = base_cals)

    # Now make them edit-able
    params = param_browser$server("browser",params)

    # Reactive simulation
    sim = reactive({
      # simplify to run_sim(params) and extract each one within run_sim()
      run_sim(params$sim_params,
              params$crop_params,
              params$farm_layout,
              params$crop_cals,
              scenario="Base")
      })

    # Dashboard tab
    dashboard$server("sim_dashboard",sim_dat=sim,params=params)

    # Comparisons tab
    comparisons$server("compare",sim_dat=sim,params=params)

  })

}
