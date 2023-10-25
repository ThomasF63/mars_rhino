# debugging
# options(shiny.reactlog=TRUE)

# Import libraries
box::use(
  shiny[bootstrapPage, navbarPage, tabPanel, div, moduleServer, NS, renderUI, tags, uiOutput, reactiveValues, reactive],
)
# Import modules
box::use(
  app/view/raw_outputs,
  #app/view/import_params,
  app/logic/file_io[get_params,get_crop_cals],
  app/logic/sim_run[run_sim],
)

#' @export
ui = function(id) {
  ns <- NS(id)
  bootstrapPage(

    navbarPage("mars demo", id="nav", collapsible = T, windowTitle = "placeholder title",

               tabPanel("Dashboard"
                        #, load dashboard components
                        ),

               tabPanel("Config",
                        # import disabled until upload sorted out
                        # import_params$ui(ns("sim_inputs"))
                        # display interactive param table
                        ),

               tabPanel("Raw Data",
                        raw_outputs$ui(ns("sim_outputs"))
                        ),

               )

  )
}

#' @export
server = function(id) {

  moduleServer(id, function(input, output, session) {

    # Inputs
    # Disable the below until file upload sorted out
    # params = reactiveValues(sim_params = import_params$server("sim_inputs")$sim_params,
    #                         crop_params = import_params$server("sim_inputs")$crop_params,
    #                         farm_layout = import_params$server("sim_inputs")$farm_layout,
    #                         cal_lookup = import_params$server("sim_inputs")$cal_lookup,
    #                         cal_path = import_params$server("sim_inputs")$cal_path,
    #                         crop_cals = import_params$server("sim_inputs")$crop_cals
    #                         )

# import the crop cals here

    params = get_params("app/data/parameters.xlsx")
    cals = get_crop_cals("app/data/Crop Calendars - Prod, Labor and Materials .xlsx",params$cal_lookup)

    # now make the above reactive

    # Reactive simulation
    sim = reactive({

      run_sim(params$sim_params,
              params$crop_params,
              params$farm_layout,
              #params$crop_cals
              cals)

      })

    # make reactive parameters edit-able

    # Raw Data tab
    raw_outputs$server("sim_outputs",sim_dat=sim)

  })

}
