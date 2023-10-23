# debugging
# options(shiny.reactlog=TRUE)

# Import libraries
box::use(
  shiny[bootstrapPage, navbarPage, tabPanel, div, moduleServer, NS, renderUI, tags, uiOutput],
  readr[read_csv],
)
# Import modules
box::use(
  app/view/sim_handler,
)

#' @export
ui = function(id) {
  ns <- NS(id)
  bootstrapPage(

    navbarPage("mars demo", id="nav", collapsible = T, windowTitle = "placeholder title",

               tabPanel("Dashboard"
                        #, load dashboard components
                        ),

               tabPanel("Config"
                        # display interactive param table
                        ),

               tabPanel("Raw Data",
                        sim_handler$ui(ns("do_sim"))
                        ),

               )

  )
}

#' @export
server = function(id) {

  moduleServer(id, function(input, output, session) {

    # Inputs (v1: static, reactive in future)
    params = list(sim_params = read_csv("app/data/parameters.csv"),
                  crop_params = read_csv("app/data/crop_parameters.csv"),
                  farm_layout = read_csv("app/data/farm_layout.csv"),
                  sheet_lookup = read_csv("app/data/sheet_lookup.csv"),

                  # move extraction here - it's very slow so I only want to do it once (startup)
                  cal_path = "app/data/Crop Calendars - Prod, Labor and Materials .xlsx"
    )

    # Raw Data tab
    sim = sim_handler$server("do_sim",params=params)

  })

}
