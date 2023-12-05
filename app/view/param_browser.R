# app/view/param_browser.R
# https://yihui.shinyapps.io/DT-edit/
# later check this out: https://github.com/jienagu/DT_editor_shiny_module
# Also check out other extensions such as column hiding and responsive width: https://rstudio.github.io/DT/extensions.html

box::use(
  DT[DTOutput,renderDT,editData],
  shiny[h3, br, moduleServer, NS, tagList, selectInput, observeEvent, downloadButton, downloadHandler, fileInput, reactiveValues, observe],
  writexl[write_xlsx],
  magrittr[...],
  shinycssloaders[withSpinner]
)
box::use(
  app/logic/file_io[get_params],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(

    h3("Double-click to edit table cells"),
    selectInput(ns("param_select"),"Choose parameter set:",
                c("Simulation Controls"="sim_params",
                  "Crop Globals"="crop_params",
                  "Farm Layout"="farm_layout")),
    DTOutput(ns("browser")) %>% withSpinner(type=7),

    downloadButton(ns("param_export"),"Export parameters"),

    br(),br(),

    fileInput(ns("param_import"), "Import an existing parameter file",
              multiple = F, accept = c(".xlsx"))
  )
}

#' @export
server <- function(id,params) {

  moduleServer(id, function(input, output, session) {
    # params enters via main as a reactiveValues


    observe({

      if(!is.null(input$param_import)) {
        new_params = get_params(input$param_import$datapath)
        params$sim_params = new_params$sim_params
        params$crop_params = new_params$crop_params
        params$farm_layout = new_params$farm_layout
        params$cal_lookup = new_params$cal_lookup
      }

    })

    # selectinput list as reactive instead (fetch names from params rather than hardcode)
    # discuss w tom whether or not (and how) to handle crop cals

    # rownames=T addresses an annoying quirk with different column numbering in DT vs df
    # Eventually add a Shiny button to export the parameter set
    output$browser = renderDT(params[[input$param_select]], selection='none', rownames=T, editable='cell', server = T,
                              # display all values, don't paginate or show size drop-down
                              options = list(dom = 'ft', pageLength = -1))

    observeEvent(input$browser_cell_edit, {
      params[[input$param_select]] = editData(params[[input$param_select]], input$browser_cell_edit, 'browser')
    })

    output$param_export = downloadHandler(
      filename = "custom_parameters.xlsx",
      contentType="application/xlsx",
      content = function(file) {
        write_xlsx(list(simulation_control = params$sim_params,
                        global_crop_parameters = params$crop_params,
                        farm_layout = params$farm_layout,
                        crop_sheet_lookup = params$cal_lookup),file)
      }
    )

    return(params)

  })

}
