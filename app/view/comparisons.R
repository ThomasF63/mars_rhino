# app/view/comparisons.R

box::use(
  dplyr[...],
  shiny[h3, moduleServer, NS, fluidPage, fluidRow, reactive, renderText, textOutput],
  bslib[page_fluid,layout_columns,layout_column_wrap,value_box],
)
box::use(
  app/logic/kpis,
  app/view/import_params,
  app/view/dashboard_widgets/compare_balance
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fluid(

    import_params$ui(ns("importer")),

    # Later on handle KPI panels with a kpi module
    # KPIs

    layout_column_wrap(
      width=1/2,
      compare_balance$ui(ns("balance_plot")),
    )
  )
}

#' @export
server <- function(id,sim_dat,params) {

  moduleServer(id, function(input, output, session) {

    # reactive on file upload: set sim-2 stuff
    sim_2 = import_params$server("importer",params)

    compare_balance$server("balance_plot",sim_dat(),sim_2())

  })

}
