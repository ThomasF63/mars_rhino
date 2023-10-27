# app/view/dashboard.R
# migrate a future version to bslib: https://appsilon.com/shiny-application-layouts/

# try this for select-able modules: https://stackoverflow.com/questions/62528413/how-to-call-different-modules-based-on-a-select-input-in-shiny
# i MIGHT be able to unify echarts filtering with this but module interaction unclear: https://echarts4r.john-coene.com/articles/connect


box::use(
  dplyr[...],
  shiny[h3, moduleServer, NS, fluidPage, fluidRow, reactive, renderText, textOutput],
  bslib[page_fluid,layout_columns,layout_column_wrap,value_box],
)
box::use(
  app/logic/kpis,
  app/view/dashboard_widgets/revenue_ts,
  app/view/dashboard_widgets/balance_ts,
  app/view/dashboard_widgets/yields_ts,
  app/view/dashboard_widgets/density_ts,
  app/view/dashboard_widgets/carbon_ts,
  app/view/dashboard_widgets/mats_drill
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fluid(

    # Later on handle KPI panels with a kpi module
    layout_columns(
      fill=F,
      value_box(title="Total Net Income",value=textOutput(ns("income"))),
      value_box(title="Total Carbon",value=textOutput(ns("carbon"))),
      value_box(title="Months to Profit",value=textOutput(ns("time_to_profit"))),
      value_box(title="Simulation Duration",value=textOutput(ns("sim_length")))
    ),

    layout_column_wrap(
      width=1/3,
      revenue_ts$ui(ns("revenue_plot")),
      balance_ts$ui(ns("balance_plot")),
      yields_ts$ui(ns("yield_plot")),
      density_ts$ui(ns("density_plot")),
      carbon_ts$ui(ns("carbon_plot")),
      mats_drill$ui(ns("mats_plot")),
    )
  )
}

#' @export
server <- function(id,sim_dat) {

  moduleServer(id, function(input, output, session) {

    # Apply filters to simoutput
    sim_filtered = reactive({ sim_dat() }) # later replace with module output

    # Later on handle KPI panels with a kpi module to hide this away
    output$income = renderText( kpis$total_income(sim_filtered()) )
    output$carbon = renderText( kpis$total_carbon(sim_filtered()) )
    output$time_to_profit = renderText( kpis$months_to_profit(sim_filtered()) )
    output$sim_length = renderText( kpis$total_months(sim_filtered()) )

    revenue_ts$server("revenue_plot",sim_filtered)
    balance_ts$server("balance_plot",sim_filtered)
    yields_ts$server("yield_plot",sim_filtered)
    density_ts$server("density_plot",sim_filtered)
    carbon_ts$server("carbon_plot",sim_filtered)
    mats_drill$server("mats_plot",sim_filtered)

  })

}
