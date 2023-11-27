# app/view/dashboard.R
# migrate a future version to bslib: https://appsilon.com/shiny-application-layouts/

# try this for select-able modules: https://stackoverflow.com/questions/62528413/how-to-call-different-modules-based-on-a-select-input-in-shiny
# i MIGHT be able to unify echarts filtering with this but module interaction unclear: https://echarts4r.john-coene.com/articles/connect


box::use(
  dplyr[...],
  shiny[h3, br, moduleServer, NS, fluidPage, fluidRow, reactive, renderText, textOutput],
  bslib[page_fluid,layout_columns,layout_column_wrap,value_box,navset_tab,nav_panel],
)
box::use(
  app/logic/kpis,
  app/view/raw_outputs,
  app/view/dashboard_widgets/balance_ts,
  app/view/dashboard_widgets/balance_net,
  app/view/dashboard_widgets/revenue_ts,
  app/view/dashboard_widgets/yields_ts,
  app/view/dashboard_widgets/density_ts,
  app/view/dashboard_widgets/carbon_ts,
  app/view/dashboard_widgets/costs_drill,
  app/view/dashboard_widgets/farm_layout
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  navset_tab(
  #page_fluid(
    nav_panel(
      title="Main",

      # Later on handle KPI panels with a kpi module
      layout_columns(
        fill=F,
        value_box(title="Total Net Income",value=textOutput(ns("income"))),
        value_box(title="Total Carbon",value=textOutput(ns("carbon"))),
        value_box(title="Months to Profit",value=textOutput(ns("time_to_profit"))),
        value_box(title="Average FTEs",value=textOutput(ns("ftes"))),
        value_box(title="Simulation Duration",value=textOutput(ns("sim_length")))
      ),

      br(),

      layout_column_wrap(
        width=1/3,
        farm_layout$ui(ns("layout")),
        balance_ts$ui(ns("balance_plot")),
        balance_net$ui(ns("net_balance_plot")),

        revenue_ts$ui(ns("revenue_plot")),
        costs_drill$ui(ns("mats_plot")),
        costs_drill$ui(ns("labor_plot")),

        density_ts$ui(ns("density_plot")),
        yields_ts$ui(ns("yield_plot")),
        carbon_ts$ui(ns("carbon_plot")),



      )
    ),

    nav_panel(
      title="Data",
      raw_outputs$ui(ns("dats"))
    )
  )
}

#' @export
server <- function(id,sim_dat,sim_params) {

  moduleServer(id, function(input, output, session) {

    # Apply filters to simoutput
    sim_filtered = reactive({ sim_dat() }) # later replace with module output

    # Later on handle KPI panels with a kpi module to hide this away
    output$income = renderText( kpis$total_income(sim_filtered()) )
    output$carbon = renderText( kpis$total_carbon(sim_filtered()) )
    output$time_to_profit = renderText( kpis$months_to_profit(sim_filtered()) )
    output$ftes = renderText( kpis$av_ftes(sim_filtered(), sim_params) )
    output$sim_length = renderText( kpis$total_months(sim_filtered()) )

    farm_layout$server("layout",sim_filtered)
    balance_ts$server("balance_plot",sim_filtered, th=kpis$income_th_usd(sim_params))
    balance_net$server("net_balance_plot",sim_filtered)

    revenue_ts$server("revenue_plot",sim_filtered)
    costs_drill$server("mats_plot",sim_filtered,cost_type="materials")
    costs_drill$server("labor_plot",sim_filtered,cost_type="labor")

    density_ts$server("density_plot",sim_filtered)
    yields_ts$server("yield_plot",sim_filtered)
    carbon_ts$server("carbon_plot",sim_filtered)

    raw_outputs$server("dats",sim_dat=sim_dat)

  })

}
