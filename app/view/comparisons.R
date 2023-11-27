# app/view/comparisons.R

box::use(
  dplyr[...],
  shiny[h3, moduleServer, NS, fluidPage, fluidRow, reactive, renderText, textOutput, br, p, req],
  bslib[page_fluid,layout_columns,layout_column_wrap,value_box,navset_tab,nav_panel],
)
box::use(
  app/logic/kpis,
  app/view/raw_outputs,
  app/view/import_params,
  #app/view/dashboard_widgets/compare_balance
  app/view/dashboard_widgets/balance_ts,
  app/view/dashboard_widgets/revenue_ts,
  app/view/dashboard_widgets/yields_ts
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  navset_tab(

    nav_panel(
      title="Main",

      import_params$ui(ns("importer")),

      br(),

      # Later on handle KPI panels with a kpi module
      # KPIs

      layout_columns(
        fill=F,
        value_box(title="Total Net Income",value=textOutput(ns("income_A")),p("Base Scenario")),
        value_box(title="",value=textOutput(ns("income_B")),p("Alt. Scenario")),

        value_box(title="Months to Profit",value=textOutput(ns("time_to_profit_A")),p("Base Scenario"),style='background-color: #228c22!important;'),
        value_box(title="",value=textOutput(ns("time_to_profit_B")),p("Alt. Scenario"),style='background-color: #228c22!important;'),

        value_box(title="Simulation Duration",value=textOutput(ns("sim_length_A")),p("Base Scenario"),style='background-color: #FDD071!important;'),
        value_box(title="",value=textOutput(ns("sim_length_B")),p("Alt. Scenario"),style='background-color: #FDD071!important;')

      ),

      br(),
      h3("Base Scenario"),

      layout_column_wrap(
        width=1/3,
        balance_ts$ui(ns("balance_A")),
        revenue_ts$ui(ns("revenue_A")),
        yields_ts$ui(ns("yield_A"))
      ),

      br(),
      h3("Alternate Scenario"),

      layout_column_wrap(
        width=1/3,
        balance_ts$ui(ns("balance_B")),
        revenue_ts$ui(ns("revenue_B")),
        yields_ts$ui(ns("yield_B")),
      )
    ),

    nav_panel(
      title="Data",
      raw_outputs$ui(ns("dats"))
    )

  )
}

#' @export
server <- function(id,sim_dat,params) {

  moduleServer(id, function(input, output, session) {

    # reactive on file upload: set sim-2 stuff
    sim_2 = import_params$server("importer",params)

    req(sim_2)

    # KPIs
    # Scenario A
    output$income_A = renderText( kpis$total_income(sim_dat()) )
    output$time_to_profit_A = renderText( kpis$months_to_profit(sim_dat()) )
    output$sim_length_A = renderText( kpis$total_months(sim_dat()) )

    # Scenario B
    output$income_B = renderText( kpis$total_income(sim_2()) )
    output$time_to_profit_B = renderText( kpis$months_to_profit(sim_2()) )
    output$sim_length_B = renderText( kpis$total_months(sim_2()) )

    # compare_balance$server("balance_plot",sim_dat(),sim_2())

    # Plots
    # Scenario A
    balance_ts$server("balance_A",sim_dat, th=kpis$income_th_usd(params$sim_params))
    revenue_ts$server("revenue_A",sim_dat)
    yields_ts$server("yield_A",sim_dat)

    # Scenario B
    balance_ts$server("balance_B",sim_2, grp='sim2', th=kpis$income_th_usd(params$sim_params))
    revenue_ts$server("revenue_B",sim_2, grp='sim2')
    yields_ts$server("yield_B",sim_2, grp='sim2')


    combined_sims = reactive({
      bind_rows(sim_dat(),sim_2())
    })

    raw_outputs$server("dats", sim_dat=combined_sims)

  })

}
