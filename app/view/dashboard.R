# app/view/dashboard.R
# migrate a future version to bslib: https://appsilon.com/shiny-application-layouts/

# try this for select-able modules: https://stackoverflow.com/questions/62528413/how-to-call-different-modules-based-on-a-select-input-in-shiny
# i MIGHT be able to unify echarts filtering with this but module interaction unclear: https://echarts4r.john-coene.com/articles/connect


box::use(
  dplyr[...],
  shiny[h3, br, moduleServer, NS, fluidPage, fluidRow, reactive, renderText, textOutput, tagList],
  bslib[sidebar,page_sidebar,page_navbar,page_fluid,layout_columns,layout_column_wrap,value_box,navset_tab,nav_panel],
)
box::use(
  app/logic/kpis,
  app/view/raw_outputs,
  app/view/dashboard_widgets/wrangle_outputs,
  app/view/dashboard_widgets/balance_ts,
  app/view/dashboard_widgets/balance_net,
  app/view/dashboard_widgets/revenue_ts,
  app/view/dashboard_widgets/yields_ts,
  app/view/dashboard_widgets/density_ts,
  app/view/dashboard_widgets/carbon_ts,
  app/view/dashboard_widgets/costs_drill,
  app/view/dashboard_widgets/costs_agg,
  app/view/dashboard_widgets/ftes_ts,
  app/view/dashboard_widgets/farm_layout
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_sidebar(

    sidebar=sidebar(
      wrangle_outputs$ui(ns("wrangling"))
      ),

    navset_tab(

    nav_panel(
      title="Summary",

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
        width=1,
        balance_net$ui(ns("net_balance_plot_summary"))
      ),
      layout_column_wrap(
        width=1/2,
        farm_layout$ui(ns("layout_summary")),
        carbon_ts$ui(ns("carbon_plot_summary"))

      )
    ),


    nav_panel(
      title="Financials",

      br(),

      layout_column_wrap(
        width=1/2,
        balance_net$ui(ns("net_balance_plot")),
        balance_ts$ui(ns("balance_plot")),

        revenue_ts$ui(ns("revenue_plot")),
        ftes_ts$ui(ns("fte_plot")),

        costs_agg$ui(ns("mat_aggregate")),
        costs_agg$ui(ns("labor_aggregate")),
      )
    ),


    nav_panel(
      title="Detailed Costs",

      br(),
      layout_column_wrap(width=1,costs_drill$ui(ns("mats_plot"))),
      br(),
      layout_column_wrap(width=1,costs_drill$ui(ns("labor_plot")))
    ),


    nav_panel(
      title="Agricultural",

      br(),

      layout_column_wrap(
        width=1/2,
        yields_ts$ui(ns("yield_plot")),
        farm_layout$ui(ns("layout")),

        density_ts$ui(ns("density_plot")),
        carbon_ts$ui(ns("carbon_plot"))

      )
    ),

    nav_panel(
      title="Data",
      raw_outputs$ui(ns("dats"))
    )
  )
  )
}

#' @export
server <- function(id,sim_dat,params) {

  moduleServer(id, function(input, output, session) {

    # Apply filters to simoutput
    sim_filtered = wrangle_outputs$server("wrangling",sim_dat()) # later replace with module output

    # Later on handle KPI panels with a kpi module to hide this away
    output$income = renderText( kpis$total_income(sim_dat()) )
    output$carbon = renderText( kpis$total_carbon(sim_dat()) )
    output$time_to_profit = renderText( kpis$months_to_profit(sim_dat()) )
    output$ftes = renderText( kpis$av_ftes(sim_dat(), params$sim_params) )
    output$sim_length = renderText( kpis$total_months(sim_dat()) )

    # summary tab
    balance_net$server("net_balance_plot_summary",sim_filtered,show_timeline=T)
    farm_layout$server("layout_summary",sim_dat)
    carbon_ts$server("carbon_plot_summary",sim_filtered)

    # financials tab
    balance_net$server("net_balance_plot",sim_filtered,show_timeline=T)
    balance_ts$server("balance_plot",sim_filtered, th=kpis$income_th_usd(params$sim_params))

    revenue_ts$server("revenue_plot",sim_filtered)
    ftes_ts$server("fte_plot",sim_filtered, params$sim_params)

    costs_agg$server("mat_aggregate",sim_filtered,cost_type="materials")
    costs_agg$server("labor_aggregate",sim_filtered,cost_type="labor")

    # costs tab
    costs_drill$server("mats_plot",sim_filtered,cost_type="materials")
    costs_drill$server("labor_plot",sim_filtered,cost_type="labor")

    # agricultural tab
    yields_ts$server("yield_plot",sim_filtered,show_timeline=T)
    farm_layout$server("layout",sim_dat)
    density_ts$server("density_plot",sim_filtered)
    carbon_ts$server("carbon_plot",sim_filtered)









    # data tab
    # note we input sim_dat directly, not the filtered data
    raw_outputs$server("dats",sim_dat=sim_dat)

  })

}
