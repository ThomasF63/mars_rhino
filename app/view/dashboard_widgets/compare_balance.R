# app/view/dashboard_widgets/compare_balance.R

box::use(
  dplyr[...],
  shiny[h3, moduleServer, NS, tagList],
  echarts4r,
)
box::use(
  app/logic/kpis,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    echarts4r$echarts4rOutput(ns("chart"))
  )
}

#' @export
server <- function(id,sim_dat,sim2_dat) {

  moduleServer(id, function(input, output, session) {

    output$chart = echarts4r$renderEcharts4r(
      sim_dat %>%
        mutate(scenario = "Base Scenario") %>%
        bind_rows(
          sim2_dat %>% mutate(scenario = "Alternate Scenario")
          ) %>%
        mutate(
          total_costs = rowSums(across(contains('labor_time'))) + rowSums(across(contains('mat_costs'))),
          net_income = revenue - total_costs
        ) %>%
        # Use dplyr group_by rather than echarts group_by, latter causes problems
        group_by(t, scenario) %>%
        summarise(balance = sum(net_income,na.rm=T)) %>%
        group_by(scenario) %>%
        echarts4r$e_chart(x = t) %>%
        echarts4r$e_line(balance,symbol='none') %>%
        #echarts4r$e_x_axis(t) %>%
        echarts4r$e_title("Monthly Balance","USD") %>%
        echarts4r$e_tooltip()
    )

  })

}
