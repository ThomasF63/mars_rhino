# app/view/dashboard_widgets/balance_ts.R

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
server <- function(id,processed_sim_dat) {
  # should be possible to colour based on value but couldn't get it to work on first try
  # https://echarts4r.john-coene.com/reference/e_visual_map


  moduleServer(id, function(input, output, session) {

    output$chart = echarts4r$renderEcharts4r(
      processed_sim_dat() %>%
        mutate(
          total_costs = rowSums(across(contains('labor_time'))) + rowSums(across(contains('mat_costs'))),
          net_income = revenue - total_costs
        ) %>%
        # Doesn't have easy ways to ignore NAs when connecting lines so just filter out 0s
        # Use dplyr group_by rather than echarts group_by, latter causes problems
        group_by(t) %>%
        summarise(balance = sum(net_income,na.rm=T)) %>%
        echarts4r$e_chart(x = t) %>%
        echarts4r$e_line(balance,symbol='none') %>%
        echarts4r$e_legend(show=F) %>%
        #echarts4r$e_x_axis(t) %>%
        echarts4r$e_title("Monthly Balance","USD") %>%
        echarts4r$e_tooltip()
    )

  })

}
