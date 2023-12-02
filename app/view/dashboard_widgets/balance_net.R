# app/view/dashboard_widgets/balance_net.R

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
server <- function(id,processed_sim_dat,grp="grp",show_timeline=F) {

  moduleServer(id, function(input, output, session) {

    output$chart = echarts4r$renderEcharts4r(
      processed_sim_dat() %>%
        mutate(
          total_costs = rowSums(across(contains('labor_time'))) + rowSums(across(contains('mat_costs'))),
          net_income = revenue - total_costs
        ) %>%
        # Doesn't have easy ways to ignore NAs when connecting lines so just filter out 0s
        # Use dplyr group_by rather than echarts group_by, latter causes problems
        group_by(timestep) %>%
        summarise(balance = sum(net_income,na.rm=T)) %>%
        mutate(cumulative_balance = cumsum(balance)) %>%
        # Doesn't have easy ways to ignore NAs when connecting lines so just filter out 0s
        # Use dplyr group_by rather than echarts group_by, latter causes problems
        echarts4r$e_charts(x = timestep) %>%
        echarts4r$e_bar(cumulative_balance,symbol='none') %>%
        echarts4r$e_visual_map(
          type="piecewise",
          show=FALSE,
          pieces=list(
            list(gt = 0, color="green"),
            list(lte = 0, color="red")
          )
        ) %>%
        echarts4r$e_legend(show=F) %>%
        echarts4r$e_x_axis(min=0,max=max(processed_sim_dat()$timestep)) %>%
        echarts4r$e_title("Cumulative Balance","USD, red below $0") %>%
        echarts4r$e_tooltip(trigger="axis") %>%
        echarts4r$e_datazoom(show=show_timeline) %>%
        echarts4r$e_group(grp) %>%
        echarts4r$e_connect_group(grp)
    )

  })

}
