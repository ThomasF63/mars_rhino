# app/view/dashboard_widgets/carbon_ts.R

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

  moduleServer(id, function(input, output, session) {

    output$chart = echarts4r$renderEcharts4r(
      processed_sim_dat() %>%
        group_by(crop,planting) %>%
        mutate(cum_carbon = cumsum(total_c)) %>%
        # Doesn't have easy ways to ignore NAs when connecting lines so just filter out 0s
        # Use dplyr group_by rather than echarts group_by, latter causes problems
        echarts4r$e_chart(x = t) %>%
        echarts4r$e_line(cum_carbon, symbol='none') %>%
        #echarts4r$e_x_axis(t) %>%
        echarts4r$e_title("Total Carbon Storage","t/ha, cumulative") %>%
        echarts4r$e_tooltip() %>%
        echarts4r$e_group("grp") %>%
        echarts4r$e_connect_group("grp")
    )

  })

}
