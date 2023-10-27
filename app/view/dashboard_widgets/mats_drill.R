# app/view/dashboard_widgets/mats_drill.R
# timeline currently appears to be a dead-end due to lack of support for nesting

box::use(
  dplyr[...],
  tidyr[pivot_longer],
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
        # Doesn't have easy ways to ignore NAs when connecting lines so just filter out 0s
        # Use dplyr group_by rather than echarts group_by, latter causes problems
        mutate(
          mat_costs_total = rowSums(across(contains('mat_costs')))
        ) %>%
        pivot_longer(contains('mat_costs'),names_to="activity",values_to="mat_cost") %>%
        group_by(crop,planting) %>%
        echarts4r$e_chart(x = t, timeline=T) %>%
        echarts4r$e_line(mat_cost) %>%
        echarts4r$e_add_nested("itemStyle",activity) %>%
        #echarts4r$e_x_axis(t) %>%
        echarts4r$e_tooltip()
    )

  })

}
